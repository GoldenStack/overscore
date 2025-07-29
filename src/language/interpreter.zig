const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Range = tokenizer.Range;

const Ir = @import("Ir.zig");
const Index = Ir.Index;
const IndexOf = Ir.IndexOf;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;
const LazyDecl = Ir.ir.LazyDecl;

// Type inference

/// Determines the type of the given value, caching it in the value and
/// returning the index of the type. This also functions as a type check.
pub fn typeOf(ir: *Ir, index: Index) Err!Index {
    // Check if the index already has a type
    if (ir.at(.type, index).*) |@"type"| return @"type";

    // Make sure we're not already evaluating it (likely an infinite loop)
    try markEval(ir, index);
    defer ir.at(.evaluating, index).* = false;

    const value_type = try switch (ir.at(.expr, index).*) {
        .word => ir.push(.word_type, ir.at(.range, index).*),

        .word_type, .decl, .product, .sum, .pointer_type, .type => ir.push(.type, ir.at(.range, index).*),

        .pointer => |ptr| ir.push(.{ .pointer_type = try typeOf(ir, try defValueCoerce(ir, ptr)) }, ir.at(.range, index).*),

        .def => (try typeOfDef(ir, .{ .index = index })).index,
        .container => typeOfContainer(ir, index),
        .dereference => typeOfDereference(ir, index),
        .member_access => typeOfMemberAccess(ir, index),

        .coerce => |coerce| coerce.type,
    };

    ir.at(.type, index).* = value_type;

    return value_type;
}

/// Returns the type of a def. Assumes the given index is a def.
fn typeOfDef(ir: *Ir, index: IndexOf(.def)) Err!IndexOf(.decl) {
    const name = ir.atOf(.def, index).name;

    const @"type" = try typeOf(ir, try defValueCoerce(ir, index));

    const decl: Ir.ir.Expr = .{ .decl = .{
        .name = name,
        .type = @"type",
    } };

    return .{ .index = try ir.push(decl, ir.at(.range, index.index).*) };
}

/// Returns the type of a container. Assumes the given index is a container.
fn typeOfContainer(ir: *Ir, index: Index) Err!Index {
    const container = ir.at(.expr, index).container;

    var decls = std.StringArrayHashMap(LazyDecl).init(ir.allocator);

    var iterator = container.defs.iterator();
    while (iterator.next()) |entry| {
        const def_index = entry.value_ptr.*;

        // Use its type if it already exists
        const lazy: LazyDecl = if (ir.at(.type, def_index.index).*) |def_type|
            .{ .decl = .{ .index = def_type } } // Safe to convert because tag(type(def)) is always decl.
        else
            .{ .def = def_index };

        try decls.putNoClobber(entry.key_ptr.*, lazy);
    }

    // TODO: Change typeOfDef to returning IndexOf(.decl)

    return try ir.push(.{ .product = decls }, ir.at(.range, index).*);
}

/// Returns the type of a dereference. Assumes the given index is a dereference.
fn typeOfDereference(ir: *Ir, index: Index) Err!Index {
    // Force a copy of the dereference to manually fix Zig's pass-by-value autoref in case of aliasing, which we have.
    const derefed_temp = ir.at(.expr, index).*;
    const derefed = (&derefed_temp.dereference).*;

    const derefed_type = try typeOf(ir, derefed);

    return switch (ir.at(.expr, derefed_type).*) {
        .pointer_type => |ptr| ptr,
        else => ir.fail(.{ .dereferenced_non_pointer = .{
            .expr = ir.at(.range, index).*,
            .type = exprToString(ir, derefed_type),
        } }),
    };
}

/// Returns the type of member access. Assumes the given index is member access.
/// This supports access via pointers.
fn typeOfMemberAccess(ir: *Ir, index: Index) Err!Index {
    const access = ir.at(.expr, index).member_access;

    const left_type = try typeOf(ir, access.container);

    return switch (ir.at(.expr, left_type).*) {
        .pointer_type => |ptr| {
            if (ir.at(.expr, ptr).* != .product) return ir.fail(.{ .unsupported_member_access = .{
                .type = exprToString(ir, left_type),
                .member = access.member,
            } });

            const member = ir.atOf(.decl, try typeOfMember(ir, ptr, access.member)).type;

            return try ir.push(.{ .pointer_type = member }, ir.at(.range, index).*);
        },
        .product => ir.atOf(.decl, try typeOfMember(ir, left_type, access.member)).type,
        else => ir.fail(.{ .unsupported_member_access = .{
            .type = exprToString(ir, left_type),
            .member = access.member,
        } }),
    };
}

/// Returns the member of a compound type definition given by the provided
/// name. Essentially, this returns a type of a field of a struct/enum given
/// its type.
fn typeOfMember(ir: *Ir, index: Index, member: Range) Err!IndexOf(.decl) {
    const members: std.StringArrayHashMap(LazyDecl) = getExprMembers(ir, index) orelse return ir.fail(.{ .unsupported_member_access = .{
        .type = exprToString(ir, index),
        .member = member,
    } });

    const member_index = members.getIndex(member.substr(ir.src)) orelse return ir.fail(.{ .unknown_member = .{
        .type = exprToString(ir, index),
        .member = member,
    } });

    // TODO: Reimplement private definitions.

    return typeOfMemberGivenIndex(ir, index, member_index);
}

/// Unsafely gets the type of a member, given its index.
fn typeOfMemberGivenIndex(ir: *Ir, index: Index, member: usize) Err!IndexOf(.decl) {
    return switch (getExprMembers(ir, index).?.values()[member]) {
        .decl => |decl| decl,
        .def => |def| {
            const def_type = try typeOfDef(ir, def);

            getExprMembers(ir, index).?.values()[member] = .{ .decl = def_type };

            return def_type;
        },
    };
}

fn getExprMembers(ir: *Ir, index: Index) ?std.StringArrayHashMap(LazyDecl) {
    return switch (ir.at(.expr, index).*) {
        inline .product, .sum => |mems| mems,
        else => null,
    };
}

/// Ensures that a lazy declaration is, in fact, a declaration, returning the
/// declaration variant after ensuring this.
fn ensureLazyDecl(ir: *Ir, lazy: LazyDecl) Err!IndexOf(.decl) {
    return switch (lazy) {
        .decl => |decl| decl,
        .def => |def| try typeOfDef(ir, def),
    };
}

/// Returns whether or not the given value represents a type.
///
/// This only applies for fully evaluated expressions; for example, this returns
/// `false` for `container { const a = word; }.a`.
pub fn isType(ir: *Ir, index: Index) bool {
    return switch (ir.at(.expr, index).*) {
        .word_type, .decl, .product, .sum, .pointer_type, .type => true,
        else => false,
    };
}

/// Expects that the given index points to a type, erroring if not.
pub fn expectType(ir: *Ir, index: Index) Err!void {
    if (!isType(ir, index)) return ir.fail(.{ .expected_type_expression = .{
        .has_wrong_type = ir.at(.range, index).*,
        .found_type = exprToString(ir, try typeOf(ir, index)),
    } });
}

// Type coercion

pub const TypeCoercion = enum {
    NonCoercible,
    Coercible,
    Equal,
};

/// Determines if two types are equal, coercible (from left to right), or
/// unequal and noncoercible. Errors if the provided expressions are types.
pub fn canCoerce(ir: *Ir, from: Index, to: Index) Err!TypeCoercion {
    if (from.index == to.index) return .Equal; // Helps with checking the type for defs with coercion already calculated

    try expectType(ir, from);
    try expectType(ir, to);

    const from_value = ir.at(.expr, from).*;

    return switch (ir.at(.expr, to).*) {
        .word_type => if (from_value == .word_type) .Equal else .NonCoercible,
        .type => if (from_value == .type) .Equal else .NonCoercible,
        .decl => |to_decl| {
            if (from_value != .decl) return .NonCoercible;

            // TODO: Do we also ensure name equality?
            return canCoerce(ir, from_value.decl.type, to_decl.type);
        },
        .pointer_type => |to_ptr| {
            if (from_value != .pointer_type) return .NonCoercible;

            return canCoerce(ir, from_value.pointer_type, to_ptr);
        },
        .product => |to_product| {
            if (from_value == .product) return canCoerceDeclMaps(ir, from_value.product, to_product);

            return .NonCoercible;
        },
        .sum => |to_sum| {
            // TODO: A sum type inside a sum type won't get coerced correctly, since it'll think it's trying to coerce all values instead of coerce into a single value.
            if (from_value == .sum) return canCoerceDeclMaps(ir, from_value.sum, to_sum);

            // TODO: When product type construction becomes an operator, this
            //       will need to check for decls.
            if (from_value != .product) return .NonCoercible;
            const product: std.StringArrayHashMap(LazyDecl) = from_value.product;

            if (product.keys().len != 1) return .NonCoercible;
            const decl_from_name = product.keys()[0];

            // Check that the single value is contained in this sum type.
            if (to_sum.getIndex(decl_from_name)) |decl_to_index| {
                const decl_from_type = try typeOfMemberGivenIndex(ir, from, 0);
                const decl_to_type = try typeOfMemberGivenIndex(ir, to, decl_to_index);

                const can_coerce = try canCoerce(ir, decl_from_type.index, decl_to_type.index);

                return if (can_coerce != .NonCoercible) .Coercible else .NonCoercible;
            } else return .NonCoercible;
        },
        else => unreachable, // Not types
    };
}

/// Coercion semantics between maps of decls.
fn canCoerceDeclMaps(ir: *Ir, from: std.StringArrayHashMap(LazyDecl), to: std.StringArrayHashMap(LazyDecl)) Err!TypeCoercion {
    if (from.values().len != to.values().len) return .NonCoercible;

    var must_coerce = false;

    for (to.keys(), to.values()) |key, value| {
        const from_decl = try ensureLazyDecl(ir, from.get(key) orelse return .NonCoercible);
        const to_decl = try ensureLazyDecl(ir, value);

        const can_coerce = try canCoerce(ir, from_decl.index, to_decl.index);

        switch (can_coerce) {
            .NonCoercible => return .NonCoercible,
            .Coercible => must_coerce = true,
            .Equal => {},
        }
    }

    return if (must_coerce) .Coercible else .Equal;
}

// Evaluation

/// Evaluates an expression until a value is returned. This value is not
/// guaranteed to be deeply evaluated.
pub fn eval(ir: *Ir, index: Index) Err!Index {
    return fmapExpr(.{
        .filter = isEvaluable,
        .map = evalOnce,
        .order = .bottomUp,
    }, ir, index);
}

fn isEvaluable(ir: *Ir, index: Index) bool {
    return switch (ir.at(.expr, index).*) {
        .dereference, .member_access, .coerce => true,
        .word, .word_type, .def, .decl, .product, .sum, .pointer_type, .type, .container, .pointer => false,
    };
}

fn evalOnce(ir: *Ir, index: Index) Err!Index {
    try markEval(ir, index);
    defer ir.at(.evaluating, index).* = false;

    return switch (ir.at(.expr, index).*) {

        .dereference => |deref| try switch (ir.at(.expr, deref).*) {
            .pointer => |def| shallowCopy(ir, try defValueCoerce(ir, def)),
            else => ir.fail(.{ .dereferenced_non_pointer = .{
                .expr = ir.at(.range, index).*,
                .type = exprToString(ir, try typeOf(ir, deref)),
            } }),
        },

        .member_access => |access| try switch (ir.at(.expr, access.container).*) {
            .pointer => |def| {
                const def_value = try defValueCoerce(ir, def);

                const member = try evalMemberAccessRaw(ir, def_value, access.member);

                return try ir.push(.{ .pointer = member }, ir.at(.range, index).*);
            },
            else => {
                const member = try evalMemberAccessRaw(ir, access.container, access.member);

                return ir.atOf(.def, member).value;
            },
        },

        .coerce => try evalCoerce(ir, .{ .index = index }),

        else => unreachable, // Cannot evaluate non-basic expressions!
    };
}

/// Finds a member on a container.
fn evalMemberAccessRaw(ir: *Ir, index: Index, member: Range) Err!IndexOf(.def) {
    if (ir.at(.expr, index).* != .container) return ir.fail(.{ .unsupported_member_access = .{
        .type = exprToString(ir, try typeOf(ir, index)),
        .member = member,
    } });

    const key = member.substr(ir.src);

    const def = ir.at(.expr, index).container.defs.get(key) orelse return ir.fail(.{ .unknown_member = .{
        .type = exprToString(ir, try typeOf(ir, index)),
        .member = member,
    } });

    if (ir.atOf(.def, def).access == .private) return ir.fail(.{ .private_member = .{
        .declaration = ir.at(.range, def.index).*,
        .member = member,
    } });

    return def;
}

/// Coerces a value to another type.
fn evalCoerce(ir: *Ir, index: IndexOf(.coerce)) Err!Index {
    const coerce = ir.atOf(.coerce, index);

    // Make a shallow copy before coercion
    const expr = try shallowCopy(ir, coerce.expr);

    // Since coercion currently doesn't change the value itself, we just change the type (for now)
    ir.at(.type, expr).* = ir.atOf(.coerce, index).type;

    return expr;
}

/// Makes a shallow copy of the given expression.
pub fn shallowCopy(ir: *Ir, index: Index) Err!Index {
    // TODO: Implement once mutability is added
    _ = ir;
    return index;
}

/// Tries to coerce a def to its type, returning an error if impossible, and the
/// value otherwise. This will calculate its type.
fn defValueCoerce(ir: *Ir, index: IndexOf(.def)) Err!Index {
    // Force a copy of the def to manually fix Zig's pass-by-value autoref in case of aliasing, which we have.
    const def_temp = ir.atOf(.def, index).*;
    const def = (&def_temp).*;

    // If there's no explicit type, no coercion is necessary.
    if (def.type == null) return def.value;

    // If we've already type checked, return the value.
    if (def.type_checked) return def.value;

    // Sets it immediately, so theoretically could convince the type checker
    // that it's completed, which would have problems if expression
    // self-dependency were not already handled.
    ir.atOf(.def, index).type_checked = true;

    // Otherwise, try to coerce.
    const from = try eval(ir, try typeOf(ir, def.value));
    const to = try eval(ir, ir.atOf(.def, index).type.?);

    const can_coerce = try canCoerce(ir, from, to);

    const def_value = ir.atOf(.def, index).value;

    return switch (can_coerce) {
        .NonCoercible => ir.fail(.{ .cannot_coerce = .{
            .from = exprToString(ir, from),
            .to = exprToString(ir, to),
            .context = ir.at(.range, def_value).*,
        } }),
        .Coercible => {
            const coerce = try ir.push(.{ .coerce = .{
                .expr = def_value,
                .type = to,
            } }, ir.at(.range, def_value).*);

            ir.atOf(.def, index).value = coerce;
            return coerce;
        },
        .Equal => def_value,
    };
}

pub const FmapContext = struct {
    filter: fn(*Ir, Index) bool,
    map: fn(*Ir, Index) Err!Index,
    order: enum { topDown, bottomUp },
};

/// Copy-on-write recursive application of mapping operations to an expression.
pub fn fmapExpr(comptime context: FmapContext, ir: *Ir, index: Index) Err!Index {
    if (!context.filter(ir, index)) return index;

    const maybe_bottom_up = if (context.order == .bottomUp) try unopinionatedFmapExpr(context, ir, index) else index;

    const mapped = try context.map(ir, try context.map(ir, maybe_bottom_up));

    const maybe_top_down = if (context.order == .topDown) try unopinionatedFmapExpr(context, ir, mapped) else mapped;

    return maybe_top_down;
}

inline fn unopinionatedFmapExpr(comptime context: FmapContext, ir: *Ir, index: Index) Err!Index {
    // Make a copy of the initial value
    var value = ir.at(.expr, index).*;

    // Make any changes we need to
    switch (value) {
        .word, .word_type, .type => {},

        .pointer_type => |*ptr| ptr.* = try fmapExpr(context, ir, ptr.*),
        .dereference => |*deref| deref.* = try fmapExpr(context, ir, deref.*),

        .def => |*def| def.value = try fmapExpr(context, ir, def.value), // TODO: Should the type also be mapped?
        .decl => |*decl| decl.type = try fmapExpr(context, ir, decl.type),
        .member_access => |*access| access.container = try fmapExpr(context, ir, access.container),
        .pointer => |*ptr| ptr.index = try fmapExpr(context, ir, ptr.index),

        .coerce => |*coerce| {
            coerce.expr = try fmapExpr(context, ir, coerce.expr);
            coerce.type = try fmapExpr(context, ir, coerce.type);
        },

        // TODO: It's an issue that we copy product/sum/container but not the
        // underlying allocation, meaning that modifications to it do end up
        // modifying global state. This will be fixed once ** and ++ become
        // binary operations instead of pseudo-operations.

        .container => |*container| {
            for (container.defs.values()) |*def| {
                def.index = try fmapExpr(context, ir, def.index);
            }
        },

        .product, .sum => |decls| {
            for (decls.values()) |*lazy| {
                switch (lazy.*) {
                    .def => |def| lazy.def.index = try fmapExpr(context, ir, def.index),
                    .decl => |decl| lazy.decl.index = try fmapExpr(context, ir, decl.index),
                }
            }
        },
    }

    // Push a new copy of the expression if a change was made.
    return if (std.meta.eql(value, ir.at(.expr, index).*))
        index
    else
        ir.push(value, ir.at(.range, index).*);
}

pub fn markEval(ir: *Ir, index: Index) Err!void {
    const evaluating = ir.at(.evaluating, index);

    if (evaluating.*) {
        return ir.fail(.{ .dependency_loop = ir.at(.range, index).* });
    }
    evaluating.* = true;
}

pub fn exprToString(ir: *Ir, expr: Index) []u8 {
    var out = std.ArrayListUnmanaged(u8){};

    // TODO: Make this more robust
    ir.printExpr(expr, out.writer(ir.allocator)) catch |err| std.debug.panic("could not print type: {any}", .{err});

    return out.items;
}

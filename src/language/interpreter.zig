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
/// returning the index of the type.
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
    const derefed = ir.at(.expr, index).dereference;

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
    if (!isType(ir, index)) return ir.fail(.{ .expected_type_expression = ir.at(.range, index).* });
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

pub const Depth = enum { shallow, deep };

/// Evaluates the given expression. This must be known within the current
/// context; if not, it will error.
pub fn eval(ir: *Ir, index: Index, comptime depth: Depth) Err!Index {
    try markEval(ir, index);
    defer ir.at(.evaluating, index).* = false;

    if (depth == .deep) switch (ir.at(.expr, index).*) {
        .product => |product| for (product.values()) |*value| {
            value.* = .{ .decl = try ensureLazyDecl(ir, value.*) };
        },

        .sum => |sum| for (sum.values()) |*value| {
            value.* = .{ .decl = try ensureLazyDecl(ir, value.*) };
        },

        .decl => |decl| ir.at(.expr, index).decl.type = try eval(ir, decl.type, depth),
        .pointer_type => |ptr| ir.at(.expr, index).pointer_type = try eval(ir, ptr, depth),
        .pointer => |ptr| _ = try evalDef(ir, ptr, depth),

        .def => _ = try evalDef(ir, .{ .index = index }, depth),

        .container => |container| {
            for (container.defs.values()) |def| _ = try evalDef(ir, def, depth);

            // Since deep evaluating the defs also deep evaluates their types,
            // we can do normal shallow typeOf for this.
            _ = try typeOf(ir, index);
        },

        else => {},
    };

    return switch (ir.at(.expr, index).*) {
        .dereference => try evalDereference(ir, .{ .index = index }, depth),
        .member_access => try evalMemberAccess(ir, .{ .index = index }, depth),
        .coerce => try evalCoerce(ir, .{ .index = index }, depth),

        .word, .word_type, .type => index,
        else => index, // Expressions that only get evaluated when .deep
    };
}

/// Evaluates a definition. This does nothing if depth is shallow, since there's
/// no other way to depend on a definition itself (instead of its value).
fn evalDef(ir: *Ir, index: IndexOf(.def), comptime depth: Depth) Err!IndexOf(.def) {
    if (depth == .deep) {
        const def_value = try eval(ir, try defValueCoerce(ir, index), depth);

        ir.atOf(.def, index).value = def_value;
        ir.at(.type, index.index).* = try eval(ir, try typeOf(ir, def_value), depth);
    }

    return index;
}

/// Makes a shallow copy of the definition that is pointed to, resulting in the
/// value of the definition.
fn evalDereference(ir: *Ir, index: IndexOf(.dereference), comptime depth: Depth) Err!Index {
    const deref = ir.atOf(.dereference, index).*;
    const value = try eval(ir, deref, depth);

    return switch (ir.at(.expr, value).*) {
        .pointer => |def| try shallowCopy(ir, try eval(ir, try defValueCoerce(ir, def), depth)),
        else => ir.fail(.{ .dereferenced_non_pointer = .{
            .expr = ir.at(.range, index.index).*,
            .type = exprToString(ir, try typeOf(ir, value)),
        } }),
    };
}

/// Definition/declaration access, product type elimination, and unsafe sum type
/// elimination. Supports access through pointers.
fn evalMemberAccess(ir: *Ir, index: IndexOf(.member_access), comptime depth: Depth) Err!Index {
    const member_access = ir.atOf(.member_access, index);
    const container = try eval(ir, member_access.container, .shallow);

    return switch (ir.at(.expr, container).*) {
        .pointer => |def| {
            const def_value = try eval(ir, try defValueCoerce(ir, def), .shallow);

            const member = try evalMemberAccessRaw(ir, def_value, member_access.member, depth);

            return try ir.push(.{ .pointer = member }, ir.at(.range, index.index).*);
        },
        else => {
            const member = try evalMemberAccessRaw(ir, container, member_access.member, depth);

            return ir.atOf(.def, member).value;
        },
    };
}

/// Finds a member on a container (will be expanded in the future). This assumes
/// the index points to an evaluated expression.
fn evalMemberAccessRaw(ir: *Ir, index: Index, member: Range, comptime depth: Depth) Err!IndexOf(.def) {
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

    return if (depth == .deep) try evalDef(ir, def, depth) else def;
}

/// Coerces a value to another type.
fn evalCoerce(ir: *Ir, index: IndexOf(.coerce), comptime depth: Depth) Err!Index {
    const coerce = ir.atOf(.coerce, index);

    // Make a shallow copy before coercion
    const expr = try shallowCopy(ir, try eval(ir, coerce.expr, depth));

    // Since coercion currently doesn't change the value itself, we just change the type (for now)
    ir.at(.type, expr).* = ir.atOf(.coerce, index).type;

    return expr;
}

/// Returns whether or not the given value is fully evaluated.
pub fn isEvaluated(ir: *Ir, index: Index) bool {
    return switch (ir.at(.expr, index).*) {
        .word, .word_type, .def, .decl, .product, .sum, .pointer_type, .type, .container, .pointer => true,
        else => false,
    };
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
    var def = ir.atOf(.def, index);

    // If there's no explicit type, no coercion is necessary.
    if (def.type == null) return def.value;

    // If we've already type checked, return the value.
    if (def.type_checked) return def.value;

    // Sets it immediately, so theoretically could convince the type checker
    // that it's completed, which would have problems if expression
    // self-dependency were not already handled.
    def.type_checked = true;

    // Otherwise, try to coerce.
    const from = try eval(ir, try typeOf(ir, def.value), .shallow);
    const to = try eval(ir, ir.atOf(.def, index).type.?, .shallow);

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

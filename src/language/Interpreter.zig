const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Range = tokenizer.Range;

const Ir = @import("Ir.zig");
const ir = Ir.ir;
const Index = Ir.Index;
const IndexOf = Ir.IndexOf;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

context: *Ir,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8, context: *Ir) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .context = context,
    };
}

// Type inference

/// Determines the type of the given value, caching it in the value and
/// returning the index of the type.
pub fn typeOf(self: *@This(), raw_index: Index) Err!Index {
    // Check if the initial expression already has a type
    if (self.at(.type, raw_index).*) |@"type"| return @"type";

    const index = try self.softEval(raw_index);

    // Check if the new expression already has a type
    if (self.at(.type, index).*) |@"type"| return @"type";

    // Make sure we're not already evaluating it (likely an infinite loop)
    if (self.at(.evaluating, index).*) return self.fail(.{ .dependency_loop = self.at(.range, index).* });
    self.at(.evaluating, index).* = true;
    defer self.at(.evaluating, index).* = false;

    const value_type = try switch (self.at(.expr, index).*) {
        .word => self.context.push(.word_type, self.at(.range, index).*),

        .word_type, .decl, .product, .sum, .pointer_type, .type => self.context.push(.type, self.at(.range, index).*),

        .pointer => |ptr| self.context.push(.{ .pointer_type = try self.typeOf(try self.defValueCoerce(ptr.index)) }, self.at(.range, index).*),
        .parentheses => |parens| self.typeOf(parens),

        .def => self.typeOfDef(index),
        .container => self.typeOfContainer(index),
        .dereference => self.typeOfDereference(index),
        .member_access => self.typeOfMemberAccess(index),

        .coerce => |coerce| coerce.type,
    };

    self.at(.type, index).* = value_type;

    return value_type;
}

/// Returns the type of a def. Assumes the given index is a def.
fn typeOfDef(self: *@This(), index: Index) Err!Index {
    const name = self.at(.expr, index).def.name;

    const @"type" = try self.typeOf(try self.defValueCoerce(index));

    const decl: ir.Expr = .{ .decl = .{
        .name = name,
        .type = @"type",
    } };

    return try self.context.push(decl, self.at(.range, index).*);
}

/// Returns the type of a container. Assumes the given index is a container.
fn typeOfContainer(self: *@This(), index: Index) Err!Index {
    const container = self.at(.expr, index).container;

    var decls = std.StringArrayHashMap(IndexOf(.decl)).init(self.allocator);

    var iterator = container.defs.iterator();
    while (iterator.next()) |entry| {
        try decls.putNoClobber(entry.key_ptr.*, .{ .index = try self.typeOfDef(entry.value_ptr.index) });
    }
    
    // TODO: Change typeOfDef to returning IndexOf(.decl)

    return try self.context.push(.{ .product = decls }, self.at(.range, index).*);
}

/// Returns the type of a dereference. Assumes the given index is a dereference.
fn typeOfDereference(self: *@This(), index: Index) Err!Index {
    const derefed = self.at(.expr, index).dereference;

    const derefed_type = try self.typeOf(derefed);

    return switch (self.at(.expr, derefed_type).*) {
        .pointer_type => |ptr| ptr,
        else => self.fail(.{ .dereferenced_non_pointer = .{
            .expr = self.at(.range, derefed_type).*,
            .type = self.exprToString(derefed_type),
        } }),
    };
}

/// Returns the type of member access. Assumes the given index is member access.
/// This supports access via pointers.
fn typeOfMemberAccess(self: *@This(), index: Index) Err!Index {
    const access = self.at(.expr, index).member_access;

    const left_type = try self.typeOf(access.container);

    return switch (self.at(.expr, left_type).*) {
        .pointer_type => |ptr| {
            if (self.at(.expr, ptr).* != .product) return self.fail(.{ .unsupported_member_access = .{
                .type = self.exprToString(left_type),
                .member = access.member,
            } });

            const member = try self.typeOfMemberAccessRaw(ptr, access.member);

            return try self.context.push(.{ .pointer_type = member }, self.at(.range, index).*);
        },
        .product => self.typeOfMemberAccessRaw(left_type, access.member),
        else => self.fail(.{ .unsupported_member_access = .{
            .type = self.exprToString(left_type),
            .member = access.member,
        } }),
    };
}

/// Returns the type of the member of a field. This assumes the index points
/// to a product type.
fn typeOfMemberAccessRaw(self: *@This(), index: Index, member: Range) Err!Index {
    const product: std.StringArrayHashMap(IndexOf(.decl)) = self.at(.expr, index).product;

    const decl = product.get(member.substr(self.src)) orelse return self.fail(.{ .unknown_member = .{
        .container = self.at(.range, index).*,
        .member = member,
    } });

    // TODO: Handle private decls
    if (false) return self.fail(.{ .private_member = .{
        .declaration = decl.name,
        .member = member,
    } });

    return self.atOf(.decl, decl).type;
}

/// Returns whether or not the given value represents a type.
///
/// This only applies for fully evaluated expressions; for example, this returns
/// `false` for `container { const a = word; }.a`.
pub fn isType(self: *@This(), index: Index) bool {
    return switch (self.at(.expr, index).*) {
        .word_type, .decl, .product, .sum, .pointer_type, .type => true,
        else => false,
    };
}

/// Expects that the given index points to a type, erroring if not.
pub fn expectType(self: *@This(), index: Index) Err!void {
    if (!self.isType(index)) return self.fail(.{ .expected_type_expression = self.at(.range, index).* });
}

// Type coercion

pub const TypeCoercion = enum {
    NonCoercible,
    Coercible,
    Equal,
};

/// Determines if two types are equal, coercible (from left to right), or
/// unequal and noncoercible. Errors if the provided expressions are types.
pub fn canCoerce(self: *@This(), from: Index, to: Index) Err!TypeCoercion {
    if (from.index == to.index) return .Equal; // Helps with checking the type for defs with coercion already calculated

    try self.expectType(from);
    try self.expectType(to);

    const from_value = self.at(.expr, from).*;
    
    return switch (self.at(.expr, to).*) {
        .word_type => if (from_value == .word_type) .Equal else .NonCoercible,
        .type => if (from_value == .type) .Equal else .NonCoercible,
        .decl => |to_decl| {
            if (from_value != .decl) return .NonCoercible;

            // TODO: Do we also ensure name equality?
            return self.canCoerce(from_value.decl.type, to_decl.type);
        },
        .pointer_type => |to_ptr| {
            if (from_value != .pointer_type) return .NonCoercible;

            return self.canCoerce(from_value.pointer_type, to_ptr);
        },
        .product => |to_product| {
            if (from_value == .product) return self.canCoerceDeclMaps(from_value.product, to_product);

            return .NonCoercible;
        },
        .sum => |to_sum| {
            if (from_value == .sum) return self.canCoerceDeclMaps(from_value.sum, to_sum);

            // TODO: When product type construction becomes an operator, this
            //       will need to check for defs.
            if (from_value != .product) return .NonCoercible;
            const product = from_value.product;

            if (product.values().len != 1) return .NonCoercible;
            const decl = self.atOf(.decl, product.values()[0]);

            // Check that the single value is contained in this sum type.
            if (to_sum.get(decl.name.substr(self.src))) |decl_to| {
                const can_coerce = try self.canCoerce(product.values()[0].index, decl_to.index);

                return if (can_coerce != .NonCoercible) .Coercible else .NonCoercible;
            } else return .NonCoercible;
        },
        else => unreachable, // Not types
    };
}

/// Coercion semantics between maps of decls.
fn canCoerceDeclMaps(self: *@This(), from: std.StringArrayHashMap(IndexOf(.decl)), to: std.StringArrayHashMap(IndexOf(.decl))) Err!TypeCoercion {
    if (from.values().len != to.values().len) return .NonCoercible;

    var must_coerce = false;

    for (to.values()) |to_decl_index| {
        const to_decl: *ir.Decl = self.atOf(.decl, to_decl_index);
        const to_decl_name = to_decl.name.substr(self.src);

        const from_decl_index = from.get(to_decl_name) orelse return .NonCoercible;
        const from_decl: *ir.Decl = self.atOf(.decl, from_decl_index);

        const can_coerce = try self.canCoerce(from_decl.type, to_decl.type);

        switch (can_coerce) {
            .NonCoercible => return .NonCoercible,
            .Coercible => must_coerce = true,
            .Equal => {},
        }
    }

    return if (must_coerce) .Coercible else .Equal;
}

// Evaluation

/// Evaluates the given expression. This must be known within the current
/// context; if not, it will error.
pub fn eval(self: *@This(), index: Index) Err!Index {
    const result = try self.softEval(index);

    // TODO: Hard evaluation entails calling functions and doing anything that
    //       may be "destructive". However, nothing like this currently exists,
    //       so we can just return it directly.
    if (!self.isEvaluated(result)) unreachable;

    return result;
}

/// Evaluates the given expression as far as possible without any "destructive"
/// actions. This helps namespace resolution and helps simplify expressions
/// without messing with the AST at all. It's also useful for
///
/// For example, this is able to evaluate expressions like
/// `container { const x = 5; }.x`, but does not error on expressions that
/// depend on unknown variables.
pub fn softEval(self: *@This(), index: Index) Err!Index {
    self.at(.evaluating, index).* = true;
    defer self.at(.evaluating, index).* = false;

    // TODO: When relying on the value of a def, we need to check its type.
    return switch (self.at(.expr, index).*) {
        .word, .word_type, .decl, .product, .sum, .pointer_type, .type, .pointer, .def, .container => index,

        .parentheses => |parens| parens,
        .dereference => try self.softEvalDereference(index),
        .member_access => try self.softEvalMemberAccess(index),
        .coerce => try self.softEvalCoerce(index),
    };
}

/// Makes a shallow copy of the variable that is pointed to.
fn softEvalDereference(self: *@This(), index: Index) Err!Index {
    const left_raw = self.at(.expr, index).dereference;

    // Try to soft evaluate the left side.
    //   If it's a pointer, continue
    //   If it's not fully evaluated, exit
    //   if it's fully evaluated but not a pointer (all other cases), fail.

    const left = try self.softEval(left_raw);

    if (!self.isEvaluated(left)) return index;

    if (self.at(.expr, left).* != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
        .expr = self.at(.range, index).*,
        .type = self.exprToString(try self.typeOf(left)),
    } });

    // Make a shallow copy of the value that is pointed to

    const ptr_value_index = self.at(.expr, left).pointer;
    const def_value_index = self.atOf(.def, ptr_value_index).value;

    return try self.shallowCopy(def_value_index);
}

/// Returns the value of member access. Assumes the given index points to member
/// access. This supports access via pointers.
fn softEvalMemberAccess(self: *@This(), index: Index) Err!Index {
    const access = self.at(.expr, index).member_access;

    return switch (self.at(.expr, access.container).*) {
        .pointer => |def| {
            const def_value = self.atOf(.def, def).value;

            if (self.at(.expr, def_value).* != .container) return self.fail(.{ .unsupported_member_access = .{
                .type = self.exprToString(try self.typeOf(access.container)),
                .member = access.member,
            } });

            const member = try self.softEvalMemberAccessRaw(def_value, access.member);

            return try self.context.push(.{ .pointer = .{ .index = member } }, self.at(.range, index).*);
        },
        .container => {
            const member = try self.softEvalMemberAccessRaw(access.container, access.member);

            const value = self.at(.expr, member).def.value;

            return try self.softEval(value);
        },
        else => self.fail(.{ .unsupported_member_access = .{
            .type = self.exprToString(access.container),
            .member = access.member,
        } }),
    };
}

/// Returns the value of the member of a container. This assumes the index
/// points to a container.
fn softEvalMemberAccessRaw(self: *@This(), index: Index, member: Range) Err!Index {
    const left = try self.softEval(index);

    if (!self.isEvaluated(left)) return index;

    if (self.at(.expr, left).* != .container) return self.fail(.{ .unsupported_member_access = .{
        .type = self.exprToString(try self.typeOf(left)),
        .member = member,
    } });

    const key = member.substr(self.src);
    const def = self.at(.expr, left).container.defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = self.at(.range, left).*,
        .member = member,
    } });

    if (self.atOf(.def, def).access == .private) return self.fail(.{ .private_member = .{
        .declaration = self.at(.range, def.index).*,
        .member = member,
    } });

    return def.index;
}

/// Coerces a value to another type. Assumes the index points to a coercion.
fn softEvalCoerce(self: *@This(), index: Index) Err!Index {
    // Exit if the coerced value cannot be evaluated
    const coerce = self.at(.expr, index).coerce;

    const coerce_value = try self.softEval(coerce.expr);
    if (!self.isEvaluated(coerce_value)) return index;

    // Make a shallow copy before coercion
    const value_copy = try self.shallowCopy(self.at(.expr, index).coerce.expr);

    // Since coercion currently can't change the value itself, we just change the expression type
    self.at(.type, value_copy).* = self.at(.expr, index).coerce.type;

    return value_copy;
}

/// Returns whether or not the given value is fully evaluated.
pub fn isEvaluated(self: *@This(), index: Index) bool {
    return switch (self.at(.expr, index).*) {
        .word, .word_type, .def, .decl, .product, .sum, .pointer_type, .type, .container, .pointer => true,
        else => false,
    };
}

/// Makes a shallow copy of the given expression.
pub fn shallowCopy(self: *@This(), index: Index) Err!Index {
    // TODO: Implement once mutability is added
    _ = self;
    return index;
}

/// Tries to coerce a def to its type, returning an error if impossible, and the
/// value otherwise. This will calculate its type.
fn defValueCoerce(self: *@This(), index: Index) Err!Index {
    const def = self.at(.expr, index).def;

    // If there's no explicit type, no coercion is necessary.
    if (def.type == null) return def.value;

    // Otherwise, try to coerce.
    const from = try self.typeOf(def.value);
    const to = self.at(.expr, index).def.type.?;

    const can_coerce = try self.canCoerce(from, to);

    const def_value = self.at(.expr, index).def.value;

    return switch (can_coerce) {
        .NonCoercible => self.fail(.{ .cannot_coerce = .{
            .from = self.exprToString(from),
            .to = self.exprToString(to),
            .context = self.at(.range, def_value).*,
        } }),
        .Coercible => {
            const coerce = try self.context.push(.{ .coerce = .{
                .expr = def_value,
                .type = to,
            } }, self.at(.range, def_value).*);

            self.at(.expr, index).def.value = coerce;
            return coerce;
        },
        .Equal => def_value,
    };
}

// pub fn expectTypeContainsValue(self: *@This(), @"type": Index, index: Index) Err!void {
//     const type_contains_value = try self.typeContainsValue(@"type", index);

//     if (!type_contains_value) {
//         return self.fail(.{ .mismatched_type = .{
//             .expected_type = self.exprToString(@"type"),
//             .found_type = self.exprToString(try self.typeOf(index)),
//             .expected_type_declared = self.context.indexGet(@"type").expr_range,
//             .has_wrong_type = self.context.indexGet(index).expr_range,
//         } });
//     }
// }

// pub fn typeContainsValue(self: *@This(), @"type": Index, expr: Index) Err!bool {
//     try self.expectIsType(@"type");
//     try self.expectEvaluated(expr);

//     const type_value = self.context.indexGet(@"type");
//     const expr_value = self.context.indexGet(expr);

//     return switch (type_value.expr) {
//         .word_type => expr_value.expr == .word,
//         .decl => |decl| try self.typeContainsValue(decl.type, expr), // TODO: Change when definitions are made expressions
//         .product => |product| {
//             if (expr_value.expr != .container) return false;
//             const container = &expr_value.expr.container;

//             // TODO: We shouldn't have to evaluate every container definition here.
//             // Can probably replace with something like #evalTypeOf.
//             // TODO: Should also disallow duplicates.
//             var container_iter = container.defs.iterator();
//             while (container_iter.next()) |def| try self.evalDef(def.value_ptr.*);

//             for (product.items) |def| {
//                 try self.evalExpr(def);
//                 // TODO: Expect the value to be a declaration.
//             }

//             // Confirm a bijection between container definitions and product declarations.
//             if (container.defs.keys().len != product.items.len) return false;

//             for (product.items) |item| {
//                 const decl = &self.context.indexGet(item).expr.decl;
//                 const decl_name = decl.name.substr(self.src);

//                 const container_def = container.defs.get(decl_name) orelse return false;
//                 const def = self.context.indexOfGet(.def, container_def);
//                 const def_name = def.name.substr(self.src);

//                 if (std.mem.eql(u8, def_name, decl_name)) {
//                     // TODO: Using the expression's value creates the fake notion of having a range.
//                     // TODO: This ignores the type annotation of the definition;
//                     if (!try self.typeContainsValue(decl.type, def.value)) {
//                         return false;
//                     }
//                 }
//             }

//             return true;
//         },
//         .sum => |sum| {
//             if (expr_value.expr != .container) return false;
//             const container = &expr_value.expr.container;

//             // TODO: We shouldn't have to evaluate every container definition here.
//             // Can probably replace with something like #evalTypeOf.
//             // TODO: Should also disallow duplicates.
//             var container_iter = container.defs.iterator();
//             while (container_iter.next()) |def| try self.evalDef(def.value_ptr.*);

//             for (sum.items) |def| {
//                 try self.evalExpr(def);
//                 // TODO: Expect the value to be a declaration.
//             }

//             // Check if the singular container definition fits within the sum type.
//             if (container.defs.keys().len != 1) return false;
//             const container_def = container.defs.values()[0];
//             const def = self.context.indexOfGet(.def, container_def);
//             const def_name = def.name.substr(self.src);

//             for (sum.items) |item| {
//                 const decl = &self.context.indexGet(item).expr.decl;
//                 const decl_name = decl.name.substr(self.src);

//                 if (std.mem.eql(u8, def_name, decl_name)) {
//                     // TODO: Using the expression's value creates the fake notion of having a range.
//                     // TODO: This ignores the type annotation of the definition;
//                     return self.typeContainsValue(decl.type, def.value);
//                 }
//             }

//             return false;
//         },
//         .pointer_type => |ptr_type| {
//             if (expr_value.expr != .pointer) return false;

//             const ptr_value = self.context.indexOfGet(.def, expr_value.expr.pointer).value;

//             try self.expectEvaluated(ptr_value);
//             try self.expectIsType(ptr_type);

//             return self.typeContainsValue(ptr_type, ptr_value);
//         },
//         .type => self.isType(expr),
//         else => unreachable,
//     };
// }

pub fn exprToString(self: *@This(), expr: Index) []u8 {
    var out = std.ArrayListUnmanaged(u8){};

    // TODO: Make this more robust
    self.context.printExpr(expr, out.writer(self.allocator)) catch |err| std.debug.panic("could not print type: {any}", .{err});

    return out.items;
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{CodeError} {
    self.error_context = @"error";
    return error.CodeError;
}

/// Utility function that proxies `self.context.at`.
pub fn at(self: *@This(), comptime field: std.meta.FieldEnum(Ir.Value), index: Index) *std.meta.FieldType(Ir.Value, field) {
    return self.context.at(field, index);
}

/// Utility function that proxies `self.context.atOf`.
pub fn atOf(self: *@This(), comptime variant: std.meta.Tag(ir.Expr), index: IndexOf(variant)) *std.meta.TagPayload(ir.Expr, index.variant) {
    return self.context.atOf(variant, index);
}

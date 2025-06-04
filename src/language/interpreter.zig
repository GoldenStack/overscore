const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Range = tokenizer.Range;

const Ir = @import("Ir.zig");
const Index = Ir.Index;
const IndexOf = Ir.IndexOf;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;

// Type inference

/// Determines the type of the given value, caching it in the value and
/// returning the index of the type.
pub fn typeOf(ir: *Ir, raw_index: Index) Err!Index {
    // Check if the initial expression already has a type
    if (ir.at(.type, raw_index).*) |@"type"| return @"type";

    const index = try softEval(ir, raw_index);

    // Check if the new expression already has a type
    if (ir.at(.type, index).*) |@"type"| return @"type";

    // Make sure we're not already evaluating it (likely an infinite loop)
    if (ir.at(.evaluating, index).*) return ir.fail(.{ .dependency_loop = ir.at(.range, index).* });
    ir.at(.evaluating, index).* = true;
    defer ir.at(.evaluating, index).* = false;

    const value_type = try switch (ir.at(.expr, index).*) {
        .word => ir.push(.word_type, ir.at(.range, index).*),

        .word_type, .decl, .product, .sum, .pointer_type, .type => ir.push(.type, ir.at(.range, index).*),

        .pointer => |ptr| ir.push(.{ .pointer_type = try typeOf(ir, try defValueCoerce(ir, ptr.index)) }, ir.at(.range, index).*),
        .parentheses => |parens| typeOf(ir, parens),

        .def => typeOfDef(ir, index),
        .container => typeOfContainer(ir, index),
        .dereference => typeOfDereference(ir, index),
        .member_access => typeOfMemberAccess(ir, index),

        .coerce => |coerce| coerce.type,
    };

    ir.at(.type, index).* = value_type;

    return value_type;
}

/// Returns the type of a def. Assumes the given index is a def.
fn typeOfDef(ir: *Ir, index: Index) Err!Index {
    const name = ir.at(.expr, index).def.name;

    const @"type" = try typeOf(ir, try defValueCoerce(ir, index));

    const decl: Ir.ir.Expr = .{ .decl = .{
        .name = name,
        .type = @"type",
    } };

    return try ir.push(decl, ir.at(.range, index).*);
}

/// Returns the type of a container. Assumes the given index is a container.
fn typeOfContainer(ir: *Ir, index: Index) Err!Index {
    const container = ir.at(.expr, index).container;

    var decls = std.StringArrayHashMap(IndexOf(.decl)).init(ir.allocator);

    var iterator = container.defs.iterator();
    while (iterator.next()) |entry| {
        try decls.putNoClobber(entry.key_ptr.*, .{ .index = try typeOfDef(ir, entry.value_ptr.index) });
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
            .expr = ir.at(.range, derefed_type).*,
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

            const member = try typeOfMemberAccessRaw(ir, ptr, access.member);

            return try ir.push(.{ .pointer_type = member }, ir.at(.range, index).*);
        },
        .product => typeOfMemberAccessRaw(ir, left_type, access.member),
        else => ir.fail(.{ .unsupported_member_access = .{
            .type = exprToString(ir, left_type),
            .member = access.member,
        } }),
    };
}

/// Returns the type of the member of a field. This assumes the index points
/// to a product type.
fn typeOfMemberAccessRaw(ir: *Ir, index: Index, member: Range) Err!Index {
    const product: std.StringArrayHashMap(IndexOf(.decl)) = ir.at(.expr, index).product;

    const decl = product.get(member.substr(ir.src)) orelse return ir.fail(.{ .unknown_member = .{
        .container = ir.at(.range, index).*,
        .member = member,
    } });

    // TODO: Handle private decls
    if (false) return ir.fail(.{ .private_member = .{
        .declaration = decl.name,
        .member = member,
    } });

    return ir.atOf(.decl, decl).type;
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
            if (from_value == .sum) return canCoerceDeclMaps(ir, from_value.sum, to_sum);

            // TODO: When product type construction becomes an operator, this
            //       will need to check for defs.
            if (from_value != .product) return .NonCoercible;
            const product = from_value.product;

            if (product.values().len != 1) return .NonCoercible;
            const decl = ir.atOf(.decl, product.values()[0]);

            // Check that the single value is contained in this sum type.
            if (to_sum.get(decl.name.substr(ir.src))) |decl_to| {
                const can_coerce = try canCoerce(ir, product.values()[0].index, decl_to.index);

                return if (can_coerce != .NonCoercible) .Coercible else .NonCoercible;
            } else return .NonCoercible;
        },
        else => unreachable, // Not types
    };
}

/// Coercion semantics between maps of decls.
fn canCoerceDeclMaps(ir: *Ir, from: std.StringArrayHashMap(IndexOf(.decl)), to: std.StringArrayHashMap(IndexOf(.decl))) Err!TypeCoercion {
    if (from.values().len != to.values().len) return .NonCoercible;

    var must_coerce = false;

    for (to.values()) |to_decl_index| {
        const to_decl: *Ir.ir.Decl = ir.atOf(.decl, to_decl_index);
        const to_decl_name = to_decl.name.substr(ir.src);

        const from_decl_index = from.get(to_decl_name) orelse return .NonCoercible;
        const from_decl: *Ir.ir.Decl = ir.atOf(.decl, from_decl_index);

        const can_coerce = try canCoerce(ir, from_decl.type, to_decl.type);

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
pub fn eval(ir: *Ir, index: Index) Err!Index {
    const result = try softEval(ir, index);

    // TODO: Hard evaluation entails calling functions and doing anything that
    //       may be "destructive". However, nothing like this currently exists,
    //       so we can just return it directly.
    if (!isEvaluated(ir, result)) unreachable;

    return result;
}

/// Evaluates the given expression as far as possible without any "destructive"
/// actions. This helps namespace resolution and helps simplify expressions
/// without messing with the AST at all. It's also useful for
///
/// For example, this is able to evaluate expressions like
/// `container { const x = 5; }.x`, but does not error on expressions that
/// depend on unknown variables.
pub fn softEval(ir: *Ir, index: Index) Err!Index {
    ir.at(.evaluating, index).* = true;
    defer ir.at(.evaluating, index).* = false;

    // TODO: When relying on the value of a def, we need to check its type.
    return switch (ir.at(.expr, index).*) {
        .word, .word_type, .decl, .product, .sum, .pointer_type, .type, .pointer, .def, .container => index,

        .parentheses => |parens| parens,
        .dereference => try softEvalDereference(ir, index),
        .member_access => try softEvalMemberAccess(ir, index),
        .coerce => try softEvalCoerce(ir, index),
    };
}

/// Makes a shallow copy of the variable that is pointed to.
fn softEvalDereference(ir: *Ir, index: Index) Err!Index {
    const left_raw = ir.at(.expr, index).dereference;

    // Try to soft evaluate the left side.
    //   If it's a pointer, continue
    //   If it's not fully evaluated, exit
    //   if it's fully evaluated but not a pointer (all other cases), fail.

    const left = try softEval(ir, left_raw);

    if (!isEvaluated(ir, left)) return index;

    if (ir.at(.expr, left).* != .pointer) return ir.fail(.{ .dereferenced_non_pointer = .{
        .expr = ir.at(.range, index).*,
        .type = exprToString(ir, try typeOf(ir, left)),
    } });

    // Make a shallow copy of the value that is pointed to

    const ptr_value_index = ir.at(.expr, left).pointer;
    const def_value_index = ir.atOf(.def, ptr_value_index).value;

    return try shallowCopy(ir, def_value_index);
}

/// Returns the value of member access. Assumes the given index points to member
/// access. This supports access via pointers.
fn softEvalMemberAccess(ir: *Ir, index: Index) Err!Index {
    const access = ir.at(.expr, index).member_access;

    return switch (ir.at(.expr, access.container).*) {
        .pointer => |def| {
            const def_value = ir.atOf(.def, def).value;

            if (ir.at(.expr, def_value).* != .container) return ir.fail(.{ .unsupported_member_access = .{
                .type = exprToString(ir, try typeOf(ir, access.container)),
                .member = access.member,
            } });

            const member = try softEvalMemberAccessRaw(ir, def_value, access.member);

            return try ir.push(.{ .pointer = .{ .index = member } }, ir.at(.range, index).*);
        },
        .container => {
            const member = try softEvalMemberAccessRaw(ir, access.container, access.member);

            const value = ir.at(.expr, member).def.value;

            return try softEval(ir, value);
        },
        else => ir.fail(.{ .unsupported_member_access = .{
            .type = exprToString(ir, access.container),
            .member = access.member,
        } }),
    };
}

/// Returns the value of the member of a container. This assumes the index
/// points to a container.
fn softEvalMemberAccessRaw(ir: *Ir, index: Index, member: Range) Err!Index {
    const left = try softEval(ir, index);

    if (!isEvaluated(ir, left)) return index;

    if (ir.at(.expr, left).* != .container) return ir.fail(.{ .unsupported_member_access = .{
        .type = exprToString(ir, try typeOf(ir, left)),
        .member = member,
    } });

    const key = member.substr(ir.src);
    const def = ir.at(.expr, left).container.defs.get(key) orelse return ir.fail(.{ .unknown_member = .{
        .container = ir.at(.range, left).*,
        .member = member,
    } });

    if (ir.atOf(.def, def).access == .private) return ir.fail(.{ .private_member = .{
        .declaration = ir.at(.range, def.index).*,
        .member = member,
    } });

    return def.index;
}

/// Coerces a value to another type. Assumes the index points to a coercion.
fn softEvalCoerce(ir: *Ir, index: Index) Err!Index {
    // Exit if the coerced value cannot be evaluated
    const coerce = ir.at(.expr, index).coerce;

    const coerce_value = try softEval(ir, coerce.expr);
    if (!isEvaluated(ir, coerce_value)) return index;

    // Make a shallow copy before coercion
    const value_copy = try shallowCopy(ir, ir.at(.expr, index).coerce.expr);

    // Since coercion currently can't change the value itself, we just change the expression type
    ir.at(.type, value_copy).* = ir.at(.expr, index).coerce.type;

    return value_copy;
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
fn defValueCoerce(ir: *Ir, index: Index) Err!Index {
    const def = ir.at(.expr, index).def;

    // If there's no explicit type, no coercion is necessary.
    if (def.type == null) return def.value;

    // Otherwise, try to coerce.
    const from = try typeOf(ir, def.value);
    const to = ir.at(.expr, index).def.type.?;

    const can_coerce = try canCoerce(ir, from, to);

    const def_value = ir.at(.expr, index).def.value;

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

            ir.at(.expr, index).def.value = coerce;
            return coerce;
        },
        .Equal => def_value,
    };
}

// pub fn expectTypeContainsValue(ir: *Ir, @"type": Index, index: Index) Err!void {
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

// pub fn typeContainsValue(ir: *Ir, @"type": Index, expr: Index) Err!bool {
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

pub fn exprToString(ir: *Ir, expr: Index) []u8 {
    var out = std.ArrayListUnmanaged(u8){};

    // TODO: Make this more robust
    ir.printExpr(expr, out.writer(ir.allocator)) catch |err| std.debug.panic("could not print type: {any}", .{err});

    return out.items;
}

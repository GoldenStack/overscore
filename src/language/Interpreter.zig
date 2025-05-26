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
    const index = try self.softEval(raw_index);

    if (self.get(index).evaluating) return self.fail(.{ .dependency_loop = self.get(index).expr_range });

    self.get(index).evaluating = true;
    defer self.get(index).evaluating = false;

    if (self.get(index).type) |@"type"| return @"type";

    const value_type = try switch (self.get(index).expr) {
        .word => self.context.indexPush(.word_type, self.get(index).expr_range),

        .word_type, .decl, .product, .sum, .pointer_type, .type => self.context.indexPush(.type, self.get(index).expr_range),

        .pointer => |ptr| self.context.indexPush(.{ .pointer_type = try self.typeOfDefValue(ptr.index) }, self.get(index).expr_range),
        .parentheses => |parens| self.typeOf(parens),

        .def => self.typeOfDef(index),
        .container => self.typeOfContainer(index),
        .dereference => self.typeOfDereference(index),
        .member_access => self.typeOfMemberAccess(index),

        .coerce => |coerce| coerce.type,
    };

    self.get(index).type = value_type;

    return value_type;
}

/// Returns the type of a def. Assumes the given index is a def.
fn typeOfDef(self: *@This(), index: Index) Err!Index {
    const name = self.get(index).expr.def.name;

    const @"type" = try self.typeOfDefValue(index);

    const decl: ir.Expr = .{ .decl = .{
        .name = name,
        .type = @"type",
    } };

    return try self.context.indexPush(decl, self.get(index).expr_range);
}

/// Returns the type of the value of a def. Assumes the given index is a def.
fn typeOfDefValue(self: *@This(), index: Index) Err!Index {
    const def = self.get(index).expr.def;

    const actual_type = try self.typeOf(def.value);

    if (def.type == null) return actual_type;
    const def_type = def.type.?;

    const can_coerce = try self.canCoerce(actual_type, def_type);

    return switch (can_coerce) {
        .NonCoercible => self.fail(.{ .cannot_coerce = .{
            .from = self.exprToString(actual_type),
            .to = self.exprToString(def_type),
            .context = self.get(index).expr_range,
        } }),
        .Coercible => {
            const coerce = try self.context.indexPush(.{ .coerce = .{
                .expr = def.value,
                .type = def_type,
            } }, self.get(index).expr_range);

            return try self.typeOf(coerce);
        },
        .Equal => def_type,
    };
}

/// Returns the type of a container. Assumes the given index is a container.
fn typeOfContainer(self: *@This(), index: Index) Err!Index {
    const container = self.get(index).expr.container;

    var decls = std.ArrayList(IndexOf(.decl)).init(self.allocator);

    for (container.defs.values()) |def_index| {
        const def_type = try self.typeOfDef(def_index.index); // TODO: Change to returning IndexOf(.decl)

        try decls.append(.{ .index = def_type });
    }

    return try self.context.indexPush(.{ .product = decls }, self.get(index).expr_range);
}

/// Returns the type of a dereference. Assumes the given index is a dereference.
fn typeOfDereference(self: *@This(), index: Index) Err!Index {
    const derefed = self.get(index).expr.dereference;

    const derefed_type = try self.typeOf(derefed);

    return switch (self.get(derefed_type).expr) {
        .pointer_type => |ptr| ptr,
        else => self.fail(.{ .dereferenced_non_pointer = .{
            .expr = self.get(derefed_type).expr_range,
            .type = self.exprToString(derefed_type),
        } }),
    };
}

/// Returns the type of member access. Assumes the given index is member access.
/// This supports access via pointers.
fn typeOfMemberAccess(self: *@This(), index: Index) Err!Index {
    const access = self.get(index).expr.member_access;

    const left_type = try self.typeOf(access.container);

    return switch (self.get(left_type).expr) {
        .pointer_type => |ptr| {
            if (self.get(ptr).expr != .product) return self.fail(.{ .unsupported_member_access = .{
                .type = self.exprToString(left_type),
                .member = access.member,
            } });

            const member = try self.typeOfMemberAccessRaw(ptr, access.member);

            return try self.context.indexPush(.{ .pointer_type = member }, self.get(index).expr_range);
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
    const product = self.get(index).expr.product;

    for (product.items) |field| {
        const decl = self.get(field.index).expr.decl;

        if (!std.mem.eql(u8, decl.name.substr(self.src), member.substr(self.src))) continue;

        // TODO: Handle private decls
        if (false) return self.fail(.{ .private_member = .{
            .declaration = decl.name,
            .member = member,
        } });

        return field.index;
    } else return self.fail(.{ .unknown_member = .{
        .container = self.get(index).expr_range,
        .member = member,
    } });
}

/// Returns whether or not the given value represents a type.
///
/// This only applies for fully evaluated expressions; for example, this returns
/// `false` for `container { const a = word; }.a`.
pub fn isType(self: *@This(), index: Index) bool {
    return switch (self.get(index).expr) {
        .word_type, .decl, .product, .sum, .pointer_type, .type => true,
        else => false,
    };
}

// Type coercion

pub const TypeCoercion = enum {
    NonCoercible,
    Coercible,
    Equal,
};

/// Requires that the provided expressions are types.
pub fn canCoerce(self: *@This(), from: Index, to: Index) Err!TypeCoercion {
    _ = self;
    _ = from;
    _ = to;
    return .Equal;
}

// Evaluation

/// Evaluates the given expression as far as possible without any "destructive"
/// actions. This helps namespace resolution and helps simplify expressions
/// without messing with the AST at all. It's also useful for
///
/// For example, this is able to evaluate expressions like
/// `container { const x = 5; }.x`, but does not error on expressions that
/// depend on unknown variables.
pub fn softEval(self: *@This(), index: Index) Err!Index {
    self.get(index).evaluating = true;

    defer self.get(index).evaluating = false;

    // TODO: When relying on the value of a def, we need to check its type.
    return switch (self.get(index).expr) {
        .word, .word_type, .decl, .product, .sum, .pointer_type, .type, .pointer, .def, .container => index,

        .parentheses => |parens| parens,
        .dereference => try self.softEvalDereference(index),
        .member_access => try self.softEvalMemberAccess(index),
        .coerce => try self.softEvalCoerce(index),
    };
}

/// Makes a shallow copy of the variable that is pointed to.
fn softEvalDereference(self: *@This(), index: Index) Err!Index {
    const left_raw = self.get(index).expr.dereference;

    // Try to soft evaluate the left side.
    //   If it's a pointer, continue
    //   If it's not fully evaluated, exit
    //   if it's fully evaluated but not a pointer (all other cases), fail.

    const left = try self.softEval(left_raw);

    if (!self.isEvaluated(left)) return index;

    if (self.get(left).expr != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
        .expr = self.get(left).expr_range,
        .type = self.exprToString(try self.typeOf(left)),
    } });

    // Make a shallow copy of the value that is pointed to

    const ptr_value_index = self.get(left).expr.pointer;
    const def_value_index = self.get(ptr_value_index.index).expr.def.value;

    return try self.shallowCopy(def_value_index);
}

/// Returns the value of member access. Assumes the given index points to member
/// access. This supports access via pointers.
fn softEvalMemberAccess(self: *@This(), index: Index) Err!Index {
    const access = self.get(index).expr.member_access;

    return switch (self.get(access.container).expr) {
        .pointer => |def| {
            const def_value = self.get(def.index).expr.def.value;

            if (self.get(def_value).expr != .container) return self.fail(.{ .unsupported_member_access = .{
                .type = self.exprToString(access.container),
                .member = access.member,
            } });

            const member = try self.softEvalMemberAccessRaw(def_value, access.member);

            return try self.context.indexPush(.{ .pointer = .{ .index = member } }, self.get(index).expr_range);
        },
        .container => try self.softEvalMemberAccessRaw(access.container, access.member),
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

    // TODO: the typeOf call loops infinitely.
    if (self.get(left).expr != .container) return self.fail(.{ .unsupported_member_access = .{
        .type = self.exprToString(try self.typeOf(left)),
        .member = member,
    } });

    const key = member.substr(self.src);
    const def = self.get(left).expr.container.defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = self.get(left).expr_range,
        .member = member,
    } });

    if (self.get(def.index).expr.def.access == .private) return self.fail(.{ .private_member = .{
        .declaration = self.get(def.index).expr_range,
        .member = member,
    } });

    return def.index;
}

/// Coerces a value to another type. Assumes the index points to a coercion.
fn softEvalCoerce(self: *@This(), index: Index) Err!Index {
    // At least for now, coercion only involves converting a single-definition
    // container to a sum type, which doesn't require modifying the value at
    // all. This means we can just convert the value.
    // Since coercion occurs to something that's already being dealt with (e.g.
    // a pointer or a struct), we don't *need* to copy it.

    const coerce = self.get(index).expr.coerce;

    // For now, can just change the type without inducing any cost.
    self.get(coerce.expr).type = coerce.type;

    return self.get(index).expr.coerce.expr;
}

/// Returns whether or not the given value is fully evaluated.
pub fn isEvaluated(self: *@This(), index: Index) bool {
    return switch (self.get(index).expr) {
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

// pub fn evalDef(self: *@This(), index: IndexOf(.def)) Err!void {
//     var def = self.context.indexOfGet(.def, index);

//     def.evaluating = true;

//     if (def.type) |@"type"| {
//         try self.evalExpr(@"type");
//         try self.evalExpr(def.value);
//         def.evaluating = false;

//         // It's fine to evalute the same thing at the same time (sometimes), but don't recursively type check.
//         if (!def.type_checked) {
//             def.type_checked = true;
//             // TODO: Coerce it if necessary
//             try self.expectIsType(@"type");
//             try self.expectTypeContainsValue(@"type", def.value);
//         }
//     } else {
//         try self.evalExpr(def.value);

//         def.evaluating = false;
//     }
// }

// Type functions

// pub fn expectIsType(self: *@This(), index: Index) Err!void {
//     if (!self.isType(index)) {
//         return self.fail(.{ .expected_type_expression = self.context.indexGet(index).expr_range });
//     }
// }

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

/// Utility function for making it easier to access values
fn get(self: *@This(), index: Index) *Ir.Value {
    return self.context.indexGet(index);
}

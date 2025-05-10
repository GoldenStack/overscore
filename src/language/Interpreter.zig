const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

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

context: Ir,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8, context: Ir) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .context = context,
    };
}

pub fn expectIsType(self: *@This(), expr: Ranged(Index)) Err!void {
    if (!self.isType(expr)) {
        return self.fail(.{ .expected_type_expression = expr.range });
    }
}

pub fn isType(self: *@This(), expr: Ranged(Index)) bool {
    const value = self.context.indexGet(expr.value).*;

    return switch (value) {
        .word_type, .decl, .product, .sum, .pointer_type, .type => true,
        else => false,
    };
}

pub fn expectEvaluated(self: *@This(), expr: Ranged(Index)) Err!void {
    const value = self.context.indexGet(expr.value).*;

    return switch (value) {
        .word, .word_type, .decl, .product, .sum, .pointer_type, .type, .container, .pointer => {},
        else => @panic("Expected fully evaluated expression"),
    };
}

pub fn expectTypeContainsValue(self: *@This(), @"type": Ranged(Index), expr: Ranged(Index)) Err!void {
    const type_contains_value = try self.typeContainsValue(@"type", expr);

    if (!type_contains_value) {
        return self.fail(.{ .mismatched_type = .{
            .expected_type = self.exprToString(@"type".value),
            .found_type = self.exprToString(try self.typeOf(expr)),
            .expected_type_declared = @"type".range,
            .has_wrong_type = expr.range,
        } });
    }
}

pub fn typeContainsValue(self: *@This(), @"type": Ranged(Index), expr: Ranged(Index)) Err!bool {
    try self.expectIsType(@"type");
    try self.expectEvaluated(expr);

    const type_value = self.context.indexGet(@"type".value);
    const expr_value = self.context.indexGet(expr.value);

    return switch (type_value.*) {
        .word_type => expr_value.* == .word,
        .decl => |decl| try self.typeContainsValue(decl.type, expr), // TODO: Change when definitions are made expressions
        .product => |product| {
            if (expr_value.* != .container) return false;
            const container = &expr_value.container;

            // TODO: We shouldn't have to evaluate every container definition here.
            // Can probably replace with something like #evalTypeOf.
            // TODO: Should also disallow duplicates.
            var container_iter = container.defs.iterator();
            while (container_iter.next()) |def| try self.evalDef(def.value_ptr.value);

            for (product.items) |def| {
                try self.evalExpr(def.value);
                // TODO: Expect the value to be a declaration.
            }

            // Confirm a bijection between container definitions and product declarations.
            if (container.defs.keys().len != product.items.len) return false;

            for (product.items) |item| {
                const decl = &self.context.indexGet(item.value).decl;
                const decl_name = decl.name.range.substr(self.src);

                const container_def = container.defs.get(decl_name) orelse return false;
                const def = self.context.indexOfGet(.def, container_def.value);
                const def_name = def.name.range.substr(self.src);

                if (std.mem.eql(u8, def_name, decl_name)) {
                    // TODO: Using the expression's value creates the fake notion of having a range.
                    // TODO: This ignores the type annotation of the definition;
                    return self.typeContainsValue(decl.type, def.value);
                }
            }

            return false;
        },
        .sum => |sum| {
            if (expr_value.* != .container) return false;
            const container = &expr_value.container;

            // TODO: We shouldn't have to evaluate every container definition here.
            // Can probably replace with something like #evalTypeOf.
            // TODO: Should also disallow duplicates.
            var container_iter = container.defs.iterator();
            while (container_iter.next()) |def| try self.evalDef(def.value_ptr.value);

            for (sum.items) |def| {
                try self.evalExpr(def.value);
                // TODO: Expect the value to be a declaration.
            }

            // Check if the singular container definition fits within the sum type.
            if (container.defs.keys().len != 1) return false;
            const container_def = container.defs.values()[0];
            const def = self.context.indexOfGet(.def, container_def.value);
            const def_name = def.name.range.substr(self.src);

            for (sum.items) |item| {
                const decl = &self.context.indexGet(item.value).decl;
                const decl_name = decl.name.range.substr(self.src);

                if (std.mem.eql(u8, def_name, decl_name)) {
                    // TODO: Using the expression's value creates the fake notion of having a range.
                    // TODO: This ignores the type annotation of the definition;
                    return self.typeContainsValue(decl.type, def.value);
                }
            }

            return false;
        },
        .pointer_type => |ptr_type| {
            if (expr_value.* != .pointer) return false;

            const ptr_value = self.context.indexOfGet(.def, expr_value.pointer.value).value;

            try self.expectEvaluated(ptr_value);
            try self.expectIsType(ptr_type);

            return self.typeContainsValue(ptr_type, ptr_value);
        },
        .type => self.isType(expr),
        else => unreachable,
    };
}

pub fn evalMain(self: *@This(), index: IndexOf(.container)) Err!Index {
    const container = self.context.indexOfGet(.container, index);

    const main = container.defs.get("main") orelse @panic("No main expression found!");

    const main_index = main.value;
    try self.evalDef(main_index);

    return self.context.indexOfGet(.def, main_index).value.value;
}

pub fn typeOfDef(self: *@This(), index: Ranged(IndexOf(.def))) Err!Ranged(Index) {
    const def = self.context.indexOfGet(.def, index.value);

    if (def.type) |@"type"| {
        // TODO: Type mismatch can potentially evade detection if the type of a
        // definition is needed but not its value.
        return @"type";
    } else {
        try self.evalDef(index.value);
        return index.swap(try self.typeOf(def.value));
    }
}

pub fn evalDef(self: *@This(), index: IndexOf(.def)) Err!void {
    var def = self.context.indexOfGet(.def, index);

    def.evaluating = true;

    if (def.type) |@"type"| {
        try self.evalExpr(@"type".value);
        try self.evalExpr(def.value.value);
        def.evaluating = false;

        // It's fine to evalute the same thing at the same time (sometimes), but don't recursively type check.
        if (!def.type_checked) {
            def.type_checked = true;
            try self.expectIsType(@"type");
            try self.expectTypeContainsValue(@"type", def.value);
        }
    } else {
        try self.evalExpr(def.value.value);

        def.evaluating = false;
    }
}

pub fn typeOf(self: *@This(), expr: Ranged(Index)) Err!Index {
    try self.expectEvaluated(expr);

    const expr_value = self.context.indexGet(expr.value);

    if (self.isType(expr)) return self.context.indexPush(.type);

    return switch (expr_value.*) {
        .word => try self.context.indexPush(.word_type),
        .container => |container| {
            var decls = std.ArrayList(Ranged(Index)).init(self.allocator);

            for (container.defs.values()) |def| {
                const def_value = self.context.indexOfGet(.def, def.value);

                // TODO: Using the expression's value creates the fake notion of having a range.
                const def_type = try self.typeOfDef(def_value.value.swap(def.value));

                // TODO: We don't have ranges for values.
                const fake_decl = try self.context.indexOfPush(.decl, .{
                    .name = def_value.name,
                    .type = def_type,
                });

                // TODO: Using the decl's range creates the fake notion of having a range.
                try decls.append(def.swap(fake_decl.index)); // Fake-out!
            }

            return try self.context.indexPush(.{ .product = decls });
        },
        .pointer => |ptr| self.context.indexPush(.{ .pointer_type = try self.typeOfDef(ptr) }),
        else => unreachable,
    };
}

/// Evaluates an expression until it is a raw value that cannot be decomposed
/// any further.
pub fn evalExpr(self: *@This(), index: Index) Err!void {
    const expr = self.context.indexGet(index);

    switch (expr.*) {
        .word, .word_type, .type => {}, // Minimal types

        .container => {}, // Already minimal, and containers are lazy (at least during interpretation)

        .def => {
            // TODO: Evaluate the def?
        },

        .decl => |decl| {
            // TODO: Is it necessary to evaluate the type?
            _ = decl;
        },

        .product => |product| {
            _ = product; // TODO: Evaluate fields?
        },

        .sum => |sum| {
            _ = sum; // TODO: Evaluate fields?
        },

        .pointer_type => |ptr| {
            try self.evalExpr(ptr.value);
            try self.expectIsType(ptr);
        },

        .pointer => |ptr| {
            const def = self.context.indexOfGet(.def, ptr.value);

            if (def.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = def.name.range,
                .depends = ptr.range,
            } });

            try self.evalDef(ptr.value);
        },

        .dereference => |deref| {
            try self.evalExpr(deref.value);
            const left = self.context.indexGet(deref.value);

            if (left.* != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
                .expr = deref.range,
                .type = self.exprToString(try self.typeOf(deref)),
            } });

            const def = self.context.indexOfGet(.def, left.pointer.value);

            // TODO: Shallow copy the expression on dereference.
            expr.* = self.context.indexGet(def.value.value).*;
        },

        .parentheses => |parens| {
            expr.* = self.context.indexGet(parens.value).*; // Clone it (suboptimal)
            try self.evalExpr(index);
        },

        .member_access => |access| {
            try self.evalExpr(access.container.value);

            try self.containerOrPtrMemberAccess(index, access.container, access.member);
        },
    }
}

/// Product type elimination on containers or pointers.
pub fn containerOrPtrMemberAccess(self: *@This(), expr: Index, container: Ranged(Index), member: Ranged(Token)) Err!void {
    const container_expr = self.context.indexGet(container.value);

    switch (container_expr.*) {
        .pointer => |ptr| {
            const ptr_range = self.context.indexOfGet(.def, ptr.value);
            const ptr_value = self.context.indexGet(ptr_range.value.value);

            if (ptr_value.* == .container) {
                // Access the field and then evaluate it
                const def = try self.containerMemberAccess(container.swap(ptr_range.value.value), member);
                try self.evalDef(def);

                self.context.indexGet(expr).* = .{ .pointer = container.swap(def) };
                return;
            }
        },
        .container => {
            // Access the field and evaluate it
            const def = try self.containerMemberAccess(container, member);
            try self.evalDef(def);

            // Clone the field (suboptimal) into the current expression
            self.context.indexGet(expr).* = self.context.indexGet(self.context.indexOfGet(.def, def).value.value).*;
            return;
        },
        else => {},
    }

    return self.fail(.{ .unsupported_member_access = .{
        .type = self.exprToString(try self.typeOf(container)),
        .member = member.range,
    } });
}

/// Product type elimination on containers or pointers. This assumes that the
/// provided container is a container.
pub fn containerMemberAccess(self: *@This(), container: Ranged(Index), member: Ranged(Token)) Err!IndexOf(.def) {
    const defs = self.context.indexGet(container.value).container.defs;

    const key = member.range.substr(self.src);

    const def_index = defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = container.range,
        .member = member.range,
    } });

    const def = self.context.indexOfGet(.def, def_index.value);

    if (def.access == .private) return self.fail(.{ .private_member = .{
        .declaration = def.name.range,
        .member = member.range,
    } });

    return def_index.value;
}

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

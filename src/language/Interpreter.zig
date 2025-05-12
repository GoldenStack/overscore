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

context: Ir,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8, context: Ir) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .context = context,
    };
}

pub fn expectIsType(self: *@This(), index: Index) Err!void {
    if (!self.isType(index)) {
        return self.fail(.{ .expected_type_expression = self.context.indexGet(index).expr_range });
    }
}

pub fn isType(self: *@This(), index: Index) bool {
    const value = self.context.indexGet(index);

    return switch (value.expr) {
        .word_type, .decl, .product, .sum, .pointer_type, .type => true,
        else => false,
    };
}

pub fn expectEvaluated(self: *@This(), index: Index) Err!void {
    const value = self.context.indexGet(index);

    return switch (value.expr) {
        .word, .word_type, .decl, .def, .product, .sum, .pointer_type, .type, .container, .pointer => {},
        else => @panic("Expected fully evaluated expression"),
    };
}

pub fn expectTypeContainsValue(self: *@This(), @"type": Index, index: Index) Err!void {
    const type_contains_value = try self.typeContainsValue(@"type", index);

    if (!type_contains_value) {
        return self.fail(.{ .mismatched_type = .{
            .expected_type = self.exprToString(@"type"),
            .found_type = self.exprToString(try self.typeOf(index)),
            .expected_type_declared = self.context.indexGet(@"type").expr_range,
            .has_wrong_type = self.context.indexGet(index).expr_range,
        } });
    }
}

pub fn typeContainsValue(self: *@This(), @"type": Index, expr: Index) Err!bool {
    try self.expectIsType(@"type");
    try self.expectEvaluated(expr);

    const type_value = self.context.indexGet(@"type");
    const expr_value = self.context.indexGet(expr);

    return switch (type_value.expr) {
        .word_type => expr_value.expr == .word,
        .decl => |decl| try self.typeContainsValue(decl.type, expr), // TODO: Change when definitions are made expressions
        .product => |product| {
            if (expr_value.expr != .container) return false;
            const container = &expr_value.expr.container;

            // TODO: We shouldn't have to evaluate every container definition here.
            // Can probably replace with something like #evalTypeOf.
            // TODO: Should also disallow duplicates.
            var container_iter = container.defs.iterator();
            while (container_iter.next()) |def| try self.evalDef(def.value_ptr.*);

            for (product.items) |def| {
                try self.evalExpr(def);
                // TODO: Expect the value to be a declaration.
            }

            // Confirm a bijection between container definitions and product declarations.
            if (container.defs.keys().len != product.items.len) return false;

            for (product.items) |item| {
                const decl = &self.context.indexGet(item).expr.decl;
                const decl_name = decl.name.substr(self.src);

                const container_def = container.defs.get(decl_name) orelse return false;
                const def = self.context.indexOfGet(.def, container_def);
                const def_name = def.name.substr(self.src);

                if (std.mem.eql(u8, def_name, decl_name)) {
                    // TODO: Using the expression's value creates the fake notion of having a range.
                    // TODO: This ignores the type annotation of the definition;
                    if (!try self.typeContainsValue(decl.type, def.value)) {
                        return false;
                    }
                }
            }

            return true;
        },
        .sum => |sum| {
            if (expr_value.expr != .container) return false;
            const container = &expr_value.expr.container;

            // TODO: We shouldn't have to evaluate every container definition here.
            // Can probably replace with something like #evalTypeOf.
            // TODO: Should also disallow duplicates.
            var container_iter = container.defs.iterator();
            while (container_iter.next()) |def| try self.evalDef(def.value_ptr.*);

            for (sum.items) |def| {
                try self.evalExpr(def);
                // TODO: Expect the value to be a declaration.
            }

            // Check if the singular container definition fits within the sum type.
            if (container.defs.keys().len != 1) return false;
            const container_def = container.defs.values()[0];
            const def = self.context.indexOfGet(.def, container_def);
            const def_name = def.name.substr(self.src);

            for (sum.items) |item| {
                const decl = &self.context.indexGet(item).expr.decl;
                const decl_name = decl.name.substr(self.src);

                if (std.mem.eql(u8, def_name, decl_name)) {
                    // TODO: Using the expression's value creates the fake notion of having a range.
                    // TODO: This ignores the type annotation of the definition;
                    return self.typeContainsValue(decl.type, def.value);
                }
            }

            return false;
        },
        .pointer_type => |ptr_type| {
            if (expr_value.expr != .pointer) return false;

            const ptr_value = self.context.indexOfGet(.def, expr_value.expr.pointer).value;

            try self.expectEvaluated(ptr_value);
            try self.expectIsType(ptr_type);

            return self.typeContainsValue(ptr_type, ptr_value);
        },
        .type => self.isType(expr),
        else => unreachable,
    };
}

pub fn typeOfDef(self: *@This(), index: IndexOf(.def)) Err!Index {
    const def = self.context.indexOfGet(.def, index);

    if (def.type) |@"type"| {
        // TODO: Type mismatch can potentially evade detection if the type of a
        // definition is needed but not its value.
        return @"type";
    } else {
        try self.evalDef(index);
        return self.typeOf(def.value);
    }
}

pub fn evalDef(self: *@This(), index: IndexOf(.def)) Err!void {
    var def = self.context.indexOfGet(.def, index);

    def.evaluating = true;

    if (def.type) |@"type"| {
        try self.evalExpr(@"type");
        try self.evalExpr(def.value);
        def.evaluating = false;

        // It's fine to evalute the same thing at the same time (sometimes), but don't recursively type check.
        if (!def.type_checked) {
            def.type_checked = true;
            try self.expectIsType(@"type");
            try self.expectTypeContainsValue(@"type", def.value);
        }
    } else {
        try self.evalExpr(def.value);

        def.evaluating = false;
    }
}

pub fn typeOf(self: *@This(), index: Index) Err!Index {
    try self.expectEvaluated(index);

    const expr_value = self.context.indexGet(index);

    if (self.isType(index)) return self.context.indexPush(.type, expr_value.expr_range);

    return switch (expr_value.expr) {
        .word => try self.context.indexPush(.word_type, expr_value.expr_range),
        .container => |container| {
            var decls = std.ArrayList(Index).init(self.allocator);

            for (container.defs.values()) |def| {
                const def_value = self.context.indexGet(def.index);

                // TODO: Using the expression's value creates the fake notion of having a range.
                const def_type = try self.typeOfDef(def);

                // TODO: We don't have ranges for values.
                const fake_decl = try self.context.indexPush(.{ .decl = .{
                    .name = def_value.expr.def.name,
                    .type = def_type,
                } }, def_value.expr_range); // TODO: Using the decl's range creates the fake notion of having a range.

                try decls.append(fake_decl); // Fake-out!
            }

            return try self.context.indexPush(.{ .product = decls }, expr_value.expr_range);
        },
        .pointer => |ptr| self.context.indexPush(.{ .pointer_type = try self.typeOfDef(ptr) }, expr_value.expr_range),
        else => unreachable,
    };
}

/// Evaluates an expression until it is a raw value that cannot be decomposed
/// any further.
pub fn evalExpr(self: *@This(), index: Index) Err!void {
    const expr = self.context.indexGet(index);

    switch (expr.expr) {
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
            try self.evalExpr(ptr);
            try self.expectIsType(ptr);
        },

        .pointer => |ptr| {
            const def = self.context.indexOfGet(.def, ptr);

            if (def.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = def.name,
                .depends = self.context.indexGet(ptr.index).expr_range,
            } });

            try self.evalDef(ptr);
        },

        .dereference => |deref| {
            try self.evalExpr(deref);
            const left = self.context.indexGet(deref);

            if (left.expr != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
                .expr = left.expr_range,
                .type = self.exprToString(try self.typeOf(deref)),
            } });

            const def = self.context.indexOfGet(.def, left.expr.pointer);

            // TODO: Shallow copy the expression on dereference.
            expr.* = self.context.indexGet(def.value).*;
        },

        .parentheses => |parens| {
            expr.* = self.context.indexGet(parens).*; // Clone it (suboptimal)
            try self.evalExpr(index);
        },

        .member_access => |access| {
            try self.evalExpr(access.container);

            try self.containerOrPtrMemberAccess(index, access.container, access.member);
        },
    }
}

/// Product type elimination on containers or pointers.
pub fn containerOrPtrMemberAccess(self: *@This(), expr: Index, container: Index, member: Range) Err!void {
    const container_expr = self.context.indexGet(container);

    switch (container_expr.expr) {
        .pointer => |ptr| {
            const ptr_index = self.context.indexOfGet(.def, ptr);
            const ptr_value = self.context.indexGet(ptr_index.value);

            if (ptr_value.expr == .container) {
                // Access the field and then evaluate it
                const def = try self.containerMemberAccess(ptr_index.value, member);
                try self.evalDef(def);

                self.context.indexGet(expr).expr = .{ .pointer = def };
                return;
            }
        },
        .container => {
            // Access the field and evaluate it
            const def = try self.containerMemberAccess(container, member);
            try self.evalDef(def);

            // Clone the field (suboptimal) into the current expression
            self.context.indexGet(expr).* = self.context.indexGet(self.context.indexOfGet(.def, def).value).*;
            return;
        },
        else => {},
    }

    return self.fail(.{ .unsupported_member_access = .{
        .type = self.exprToString(try self.typeOf(container)),
        .member = member,
    } });
}

/// Product type elimination on containers or pointers. This assumes that the
/// provided container is a container.
pub fn containerMemberAccess(self: *@This(), container: Index, member: Range) Err!IndexOf(.def) {
    const index = self.context.indexGet(container);
    const defs = index.expr.container.defs;

    const key = member.substr(self.src);

    const def_index = defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = index.expr_range,
        .member = member,
    } });

    const def = self.context.indexOfGet(.def, def_index);

    if (def.access == .private) return self.fail(.{ .private_member = .{
        .declaration = def.name,
        .member = member,
    } });

    return def_index;
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

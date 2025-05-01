const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const Ir = @import("Ir.zig");
const ir = Ir.ir;
const Index = Ir.Index;
const failure = @import("failure.zig");

/// The error set of errors that can occur while interpreting.
pub const Error = error{
    CodeError,
    OutOfMemory,
};

src: [:0]const u8,
allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.InterpreterError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?failure.Error = null,

context: Ir,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8, context: Ir) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .context = context,
    };
}

fn expectIsType(self: *@This(), expr: Ranged(Index(ir.Expr))) Error!void {
    if (!self.isType(expr)) {
        return self.fail(.{ .expected_type_expression = expr.range });
    }
}

fn isType(self: *@This(), expr: Ranged(Index(ir.Expr))) bool {
    const value = self.context.indexExpr(expr.value).*;

    return switch (value) {
        .word_type, .decl, .product, .sum, .pointer_type, .type => true,
        else => false,
    };
}

fn expectEvaluated(self: *@This(), expr: Ranged(Index(ir.Expr))) Error!void {
    const value = self.context.indexExpr(expr.value).*;

    return switch (value) {
        .word, .word_type, .decl, .product, .sum, .pointer_type, .type, .container, .pointer => {},
        else => @panic("Expected fully evaluated expression"),
    };
}

fn expectTypeContainsValue(self: *@This(), @"type": Ranged(Index(ir.Expr)), expr: Ranged(Index(ir.Expr))) Error!void {
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

fn typeContainsValue(self: *@This(), @"type": Ranged(Index(ir.Expr)), expr: Ranged(Index(ir.Expr))) Error!bool {
    try self.expectIsType(@"type");
    try self.expectEvaluated(expr);

    const type_value = self.context.indexExpr(@"type".value);
    const expr_value = self.context.indexExpr(expr.value);

    return switch (type_value.*) {
        .word_type => expr_value.* == .word,
        .decl => |decl| try self.typeContainsValue(decl.type, expr), // TODO: Change when definitions are made expressions
        .product => |product| {
            if (expr_value.* != .container) return false;
            const container = self.context.indexContainer(expr_value.container);

            // TODO: We shouldn't have to evaluate every container definition here.
            // Can probably replace with something like #evalTypeOf.
            // TODO: Should also disallow duplicates.
            var container_iter = container.defs.iterator();
            while (container_iter.next()) |def| try self.evalDef(def.value_ptr.value.def);

            for (product.items) |def| try self.evalExpr(def.value);

            // Confirm a bijection between container definitions and product declarations.
            if (container.defs.keys().len != product.items.len) return false;

            for (product.items) |item| {
                const decl = &self.context.indexExpr(item.value).decl;
                const decl_name = decl.name.range.substr(self.src);

                const container_def = container.defs.get(decl_name) orelse return false;
                const def = self.context.indexDef(container_def.value.def);
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
            const container = self.context.indexContainer(expr_value.container);

            // TODO: We shouldn't have to evaluate every container definition here.
            // Can probably replace with something like #evalTypeOf.
            // TODO: Should also disallow duplicates.
            var container_iter = container.defs.iterator();
            while (container_iter.next()) |def| try self.evalDef(def.value_ptr.value.def);

            for (sum.items) |def| {
                try self.evalExpr(def.value);
                // TODO: Expect the value to be a declaration.
            }

            // Check if the singular container definition fits within the sum type.
            if (container.defs.keys().len != 1) return false;
            const container_def = container.defs.values()[0];
            const def = self.context.indexDef(container_def.value.def);
            const def_name = def.name.range.substr(self.src);

            for (sum.items) |item| {
                const decl = &self.context.indexExpr(item.value).decl;
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

            const ptr_value = self.context.indexDef(expr_value.pointer.value).value;

            try self.expectEvaluated(ptr_value);
            try self.expectIsType(ptr_type);

            return self.typeContainsValue(ptr_type, ptr_value);
        },
        .type => self.isType(expr),
        else => unreachable,
    };
}

pub fn evalMain(self: *@This(), index: Index(ir.Container)) Error!Index(ir.Expr) {
    const container = self.context.indexContainer(index);

    const main = container.defs.get("main") orelse @panic("No main expression found!");

    const main_index = main.value.def;
    try self.evalDef(main_index);

    return self.context.indexDef(main_index).value.value;
}

fn typeOfDef(self: *@This(), index: Ranged(Index(ir.Def))) Error!Ranged(Index(ir.Expr)) {
    const def = self.context.indexDef(index.value);

    if (def.type) |@"type"| {
        // TODO: Type mismatch can potentially evade detection if the type of a
        // definition is needed but not its value.
        return @"type";
    } else {
        try self.evalDef(index.value);
        return index.swap(try self.typeOf(def.value));
    }
}

fn evalDef(self: *@This(), index: Index(ir.Def)) Error!void {
    var def = self.context.indexDef(index);

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

pub fn typeOf(self: *@This(), expr: Ranged(Index(ir.Expr))) Error!Index(ir.Expr) {
    try self.expectEvaluated(expr);

    const expr_value = self.context.indexExpr(expr.value);

    if (self.isType(expr)) return self.context.pushExpr(.type);

    return switch (expr_value.*) {
        .word => try self.context.pushExpr(.word_type),
        .container => |container_index| {
            const container = self.context.indexContainer(container_index);
            var decls = std.ArrayList(Ranged(Index(ir.Expr))).init(self.allocator);

            for (container.defs.values()) |def| {
                const def_value = self.context.indexDef(def.value.def);

                // TODO: Using the expression's value creates the fake notion of having a range.
                const def_type = try self.typeOfDef(def_value.value.swap(def.value.def));

                // TODO: We don't have ranges for values.
                const fake_decl = try self.context.pushExpr(.{ .decl = .{
                    .name = def_value.name,
                    .type = def_type,
                } });

                // TODO: Using the decl's range creates the fake notion of having a range.
                try decls.append(def.swap(fake_decl)); // Fake-out!

            }

            return try self.context.pushExpr(.{ .product = decls });
        },
        .pointer => |ptr| {
            const @"type" = try self.typeOfDef(ptr);

            const ptr_type: ir.Expr = .{ .pointer_type = @"type" };

            return try self.context.pushExpr(ptr_type);
        },
        else => unreachable,
    };
}

/// Evaluates an expression until it is a raw value that cannot be decomposed
/// any further.
fn evalExpr(self: *@This(), index: Index(ir.Expr)) Error!void {
    const expr = self.context.indexExpr(index);

    switch (expr.*) {
        .word, .word_type, .type => {}, // Minimal types

        .container => {}, // Already minimal, and containers are lazy (at least during interpretation)

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
            const def = self.context.indexDef(ptr.value);

            if (def.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = def.name.range,
                .depends = ptr.range,
            } });

            try self.evalDef(ptr.value);
        },

        .dereference => |deref| {
            try self.evalExpr(deref.value);
            const left = self.context.indexExpr(deref.value);

            if (left.* != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
                .expr = deref.range,
                .@"type" = self.exprToString(try self.typeOf(deref)),
            } });

            const def = self.context.indexDef(left.pointer.value);

            // TODO: Shallow copy the expression on dereference.
            expr.* = self.context.indexExpr(def.value.value).*;
        },

        .parentheses => |parens| {
            expr.* = self.context.indexExpr(parens.value).*; // Clone it (suboptimal)
            try self.evalExpr(index);
        },

        .member_access => |access| {
            try self.evalExpr(access.container.value);

            try self.containerOrPtrMemberAccess(index, access.container, access.member);
        },
    }
}

/// Product type elimination on containers or pointers.
fn containerOrPtrMemberAccess(self: *@This(), expr: Index(ir.Expr), container: Ranged(Index(ir.Expr)), member: Ranged(Token)) Error!void {
    const container_expr = self.context.indexExpr(container.value);

    switch (container_expr.*) {
        .pointer => |ptr| {
            const ptr_range = self.context.indexDef(ptr.value);
            const ptr_value = self.context.indexExpr(ptr_range.value.value);

            if (ptr_value.* == .container) {
                // Access the field and then evaluate it
                const def = try self.containerMemberAccess(container.swap(ptr_range.value.value), member);
                try self.evalDef(def);

                self.context.indexExpr(expr).* = .{ .pointer = container.swap(def) };
                return;
            }
        },
        .container => {
            // Access the field and evaluate it
            const def = try self.containerMemberAccess(container, member);
            try self.evalDef(def);

            // Clone the field (suboptimal) into the current expression
            self.context.indexExpr(expr).* = self.context.indexExpr(self.context.indexDef(def).value.value).*;
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
fn containerMemberAccess(self: *@This(), container: Ranged(Index(ir.Expr)), member: Ranged(Token)) Error!Index(ir.Def) {
    const container_expr = self.context.indexExpr(container.value);
    const defs = self.context.indexContainer(container_expr.container).defs;

    const key = member.range.substr(self.src);

    const container_def = defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = container.range,
        .member = member.range,
    } });

    const def = self.context.indexDef(container_def.value.def);

    if (container_def.value.access == .private) return self.fail(.{ .private_member = .{
        .declaration = def.name.range,
        .member = member.range,
    } });

    return container_def.value.def;
}

fn exprToString(self: *@This(), expr: Index(ir.Expr)) []u8 {
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

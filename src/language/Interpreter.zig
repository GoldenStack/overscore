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
    InterpreterError,
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

pub fn evalMain(self: *@This(), index: Index(ir.Container)) Error!ir.Expr {
    const container = self.context.containers.items[index];
    const main = container.decls.get("main") orelse @panic("No main found!");

    const mainIndex = main.value.decl;

    try self.evalDecl(mainIndex);

    return self.context.decls.items[mainIndex].value.value;
}

fn evalDecl(self: *@This(), index: Index(ir.Decl)) Error!void {
    var decl = &self.context.decls.items[index];

    decl.evaluating = true;

    const @"type" = &decl.type.value;
    @"type".* = try self.evalExpr(@"type".*);

    const expr = &decl.value.value;
    expr.* = try self.evalExpr(expr.*);

    decl.evaluating = false;

    try self.expectTypeExpression(decl.type);
    try self.expectType(decl.value, @"type".type, decl.type.range);
}

fn expectTypeExpression(self: *@This(), expr: Ranged(ir.Expr)) Error!void {
    if (!self.typeContainsValue(expr.value, .type)) return self.fail(.{ .expected_type_expression = .{
        .found_type = try self.typeOf(expr.value),
        .has_wrong_type = expr.range,
    } });
}

fn expectType(self: *@This(), expr: Ranged(ir.Expr), @"type": ir.Type, cause: tokenizer.Range) Error!void {
    if (!self.typeContainsValue(expr.value, @"type")) return self.fail(.{ .mismatched_type = .{
        .expected_type = @"type",
        .found_type = try self.typeOf(expr.value),
        .expected_type_declared = cause,
        .has_wrong_type = expr.range,
    } });
}

fn typeContainsValue(self: *@This(), expr: ir.Expr, @"type": ir.Type) bool {
    return switch (@"type") {
        .word => expr == .word,
        .type => expr == .type,
        .container => false,
        .pointer => |ptr| {
            // An expression `e` has type `*T` if `e` is a pointer and `e.value` has type `T`.
            if (expr != .pointer) return false;

            // Pre-evaluated pointer types must be type expressions
            const value = self.context.decls.items[expr.pointer.value].value.value;
            const type2 = ptr.value.type;

            return self.typeContainsValue(value, type2);
        },
    };
}

/// Assumes that the provided expression has been fully evaluated. This means
/// the provided expression must be a *minimal* value, i.e. `.type` or `.word`.
///
/// Call `evalExpr` to make sure.
pub fn typeOf(self: *@This(), expr: ir.Expr) Error!ir.Type {
    return switch (expr) {
        .type => .type,
        .word => .word,
        .pointer => |ptr| {
            const ptr_value = self.context.decls.items[ptr.value].value.value;
            const ptr_type = try self.typeOf(ptr_value);

            const type_ptr = try self.allocator.create(ir.Expr);
            type_ptr.* = .{ .type = ptr_type };
            return .{ .pointer = ptr.swap(type_ptr) };
        },
        else => |value| std.debug.panic("Expected fully evaluated expression, but found one of type {s}", .{@tagName(value)}),
    };
}

fn evalExprPtr(self: *@This(), expr: *ir.Expr) Error!*ir.Expr {
    const ptr = try self.allocator.create(ir.Expr);
    ptr.* = try self.evalExpr(expr.*);
    return ptr;
}

/// Evaluates an expression until it's a raw value. This means the returned
/// expression is only ever `.type` or `.word`.
fn evalExpr(self: *@This(), expr: ir.Expr) Error!ir.Expr {
    return switch (expr) {
        .type => |@"type"| .{ .type = try self.evalType(@"type") },
        .word => |word| .{ .word = word },
        .pointer => |ptr| {
            const decl = &self.context.decls.items[ptr.value];

            if (decl.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = decl.name.range,
                .depends = ptr.range,
            } });

            try self.evalDecl(ptr.value);

            return expr;
        },
        .parentheses => |parens| try self.evalExpr(parens.value.*),
        .member_access => |access| {
            const container = access.container.swap(try self.evalExpr(access.container.value.*));
            return self.memberAccessGeneric(container, access.member);
        },
    };
}

fn evalType(self: *@This(), @"type": ir.Type) Error!ir.Type {
    return switch (@"type") {
        .type => .type,
        .word => .word,
        .container => |container| .{ .container = container },
        .pointer => |ptr| {
            const value = try ptr.map(self, evalExprPtr);
            try self.expectTypeExpression(value.swap(value.value.*));
            return .{ .pointer = value };
        },
    };
}

/// Member access works in several different ways, primarily static and dynamic
/// field access. This function figures it out.
fn memberAccessGeneric(self: *@This(), expr: Ranged(ir.Expr), member: Ranged(Token)) Error!ir.Expr {
    if (expr.value == .pointer) {
        const ptr_range = self.context.decls.items[expr.value.pointer.value].value;
        const ptr_value = ptr_range.value;

        if (ptr_value == .type and ptr_value.type == .container) {
            const decl = try self.staticMemberAccess(ptr_range, member);
            try self.evalDecl(decl);
            return .{ .pointer = expr.value.pointer.swap(decl) };
        }
    }

    if (expr.value == .type and expr.value.type == .container) {
        const decl = try self.staticMemberAccess(expr, member);
        try self.evalDecl(decl);
        return self.context.decls.items[decl].value.value;
    }

    return self.fail(.{ .unsupported_member_access = .{
        .type = try self.typeOf(expr.value),
        .member = member.range,
    } });
}

/// Access declarations (static members, essentially) on a container.
/// This assumes that the provided expression is a container type.
fn staticMemberAccess(self: *@This(), container: Ranged(ir.Expr), member: Ranged(Token)) Error!Index(ir.Decl) {
    const container_index = container.value.type.container;
    const decls = self.context.containers.items[container_index].decls;

    const key = member.range.substr(self.src);

    const container_decl = decls.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = container.range,
        .member = member.range,
    } });

    const decl = self.context.decls.items[container_decl.value.decl];

    if (container_decl.value.access == .private) return self.fail(.{ .private_member = .{
        .declaration = decl.name.range,
        .member = member.range,
    } });

    return container_decl.value.decl;
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}

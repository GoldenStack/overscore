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
    _ = self;
    return switch (@"type") {
        .word => expr == .word,
        .type => expr == .type,
        .container => false,
    };
}

/// Assumes that the provided expression has been fully evaluated. This means
/// the provided expression must be a *minimal* value, i.e. `.type` or `.word`.
///
/// Call `evalExpr` to make sure.
fn typeOf(self: *@This(), expr: ir.Expr) Error!ir.Type {
    _ = self;
    return switch (expr) {
        .type => .type,
        .word => .word,
        else => @panic("Expected fully evaluated expression"),
    };
}

/// Evaluates an expression until it's a raw value. This means the returned
/// expression is only ever `.type` or `.word`.
fn evalExpr(self: *@This(), expr: ir.Expr) Error!ir.Expr {
    return switch (expr) {
        .type => |@"type"| .{ .type = try self.evalType(@"type") },
        .word => |word| .{ .word = word },
        .ident => |ident| {
            const decl = &self.context.decls.items[ident.value];

            if (decl.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = decl.name.range,
                .depends = ident.range,
            } });

            try self.evalDecl(ident.value);

            return decl.value.value;
        },
        .parentheses => |parens| try self.evalExpr(parens.value.*),
        .member_access => |access| {
            const container = access.container.swap(try self.evalExpr(access.container.value.*));
            const member = try self.memberAccessGeneric(container, access.member);
            try self.evalDecl(member);
            return self.context.decls.items[member].value.value;
        },
    };
}

fn evalType(self: *@This(), @"type": ir.Type) Error!ir.Type {
    _ = self;
    return switch (@"type") {
        .type => .type,
        .word => .word,
        .container => |container| .{ .container = container },
    };
}

/// Member access works in several different ways, primarily static and dynamic
/// field access. This function figures it out.
fn memberAccessGeneric(self: *@This(), expr: Ranged(ir.Expr), member: Ranged(Token)) Error!Index(ir.Decl) {
    if (expr.value == .type and expr.value.type == .container) {
        return self.staticMemberAccess(expr, member);
    } // TODO: Handle struct field member access when added

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

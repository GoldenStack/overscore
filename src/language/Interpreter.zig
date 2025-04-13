const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const ast = @import("Parser.zig").ast;
const failure = @import("failure.zig");

pub const Value = struct {
    decl: ast.Decl,
    evaluating: bool = false,
};

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

namespace: std.StringHashMap(Value),

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .namespace = std.StringHashMap(Value).init(allocator),
    };
}

pub fn evalMain(self: *@This(), container: ast.Container) Error!ast.Expr {
    for (container.decls.items) |item| {
        try self.namespaceAdd(item.value.decl);
    }

    defer for (container.decls.items) |item| {
        self.namespaceRemove(item.value.decl);
    };

    const mainEntry = self.namespace.getEntry("main") orelse @panic("No main function found!");
    try self.evalNamespace(mainEntry.value_ptr);

    return mainEntry.value_ptr.decl.value.value;
}

fn evalNamespace(self: *@This(), value: *Value) Error!void {
    value.evaluating = true;
    try self.evalDecl(&value.decl);
    value.evaluating = false;
}

fn evalDecl(self: *@This(), decl: *ast.Decl) Error!void {
    const @"type" = &decl.type.value;
    @"type".* = try self.evalExpr(@"type".*);

    const expr = &decl.value.value;
    expr.* = try self.evalExpr(expr.*);

    try self.expectTypeExpression(decl.type);
    try self.expectType(decl.value, @"type".type, decl.type.range);
}

fn expectTypeExpression(self: *@This(), expr: Ranged(ast.Expr)) Error!void {
    if (!self.typeContainsValue(expr.value, .type)) return self.fail(.{ .expected_type_expression = .{
        .found_type = try self.typeOf(expr.value),
        .has_wrong_type = expr.range,
    } });
}

fn expectType(self: *@This(), expr: Ranged(ast.Expr), @"type": ast.Type, cause: tokenizer.Range) Error!void {
    if (!self.typeContainsValue(expr.value, @"type")) return self.fail(.{ .mismatched_type = .{
        .expected_type = @"type",
        .found_type = try self.typeOf(expr.value),
        .expected_type_declared = cause,
        .has_wrong_type = expr.range,
    } });
}

fn typeContainsValue(self: *@This(), expr: ast.Expr, @"type": ast.Type) bool {
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
fn typeOf(self: *@This(), expr: ast.Expr) Error!ast.Type {
    _ = self;
    return switch (expr) {
        .type => .type,
        .word => .word,
        else => @panic("Expected fully evaluated expression"),
    };
}

/// Evaluates an expression until it's a raw value. This means the returned
/// expression is only ever `.type` or `.word`.
fn evalExpr(self: *@This(), expr: ast.Expr) Error!ast.Expr {
    return switch (expr) {
        .type => |@"type"| .{ .type = try self.evalType(@"type") },
        .word => |word| .{ .word = word },
        .ident => |ident| {
            const entry = try self.namespaceGet(ident);
            if (entry.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = entry.decl.name.range,
                .depends = ident.range,
            } });
            try self.evalNamespace(entry);
            return entry.decl.value.value;
        },
        .parentheses => |parens| try self.evalExpr(parens.value.*),
        .member_access => |access| {
            const container = access.container.swap(try self.evalExpr(access.container.value.*));
            const member = try self.memberAccessGeneric(container, access.member);
            try self.evalNamespace(member);
            return member.decl.value.value;
        }
    };
}

fn evalType(self: *@This(), @"type": ast.Type) Error!ast.Type {
    _ = self;
    return switch (@"type") {
        .type => .type,
        .word => .word,
        .container => |container| .{ .container = container },
    };
}

/// Member access works in several different ways, primarily static and dynamic
/// field access. This function figures it out.
fn memberAccessGeneric(self: *@This(), expr: Ranged(ast.Expr), member: Ranged(Token)) Error!*Value {
    if (expr.value == .type and expr.value.type == .container) {
        return self.staticMemberAccess(expr, member);
    } // TODO: Handle struct field member access when added

    return self.fail(.{ .unsupported_member_access = .{
        .@"type" = try self.typeOf(expr.value),
        .member = member.range,
    } });
}

/// Access declarations (static members, essentially) on a container.
/// This assumes that the provided expression is a container type.
fn staticMemberAccess(self: *@This(), container: Ranged(ast.Expr), member: Ranged(Token)) Error!*Value {
    const name = member.range.substr(self.src);

    const container_value = container.value.type.container;

    for (container_value.decls.items) |*decl| {
        if (std.mem.eql(u8, decl.value.decl.name.range.substr(self.src), name)) {
            if (decl.value.access == .private) return self.fail(.{ .private_member = .{
                .declaration = decl.value.decl.name.range,
                .member = member.range,
            } });
            
            @panic("Member access not fully supported yet");
        }
    } else return self.fail(.{ .unknown_member = .{
        .container = container.range,
        .member = member.range,
    } });
}

fn namespaceGet(self: *@This(), ident: Ranged(Token)) Error!*Value {
    return if (self.namespace.getEntry(ident.range.substr(self.src))) |entry| {
        return entry.value_ptr;
    } else self.fail(.{ .unknown_identifier = ident.range });
}

/// Adds a declaration to this namespace, returning an error if impossible.
fn namespaceAdd(self: *@This(), decl: ast.Decl) Error!void {
    const result = try self.namespace.getOrPut(decl.name.range.substr(self.src));

    if (result.found_existing) {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = result.value_ptr.decl.name.range,
            .redeclared = decl.name.range,
        } });
    } else {
        result.value_ptr.* = .{ .decl = decl };
    }
}

/// Removes a declaration from this namespace, assuming it was previously
/// defined (panicking if otherwise).
fn namespaceRemove(self: *@This(), decl: ast.Decl) void {
    if (!self.namespace.remove(decl.name.range.substr(self.src))) {
        @panic("Expected to be able to remove previously-added declaration!");
    }
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}

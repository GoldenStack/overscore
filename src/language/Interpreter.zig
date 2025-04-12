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
    if (!self.isType(expr.value, .type)) return self.fail(.{ .expected_type_expression = .{
        .found_type = try self.typeOf(expr.value),
        .has_wrong_type = expr.range,
    } });
}

fn expectType(self: *@This(), expr: Ranged(ast.Expr), @"type": ast.Type, cause: tokenizer.Range) Error!void {
    if (!self.isType(expr.value, @"type")) return self.fail(.{ .mismatched_type = .{
        .expected_type = @"type",
        .found_type = try self.typeOf(expr.value),
        .expected_type_declared = cause,
        .has_wrong_type = expr.range,
    } });
}

fn isType(self: *@This(), expr: ast.Expr, @"type": ast.Type) bool {
    _ = self;
    return switch (@"type") {
        .word => expr == .word,
        .type => expr == .type,
    };
}

fn typeOf(self: *@This(), expr: ast.Expr) Error!ast.Type {
    return switch (expr) {
        .type => .type,
        .word => .word,
        .ident => |ident| try self.typeOf((try self.namespaceGet(ident)).decl.value.value),
        .parentheses => |parens| try self.typeOf(parens.value.*),
    };
}

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
    };
}

fn evalType(self: *@This(), @"type": ast.Type) Error!ast.Type {
    _ = self;
    return switch (@"type") {
        .type => .type,
        .word => .word,
    };
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

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

pub fn eval_main(self: *@This(), container: ast.Container) Error!ast.Expr {
    for (container.decls.items) |item| {
        try self.namespace_add(item.value.decl);
    }

    defer for (container.decls.items) |item| {
        self.namespace_remove(item.value.decl);
    };

    const mainEntry = self.namespace.getEntry("main") orelse @panic("No main function found!");
    try self.eval_namespace(mainEntry.value_ptr);

    return mainEntry.value_ptr.decl.value.value;
}

fn eval_namespace(self: *@This(), value: *Value) Error!void {
    value.evaluating = true;
    try self.eval_decl(&value.decl);
    value.evaluating = false;
}

fn eval_decl(self: *@This(), decl: *ast.Decl) Error!void {
    const @"type" = &decl.type.value;
    @"type".* = try self.eval_expr(@"type".*);

    const expr = &decl.value.value;
    expr.* = try self.eval_expr(expr.*);

    try self.expect_type(decl.type, .type, null);
    try self.expect_type(decl.value, @"type".type, decl.type.range);
}

fn expect_type(self: *@This(), expr: Ranged(ast.Expr), @"type": ast.Type, cause: ?tokenizer.Range) Error!void {
    if (!self.is_type(expr.value, @"type")) return self.fail(.{ .mismatched_type = .{
        .expected_type = @"type",
        .found_type = try self.type_of(expr.value),
        .expected_type_declared = cause,
        .has_wrong_type = expr.range,
    } });
}

fn is_type(self: *@This(), expr: ast.Expr, @"type": ast.Type) bool {
    _ = self;
    return switch (@"type") {
        .word => expr == .word,
        .type => expr == .type,
    };
}

fn type_of(self: *@This(), expr: ast.Expr) Error!ast.Type {
    return switch (expr) {
        .type => .type,
        .word => .word,
        .ident => |ident| try self.type_of((try self.namespace_get(ident)).decl.value.value),
    };
}

fn eval_expr(self: *@This(), expr: ast.Expr) Error!ast.Expr {
    return switch (expr) {
        .type => |@"type"| .{ .type = try self.eval_type(@"type") },
        .word => |word| .{ .word = word },
        .ident => |ident| {
            const entry = try self.namespace_get(ident);
            if (entry.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = entry.decl.name.range,
                .depends = ident.range,
            } });
            try self.eval_namespace(entry);
            return entry.decl.value.value;
        },
    };
}

fn eval_type(self: *@This(), @"type": ast.Type) Error!ast.Type {
    _ = self;
    return switch (@"type") {
        .type => .type,
        .word => .word,
    };
}

fn namespace_get(self: *@This(), ident: Ranged(Token)) Error!*Value {
    return if (self.namespace.getEntry(ident.range.substr(self.src))) |entry| {
        return entry.value_ptr;
    } else self.fail(.{ .unknown_identifier = ident.range });
}

/// Adds a declaration to this namespace, returning an error if impossible.
fn namespace_add(self: *@This(), decl: ast.Decl) Error!void {
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
fn namespace_remove(self: *@This(), decl: ast.Decl) void {
    if (!self.namespace.remove(decl.name.range.substr(self.src))) {
        @panic("Expected to be able to remove previously-added declaration!");
    }
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}

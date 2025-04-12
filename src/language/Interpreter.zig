const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const ast = @import("Parser.zig").ast;
const failure = @import("failure.zig");

pub const Value = struct {
    name: Ranged(Token),
    value: Ranged(ast.Expr),
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
        try self.namespace_add(item.value.name, item.value.value);
    }

    defer for (container.decls.items) |item| {
        self.namespace_remove(item.value.name);
    };

    const mainEntry = self.namespace.getEntry("main") orelse @panic("No main function found!");
    mainEntry.value_ptr.evaluating = true;

    const mainValue = try self.eval_expr(mainEntry.value_ptr.value.value);

    return mainValue;
}

fn eval_expr(self: *@This(), expr: ast.Expr) Error!ast.Expr {
    return switch (expr) {
        .type => |@"type"| .{ .type = try self.eval_type(@"type") },
        .word => |word| .{ .word = word },
        .ident => |ident| {
            const entry = try self.namespace_get(ident);
            if (entry.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = entry.name.range,
                .depends = ident.range,
            } });
            entry.evaluating = true;

            return try self.eval_expr(entry.value.value);
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

/// Adds the given name and value to this namespace, returning an error if
/// impossible.
fn namespace_add(self: *@This(), name: Ranged(Token), value: Ranged(ast.Expr)) Error!void {
    // TODO: Differentiate between duplicate struct members and otherwise
    //       duplicate names.
    const result = try self.namespace.getOrPut(name.range.substr(self.src));

    if (result.found_existing) {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = result.value_ptr.value.range,
            .redeclared = name.range,
        } });
    } else {
        result.value_ptr.* = .{
            .name = name,
            .value = value,
        };
    }
}

fn namespace_remove(self: *@This(), name: Ranged(Token)) void {
    if (!self.namespace.remove(name.range.substr(self.src))) {
        @panic("Expected to be able to remove previously-added declaration!");
    }
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}

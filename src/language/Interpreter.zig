const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const ast = @import("Parser.zig").ast;
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

namespace: TokenHashMap(Ranged(ast.Expr)),

/// Currently does not preserve order; waiting on
/// [#23344](https://github.com/ziglang/zig/issues/23344) `ArrayHashMap` once
/// again.
fn TokenHashMap(comptime V: type) type {
    const Context = struct {
        src: [:0]const u8,

        pub fn hash(self: @This(), token: Ranged(Token)) u64 {
            return std.hash_map.hashString(token.range.substr(self.src));
        }
        pub fn eql(self: @This(), a: Ranged(Token), b: Ranged(Token)) bool {
            return std.mem.eql(u8, a.range.substr(self.src), b.range.substr(self.src));
        }
    };

    return std.HashMap(Ranged(Token), V, Context, std.hash_map.default_max_load_percentage);
}

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .namespace = TokenHashMap(Ranged(ast.Expr)).initContext(allocator, .{
            .src = src,
        }),
    };
}

pub fn eval_main(self: *@This(), container: ast.Container) Error!ast.Expr {
    for (container.decls.items) |item| {
        try self.namespace_add(item.value.name, item.value.value);
    }

    defer for (container.decls.items) |item| {
        _ = self.namespace.remove(item.value.name);
    };

    // TODO: Get, eval, and return main.
    return undefined;
}

/// Adds the given name and value to this namespace, returning an error if
/// impossible.
fn namespace_add(self: *@This(), name: Ranged(Token), value: Ranged(ast.Expr)) Error!void {
    const result = try self.namespace.getOrPut(name);

    if (result.found_existing) {
        return self.fail(.{ .duplicate_member_name = .{
            .declared = result.key_ptr.*.range,
            .redeclared = name.range,
        } });
    } else {
        result.value_ptr.* = value;
    }
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}
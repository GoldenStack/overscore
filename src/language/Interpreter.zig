const std = @import("std");
const Parser = @import("Parser.zig");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const Int = std.math.big.int.Managed;
const Error = @import("failure.zig").Error;

/// The error set of errors that can occur while interpreting.
pub const InterpreterError = error{
    InterpreterError,
    OutOfMemory,
};

/// Currently does not preserve order; waiting on
/// [#23400](https://github.com/ziglang/zig/issues/23400) and/or
/// [#23362](https://github.com/ziglang/zig/issues/23362) to become an
/// `ArrayHashMap` once again.
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

src: [:0]const u8,
allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.InterpreterError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?Error = null,

namespace: TokenHashMap(*Ranged(Parser.Expr)),

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{ .src = src, .allocator = allocator, .namespace = TokenHashMap(*Ranged(Parser.Expr)).initContext(allocator, .{
        .src = src,
    }) };
}

pub fn eval_main(self: *@This(), container: *Ranged(Parser.Container)) InterpreterError!void {
    var main: ?*Ranged(Parser.Expr) = null;

    // Add to namespace
    for (container.value.decls.items) |*item| {
        const decl = &item.value.decl.value;

        try self.define(decl.name, &decl.value);

        if (self.is_main(decl)) {
            main = &decl.value;
        }
    }

    // Find the main function
    if (main == null) @panic("Couldn't find main");
    var params: []Ranged(Parser.Expr) = &.{};
    const result = try self.call_function(main.?, &params);
    std.debug.print("Main returned {any}\n", .{result.*});

    // Remove from namespace
    for (container.value.decls.items) |item| {
        _ = self.namespace.remove(item.value.decl.value.name);
    }
}

fn call_function(self: *@This(), function: *Ranged(Parser.Expr), args: *[]Ranged(Parser.Expr)) InterpreterError!*Ranged(Parser.Expr) {
    // Fully evaluate the function
    try self.eval_expr(function);

    if (function.value != .function) @panic("Tried to call non-function");

    const func = &function.value.function;
    const params = &func.parameters.items;

    if (args.len != params.len) @panic("Wrong number of parameters");

    // Add to namespace
    for (params.*, args.*) |param, *arg| {
        // TODO: Assert types
        try self.define(param.value.name, arg);
    }

    var @"return": ?*Ranged(Parser.Expr) = null;

    for (func.body.value.stmts.items) |*stmt| {
        const result = try self.eval_stmt(stmt);
        if (result) |value| {
            @"return" = value;
            break;
        }
    }

    // Remove from namespace
    for (params.*) |param| {
        _ = self.namespace.remove(param.value.name);
    }

    // TODO: Assert types. Unit types are implied.
    if (@"return" == null) @panic("Didn't return anything!") else return @"return".?;
}

fn is_main(self: *@This(), decl: *Parser.Decl) bool {
    if (!std.mem.eql(u8, "main", decl.name.range.substr(self.src))) return false;
    
    return switch (decl.value.value) {
        .function => |function| function.parameters.items.len == 0,
        else => false,
    };
}

fn eval_stmt(self: *@This(), stmt: *Ranged(Parser.Stmt)) InterpreterError!?*Ranged(Parser.Expr) {
    switch (stmt.value) {
        .decl => |*decl| try self.define(decl.value.name, &decl.value.value),
        .expr => |*expr| if (expr.value == .@"return") return expr.value.@"return",
    }
    return null;
}

fn eval_expr(self: *@This(), expr: *Ranged(Parser.Expr)) InterpreterError!void {
    return switch (expr.value) {
        .parentheses => |value| {
            expr.* = value.*;
        },
        .ident => |ident| if (self.namespace.get(ident)) |value| {
            expr.* = value.*;
        } else self.fail(.{ .unknown_identifier = ident.range }),
        .call => |*call| {
            const result = try self.call_function(call.function, &call.arguments.items);
            expr.* = result.*;
        },
        .function => {}, // Functions are values - nothing to evaluate
        .word => {}, // Words are values - nothing to evaluate
        else => @panic("todo"),
    };
}

fn define(self: *@This(), name: Ranged(Token), expr: *Ranged(Parser.Expr)) InterpreterError!void {
    const expr_ptr = try self.namespace_add(name);
    expr_ptr.* = expr;
}

fn namespace_add(self: *@This(), name: Ranged(Token)) InterpreterError!**Ranged(Parser.Expr) {
    const result = try self.namespace.getOrPut(name);

    if (result.found_existing) {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = result.key_ptr.*.range,
            .redeclared = name.range,
        } });
    } else {
        return result.value_ptr;
    }
}

fn fail(self: *@This(), @"error": Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}

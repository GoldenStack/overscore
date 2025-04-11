const std = @import("std");
const Parser = @import("Parser.zig");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const Int = std.math.big.int.Managed;
const Error = @import("failure.zig").Error;

pub const SideEffect = union(enum) {
    /// Indicates that a value is being returned from a scope.
    @"return": *Ranged(Parser.Expr),
};

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
    return .{
        .src = src,
        .allocator = allocator,
        .namespace = TokenHashMap(*Ranged(Parser.Expr)).initContext(allocator, .{
            .src = src,
        }),
    };
}

pub fn eval_main(self: *@This(), container: *Ranged(Parser.Container)) InterpreterError!*Ranged(Parser.Expr) {
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
    _ = try self.eval_expr(result);

    // Remove from namespace
    for (container.value.decls.items) |item| {
        _ = self.namespace.remove(item.value.decl.value.name);
    }

    return result;
}

fn call_function(self: *@This(), function: *Ranged(Parser.Expr), args: *[]Ranged(Parser.Expr)) InterpreterError!*Ranged(Parser.Expr) {
    // Fully evaluate the function
    if (try self.eval_expr(function)) |result| return result.@"return";

    if (function.value != .function) @panic("Tried to call non-function");

    const func = &function.value.function;
    const params = &func.parameters.items;

    if (args.len != params.len) @panic("Wrong number of parameters");

    // Add to namespace
    for (params.*, args.*) |*param, *arg| {
        try self.define(param.value.name, arg);
    }

    defer { // Remove from namespace
        for (params.*) |param| {
            _ = self.namespace.remove(param.value.name);
        }
    }

    for (params.*, args.*) |*param, *arg| {
        if (try self.eval_expr(&param.value.value)) |_| @panic("Unexpected effect from type expression");
        if (param.value.value.value != .type) @panic("Expected type expression for parameter");

        if (!try self.expr_in_type(arg, &param.value.value.value.type)) @panic("Wrongly typed parameter!");
    }

    if (try self.eval_expr(func.@"return")) |_| @panic("Unexpected effect from return expression");
    if (func.@"return".value != .type) @panic("Expected type expression for return expression");

    const block_result = try self.eval_block(&func.body);

    // TODO: Assert types. Unit types are implied.
    if (block_result) |result| {
        return result.@"return";
    } else {
        // TODO: Nothing returned => try return unit value; allow `return;` to also imply unit value return?
        @panic("Didn't return anything");
    }
}

fn is_main(self: *@This(), decl: *Parser.Decl) bool {
    if (!std.mem.eql(u8, "main", decl.name.range.substr(self.src))) return false;

    return switch (decl.value.value) {
        .function => |function| function.parameters.items.len == 0,
        else => false,
    };
}

fn expr_in_type(self: *@This(), expr: *Ranged(Parser.Expr), @"type": *Parser.Type) !bool {
    _ = self;
    return switch (@"type".*) {
        .function => |function_type| switch (expr.value) {
            .function => |function| {
                if (function_type.parameters.items.len != function.parameters.items.len) return false;

                // TODO: Check every parameter, as well as the return type

                return true;
            },
            else => false,
        },
        .container => false, // TODO: Implement struct literals
        .distinct => false, // TODO: Figure out semantics for this
        .word => expr.value == .word,
        .type => expr.value == .type,
    };
}

fn eval_expr(self: *@This(), expr: *Ranged(Parser.Expr)) InterpreterError!?SideEffect {
    return switch (expr.value) {
        .word => null, // Words are values - nothing to evaluate
        .function => null, // TODO: Evaluate

        .type => |*@"type"| self.eval_type(@"type"),

        .call => |*call| {
            for (call.arguments.items) |*item| {
                if (try self.eval_expr(item)) |effect| return effect;
            }

            const result = try self.call_function(call.function, &call.arguments.items);
            expr.* = result.*;
            return null;
        },
        .ident => |ident| if (self.namespace.get(ident)) |value| {
            if (try self.eval_expr(value)) |effect| return effect;
            expr.* = value.*;
            return null;
        } else self.fail(.{ .unknown_identifier = ident.range }),
        .block => |*block| self.eval_block(block),
        .parentheses => |value| {
            if (try self.eval_expr(value)) |effect| return effect;
            expr.* = value.*; // "Unwrap" the value
            return null;
        },
        .@"if" => |*condition| {
            if (try self.eval_expr(condition.condition)) |effect| return effect;

            const value = &condition.condition.value;

            if (value.* != .word) @panic("Can only compare against word");

            if (value.word != 0) { // Temporary boolean measures
                if (try self.eval_expr(condition.then)) |effect| return effect;

                expr.* = condition.then.*;
            } else if (condition.@"else") |otherwise| {
                if (try self.eval_expr(otherwise)) |effect| return effect;

                expr.* = otherwise.*;
            }
            return null;
        },
        .@"return" => |value| {
            if (try self.eval_expr(value)) |effect| return effect;
            return .{ .@"return" = value };
        },
        else => @panic("todo"),
    };
}

// TODO: Block evaluates to void (unit type) until labeled blocks are added
fn eval_block(self: *@This(), block: *Ranged(Parser.Block)) InterpreterError!?SideEffect {
    for (block.value.stmts.items) |*stmt| {
        switch (stmt.value) {
            .decl => |*decl| {
                if (try self.eval_expr(&decl.value.value)) |effect| return effect;
                try self.define(decl.value.name, &decl.value.value);
            },
            .expr => |*expr| if (try self.eval_expr(expr)) |effect| return effect,
        }
    }

    for (block.value.stmts.items) |*stmt| {
        if (stmt.value == .decl) {
            _ = self.namespace.remove(stmt.value.decl.value.name);
        }
    }
    return null;
}

fn eval_type(self: *@This(), @"type": *Parser.Type) InterpreterError!?SideEffect {
    return switch (@"type".*) {
        .function => null, // TODO
        .container => null, // TODO
        .distinct => |distinct| self.eval_expr(distinct),
        .word => null, // No information; nothing to evaluate
        .type => null, // No information; nothing to evaluate
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

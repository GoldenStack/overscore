const std = @import("std");
const Parser = @import("Parser.zig");
const tokenizer = @import("tokenizer.zig");

/// The tag for types.
pub const TypeTag = enum {
    empty,
    unit,
    function,
    product,
    tagged_product,
    sum,
    tagged_sum,
    unique,
    type,
};

/// Possible fully-evaluated type expressions.
pub const Type = union(TypeTag) {
    /// The empty type. This has no inhabitants, and is thus equivalent to the
    /// concept of `noreturn`. This is equivalent to `sum {}`.
    empty,

    /// The unit type. This has exactly one inhabitant, so is equivalent to
    /// `null`, `void`, etc. This is equivalent to `product {}`.
    unit,

    /// A function that can be called. This is the only way of expressing any
    /// sort of "lazy" evaluation, because functions are not (and usually can
    /// not) be eagerly evaluated.
    ///
    /// A function signature works like this: `fn(x: i32, y: i32) x * y`.
    ///
    /// You can omit the type names for it to be coerceable to other types, but
    /// if no coercion happens, it works like Zig's `anytype`. This might be
    ///
    /// For example: `const Mul = fn(x, y) x * y;`
    ///
    function: struct {
        parameters: std.ArrayList(Type),
        @"return": *Type,
    },

    /// The product between multiple types. This is equivalent to a tuple.
    ///
    /// For example: `const Pos = product { u32, u32 }`
    product: std.ArrayList(Type),

    /// The product between multiple types, including a tag. This is equivalent
    /// to a struct.
    ///
    /// For example: `const Pos = product { x: u32, y: u32 }`
    tagged_product: std.StringHashMap(Type),

    /// The sum between multiple types. This is equivalent to a union.
    ///
    /// For example: `const Ip = sum { u32, [4]u8 }`
    sum: std.ArrayList(Type),

    /// The sum between multiple types, including a tag. This is equivalent to a
    /// tagged union.
    ///
    /// For example: `const Ip = sum { v4: u32, v6: u128 }`
    tagged_sum: std.StringHashMap(Type),

    /// A unique wrapper around a type.
    ///
    /// Since all types are, by default, equal by structure (i.e. `struct { u32
    /// } == struct { u32 }`), a unique type simply removes equality by
    /// identity, so, for example, `unique struct { u32 } != unique struct { u32
    /// }`.
    ///
    /// This does not necessarily affect coercion (i.e. `struct { 5 }` being
    /// coercible to the type `unique struct { u32 }`).
    unique: *Type,

    /// A type.
    ///
    /// When the value of an expression is a type, the type of the expression is
    /// `type`. This does lead to unsoundness in the type system due to
    /// Russell's paradox, likely the only point of unsoundness, but I don't
    /// really care.
    type,
};

/// Every possible error that can occur when compiling.
pub const Error = enum {
    ident_unknown,
    ident_redeclared,
};

/// The context for errors that occur while compiling.
pub const ErrorContext = union(Error) {
    ident_unknown: tokenizer.Token,
    ident_redeclared: struct {
        declared: tokenizer.Token,
        redeclared: tokenizer.Token,
    },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .ident_unknown => |ident| try writer.print("Unknown identifier {s}", .{ident.value}),
            .ident_redeclared => |ident| {
                try writer.print("Identifier {s} was already declared at {}", .{ ident.declared.value, ident.redeclared.start });
            },
        }
    }
};

/// The error set of errors that can occur while compiling.
pub const CompilerError = error{
    CompilerError,
    OutOfMemory,
};

allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.CompilerError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?ErrorContext = null,

names: TokenHashMap(Parser.Expr),

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
        .names = TokenHashMap(Parser.Expr).init(allocator),
    };
}

fn TokenHashMap(comptime V: type) type {
    const Context = struct {
        pub fn hash(_: @This(), token: tokenizer.Token) u64 {
            return std.hash_map.hashString(token.value);
        }
        pub fn eql(_: @This(), a: tokenizer.Token, b: tokenizer.Token) bool {
            return std.mem.eql(u8, a.value, b.value);
        }
    };

    return std.HashMap(tokenizer.Token, V, Context, std.hash_map.default_max_load_percentage);
}

fn names_block(self: *@This(), block: Parser.Block) CompilerError!void {
    for (block.stmts.items) |stmt| {
        switch (stmt) {
            .decl => |decl| {
                try self.names_expr(decl.value);

                try self.check_name(decl.name);
                try self.names.put(decl.name, decl.value);
            },
            .@"return" => |ret| try self.names_expr(ret),
        }
    }

    for (block.stmts.items) |stmt| {
        switch (stmt) {
            .decl => _ = self.names.remove(stmt.decl.name),
            else => {},
        }
    }
}

fn names_expr(self: *@This(), expr: Parser.Expr) CompilerError!void {
    switch (expr) {
        .block => |block| try self.names_block(block),
        .container => |cont| try self.names_container(cont),
        .function => |func| {
            for (func.parameters.items) |param| if (param == .tagged) {
                try self.check_name(param.tagged.name);
                try self.names.put(param.tagged.name, param.tagged.value);
            };

            if (func.body) |body| try self.names_block(body);

            for (func.parameters.items) |param| if (param == .tagged) {
                _ = self.names.remove(param.tagged.name);
            };
        },
        .call => |call| {
            try self.names_expr(call.function.*);

            for (call.arguments.items) |arg| try self.names_expr(arg);
        },
        .ident => |ident| if (!self.names.contains(ident)) return self.fail(.{ .ident_unknown = ident }),
        .number => {},
        .parentheses => |parens| try self.names_expr(parens.*),
    }
}

fn names_container(self: *@This(), container: Parser.Container) CompilerError!void {
    for (container.decls.items) |decl| {
        try self.check_name(decl.decl.name);
        try self.names.put(decl.decl.name, decl.decl.value);
    }

    for (container.decls.items) |decl| try self.names_expr(decl.decl.value);

    for (container.decls.items) |decl| _ = self.names.remove(decl.decl.name);
}

fn check_name(self: *@This(), name: tokenizer.Token) CompilerError!void {
    const ptr = self.names.getEntry(name);

    if (ptr) |existing| return self.fail(.{ .ident_redeclared = .{
        .declared = existing.key_ptr.*,
        .redeclared = name,
    } });
}

fn fail(self: *@This(), @"error": ErrorContext) error{CompilerError} {
    self.error_context = @"error";
    return error.CompilerError;
}

pub fn compile(self: *@This(), container: Parser.Container) CompilerError!void {
    try self.names_container(container);
}

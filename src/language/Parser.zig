const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

/// A container, containing any number of declarations.
pub const Container = struct {
    namespace: std.ArrayList(Declaration),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll("Container { ");

        for (self.namespace.items) |item| try writer.print("{}", .{item});

        try writer.writeAll(" }");
    }
};

/// A declaration inside a scope.
pub const Declaration = struct {
    access: Access,
    mutability: Mutability,
    name: Tokenizer.Token,
    value: Expression,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {s} \"{s}\" = {}", .{
            @tagName(self.access),
            @tagName(self.mutability),
            self.name.value,
            self.value,
        });
    }
};

/// An access modifier, either private or public.
pub const Access = enum {
    private,
    public,
};

/// A mutability modifier, either constant or variable.
pub const Mutability = enum {
    constant,
    variable,
};

/// The tag for expressions.
pub const ExpressionTag = enum {
    block,
    number,
};

/// An arbitrary expression.
pub const Expression = union(ExpressionTag) {
    block: std.ArrayList(Statement),
    number: u32,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .block => |block| {
                try writer.writeAll("{ ");

                for (block.items) |item| {
                    try writer.print("{}; ", .{item});
                }

                try writer.writeAll("}");
            },
            .number => |value| try writer.print("{}", .{value}),
        }
    }
};

/// The tag for statements.
pub const StatementTag = enum {
    mov,
    expression,
};

/// An arbitrary statement.
pub const Statement = union(StatementTag) {
    mov: struct { Expression, Expression },
    expression: Expression,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .mov => |mov| {
                try writer.print("mov {} {}", mov);
            },
            .expression => |value| try writer.print("({})", .{value}),
        }
    }
};

/// Every possible error that can occur while parsing.
pub const Error = enum {
    eof,
    expected_token,
    expected_tokens,
    number_too_large,
};

/// The context for an error that can occur while parsing.
pub const ErrorContext = union(Error) {
    eof,
    expected_token: Tokenizer.TokenType,
    expected_tokens: []const Tokenizer.TokenType,
    number_too_large: []const u8,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .eof => try writer.print("Unexpected EOF", .{}),
            .expected_token => |token| try writer.print("Expected {s}", .{@tagName(token)}),
            .expected_tokens => |tokens| {
                try writer.writeAll("Expected one of ");

                for (0.., tokens) |index, token| {
                    if (index != 0) try writer.writeAll(", ");

                    try writer.print("{s}", .{@tagName(token)});
                }
            },
            .number_too_large => |number| try writer.print("Number \"{s}\" too large to store ", .{number}),
        }
    }
};

/// The error set of errors that can occur while parsing.
pub const ParsingError = error{
    ParsingError,
    OutOfMemory,
};

tokens: Tokenizer.TokenIterator,
allocator: std.mem.Allocator,

next_token: ?Tokenizer.Token = null,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.ParsingError, this value is significant. Otherwise, it may
/// contain anything.
error_context: ?ErrorContext = null,

pub fn init(tokens: Tokenizer.TokenIterator, allocator: std.mem.Allocator) @This() {
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn read_container(self: *@This()) ParsingError!Container {
    var declarations = std.ArrayList(Declaration).init(self.allocator);

    while (self.peek()) |_| {
        try declarations.append(try self.read_declaration());
    } else |_| return .{
        .namespace = declarations,
    };
}

pub fn read_declaration(self: *@This()) ParsingError!Declaration {
    const access: Access = if (self.expect(.@"pub")) |_| .public else |_| .private;

    const mutability: Mutability = switch ((try self.expect_many(.{ .@"const", .@"var" })).type) {
        .@"const" => .constant,
        .@"var" => .variable,
        else => unreachable,
    };

    const name = try self.expect(.ident);

    _ = try self.expect(.@"=");

    const value = try self.read_expression();

    _ = try self.expect(.@";");

    return .{
        .access = access,
        .mutability = mutability,
        .name = name,
        .value = value,
    };
}

pub fn read_expression(self: *@This()) ParsingError!Expression {
    return switch ((try self.peek()).type) {
        .@"{" => {
            _ = try self.expect(.@"{");

            var statements = std.ArrayList(Statement).init(self.allocator);

            while ((try self.peek()).type != .@"}") {
                try statements.append(try self.read_statement());
            }

            _ = try self.expect(.@"}");

            return .{
                .block = statements,
            };
        },
        .number => .{ .number = try self.read_number() },
        else => self.fail(.{ .expected_tokens = &.{ .@"{", .number } }),
    };
}

pub fn read_statement(self: *@This()) ParsingError!Statement {
    return switch ((try self.peek()).type) {
        .mov => {
            _ = try self.expect(.mov);

            const left = try self.read_expression();
            const right = try self.read_expression();

            return .{ .mov = .{ left, right } };
        },
        else => {
            const expr = try self.read_expression();

            _ = try self.expect(.@";");

            return .{ .expression = expr };
        },
    };
}

pub fn read_number(self: *@This()) ParsingError!u32 {
    const token = try self.expect(.number);

    return std.fmt.parseUnsigned(u32, token.value, 10) catch self.fail(.{ .number_too_large = token.value });
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": ErrorContext) error{ParsingError} {
    self.error_context = @"error";
    return error.ParsingError;
}

/// Reads a token, expecting it to be the provided type, returning an error if
/// it's not.
pub fn expect(self: *@This(), @"type": Tokenizer.TokenType) !Tokenizer.Token {
    const token = try self.peek();

    return if (token.type == @"type")
        self.next()
    else
        self.fail(.{ .expected_token = @"type" });
}

/// Reads a token, expecting it to be one of the provided token types, returning
/// an error if it's not one of those types.
pub fn expect_many(self: *@This(), types: anytype) !Tokenizer.Token {
    const token = try self.peek();

    return if (std.mem.indexOfScalar(Tokenizer.TokenType, &types, token.type) != null)
        self.next()
    else
        return self.fail(.{ .expected_tokens = &types });
}

/// Returns the next token from the backing token iterator without advancing the
/// iterator itself. This value is cached, so peeking does not require
/// re-reading.
pub fn peek(self: *@This()) !Tokenizer.Token {
    // Return the cached token if possible
    if (self.next_token) |token| return token;

    // Load the next token and backtrack.
    // This could be structured so that next() depends on peek(), but this would
    // mean that there's always hidden and unnecessary backtracking, which is
    // not ideal.
    const token = try self.next();

    self.next_token = token;
    self.tokens.pos = token.start.pos;

    return token;
}

/// Reads the next token from the backing token iterator.
pub fn next(self: *@This()) !Tokenizer.Token {
    // Return the cached token if possible
    if (self.next_token) |token| {
        self.tokens.pos = token.end.pos;
        self.next_token = null;
        return token;
    }

    // Skip tokens until there's a non-comment
    while (self.tokens.next() orelse self.fail(.eof)) |token| {
        // Comments mean nothing for now.
        if (token.type != .comment) return token;
    } else |err| return err;
}

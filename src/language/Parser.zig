const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

/// A container, containing any number of declarations.
pub const Container = struct {
    namespace: std.ArrayList(Declaration),
};

/// A declaration inside a scope.
pub const Declaration = struct {
    access: Access,
    mutability: Mutability,
    name: Tokenizer.Token,
    value: Expression,
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
};

/// The error set of errors that can occur while parsing.
pub const ParsingError = error {
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

            return .{ .mov = .{
                left, right
            } }; 
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

    const number = std.fmt.parseUnsigned(u32, token.value, 10)
        catch return self.fail(.{ .number_too_large = token.value });

    return number;
}

pub fn fail(self: *@This(), @"error": ErrorContext) error { ParsingError } {
    self.error_context = @"error";
    return error.ParsingError;
}

pub fn expect(self: *@This(), @"type": Tokenizer.TokenType) !Tokenizer.Token {
    const token = try self.peek();

    if (token.type == @"type") {
        return self.next();
    } else {
        return self.fail(.{ .expected_token = @"type" });
    }
}

pub fn expect_many(self: *@This(), types: anytype) !Tokenizer.Token {
    const token = try self.peek();

    if (std.mem.indexOfScalar(Tokenizer.TokenType, &types, token.type) != null) {
        return self.next();
    } else {
        return self.fail(.{ .expected_tokens = &types });
    }
}

pub fn peek(self: *@This()) !Tokenizer.Token {
    if (self.next_token) |token| return token;

    const token = try self.next();

    self.next_token = token;
    self.tokens.pos = token.start.pos;

    return token;
}

pub fn next(self: *@This()) !Tokenizer.Token {
    if (self.next_token) |token| {
        self.tokens.pos = token.end.pos;
        self.next_token = null;
        return token;
    }

    while (self.tokens.next() orelse self.fail(.eof)) |token| {
        // Comments mean nothing for now.
        if (token.type != .comment) return token;
    } else |err| return err;
}

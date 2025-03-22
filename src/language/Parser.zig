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
    number,
};

/// An arbitrary expression.
pub const Expression = union(ExpressionTag) {
    number: u32,
};

/// Every possible error that can occur while parsing.
pub const Error = enum {
    eof,
    expected_token,
    expected_tokens,
};

/// The context for an error that can occur while parsing.
pub const ErrorContext = union(Error) {
    eof,
    expected_token: Tokenizer.TokenType,
    expected_tokens: []const Tokenizer.TokenType,
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

pub fn read_container(self: *@This()) !Container {
    var declarations = std.ArrayList(Declaration).init(self.allocator);

    while (true) {
        if (self.peek()) |_| {
            const declaration = try self.read_declaration();

            try declarations.append(declaration);
        } else |_| return .{
            .namespace = declarations,
        };
    }

    return self.fail(.eof);
}

pub fn read_declaration(self: *@This()) !Declaration {
    const access: Access = if (self.expect(.@"pub")) |_| .public else |_| .private;

    const mutability: Mutability = switch ((try self.expect_many(.{ .@"const", .@"var" })).type) {
        .@"const" => .constant,
        .@"var" => .variable,
        else => unreachable,
    };

    const name = try self.expect(.ident);

    _ = try self.expect(.@"=");

    const value = try self.read_expression();

    if (@as(ExpressionTag, value) == .number) _ = try self.expect(.@";");

    return .{
        .access = access,
        .mutability = mutability,
        .name = name,
        .value = value,
    };
}

pub fn read_expression(self: *@This()) !Expression {
    return .{ .number = try self.read_number() };
}

pub fn read_number(self: *@This()) !u32 {
    const token = try self.expect(.number);

    // TODO: Parsing error
    const number = try std.fmt.parseUnsigned(u32, token.value, 10);

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

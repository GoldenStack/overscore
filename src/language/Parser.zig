const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

/// A literal value. Currently constrained to being a u32.
pub const Lit = u32;

/// The tag for statements.
pub const StatementTag = enum {
    mov,
};

/// An arbitrary statement.
pub const Statement = union(StatementTag) {
    mov: struct { left: Lit, right: Lit },
};

/// The tag for the AST.
pub const AstTag = enum {
    block,
};

/// The abstract syntax tree for the language
pub const Ast = union(AstTag) {
    block: std.ArrayList(Statement),
};

/// Every possible error that can occur while parsing.
pub const Error = enum {
    eof,
    expected_token,
};

/// The context for an error that can occur while parsing.
pub const ErrorContext = union(Error) {
    eof,
    expected_token: Tokenizer.TokenType,
};

tokens: Tokenizer.TokenIterator,
allocator: std.mem.Allocator,

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

pub fn read_ast(self: *@This()) !Ast {
    return self.read_block();
}

pub fn fail(self: *@This(), comptime T: type, @"error": ErrorContext) !T {
    self.error_context = @"error";
    return error.ParsingError;
}

pub fn read_block(self: *@This()) !Ast {
    _ = try self.expect(.@"{");

    var statements = std.ArrayList(Statement).init(self.allocator);

    while (true) {
        if (self.expect(.@"}")) |_| {

            return .{ .block = statements };

        } else |_| {
            const statement = try self.read_statement();

            try statements.append(statement);
        }

    }
}

pub fn read_statement(self: *@This()) !Statement {
    _ = self;
    @panic("1");
}

pub fn expect(self: *@This(), @"type": Tokenizer.TokenType) !Tokenizer.Token {
    const token = try self.next();

    return if (token.type == @"type") token else self.fail(Tokenizer.Token, .{ .expected_token = @"type" });
}

pub fn next(self: *@This()) !Tokenizer.Token {
    while (true) {
        const token = self.tokens.next() orelse return self.fail(Tokenizer.Token, .eof);

        // Comments mean nothing for now.
        if (token.type != .comment) return token;
    }
}

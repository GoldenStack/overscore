const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

/// A literal number. Currently constrained to being a u32.
pub const Number = u32;

/// The tag for statements.
pub const StatementTag = enum {
    mov,
};

/// An arbitrary statement.
pub const Statement = union(StatementTag) {
    mov: struct { left: Number, right: Number },
};

/// The tag for the AST.
pub const AstTag = enum {
    block,
};

/// The abstract syntax tree for the language
pub const Ast = union(AstTag) {
    block: std.ArrayList(Statement),

    pub fn deinit(self: *const @This()) void {
        switch (self.*) {
            .block => |block| block.deinit(),
        }
    }
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
    _ = try self.expect(.mov);

    const left = try self.read_number();
    const right = try self.read_number();

    return .{ .mov = .{
        .left = left,
        .right = right,
    } };
}

pub fn read_number(self: *@This()) !Number {
    const token = try self.expect(.number);

    // TODO: Parsing error
    const number = try std.fmt.parseUnsigned(Number, token.value, 10);

    return number;
}

pub fn expect(self: *@This(), @"type": Tokenizer.TokenType) !Tokenizer.Token {
    const start = self.tokens.pos;

    const token = try self.next();

    if (token.type == @"type") return token;

    self.tokens.pos = start; // Backtrack
    return self.fail(Tokenizer.Token, .{ .expected_token = @"type" });
}

pub fn next(self: *@This()) !Tokenizer.Token {
    while (true) {
        const token = self.tokens.next() orelse return self.fail(Tokenizer.Token, .eof);

        // Comments mean nothing for now.
        if (token.type != .comment) return token;
    }
}

const std = @import("std");
const lex = @import("../lex.zig");

pub const Token = enum {
    // Special characters
    opening_curly_bracket,
    closing_curly_bracket,
    opening_parentheses,
    closing_parentheses,
    equals,
    semicolon,
    colon,
    comma,
    period,
    asterisk,
    period_asterisk,
    arrow,

    // Kewords
    @"pub",
    @"var",
    word,
    type,
    @"and",
    @"or",

    // General language constructs
    ident,
    symbol,
    number,
    comment,

    // Meta-tokens
    eof,

    /// Multi-character alphabetic tokens.
    pub const keywords = std.StaticStringMap(Token).initComptime(.{
        .{ "pub", .@"pub" },
        .{ "var", .@"var" },
        .{ "word", .word },
        .{ "type", .type },
        .{ "and", .@"and" },
        .{ "or", .@"or" },
    });

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.writeAll(@tagName(self));
    }
};

/// A token iterator over a source string.
pub const Tokenizer = struct {
    src: [:0]const u8,
    loc: lex.Location,

    next_token: ?lex.Ranged(Token) = null,

    /// Returns a tokenizer for the given source string. The tokenizer does not
    /// verify the context for any given token; it simply assigns meaning to
    /// each of the individual tokens in the source string.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .src = src,
            .loc = .{
                .pos = 0,
                .row = 1,
                .col = 1,
            },
        };
    }

    pub fn location(self: *@This()) lex.Location {
        return self.loc;
    }

    fn peekChar(self: *const @This()) u8 {
        return self.src[self.loc.pos];
    }

    fn nextChar(self: *@This()) u8 {
        const char = self.peekChar();
        if (char == 0) return char; // Do not advance past the end

        self.loc.pos += 1;

        if (char == '\n') {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }

        return char;
    }

    fn nextTag(self: *@This()) Token {
        return switch (self.nextChar()) {
            // Fast paths for singular character tokens
            0 => .eof,
            '{' => .opening_curly_bracket,
            '}' => .closing_curly_bracket,
            '(' => .opening_parentheses,
            ')' => .closing_parentheses,
            '=' => .equals,
            ';' => .semicolon,
            ':' => .colon,
            ',' => .comma,
            '.' => {
                if (self.peekChar() == '*') {
                    _ = self.nextChar();
                    return .period_asterisk;
                } else return .period;
            },
            '*' => .asterisk,
            '-' => {
                if (self.peekChar() == '>') {
                    _ = self.nextChar();
                    return .arrow;
                } else return .symbol;
            },

            // Less fast paths for multi-character non-alphabetic tokens
            '/' => {
                if (self.peekChar() != '/') return .symbol;

                // Skip until newline or EOF
                while (true) {
                    const token = self.nextChar();
                    if (token == '\n' or token == 0) return .comment;
                }
            },

            'a'...'z', 'A'...'Z' => {
                self.skipWhile(std.ascii.isAlphanumeric);

                return .ident;
            },

            '0'...'9' => {
                self.skipWhile(std.ascii.isDigit);

                return .number;
            },

            else => .symbol,
        };
    }

    fn skipWhile(self: *@This(), function: fn (u8) bool) void {
        while (function(self.peekChar())) _ = self.nextChar();
    }

    /// Returns the next token from this iterator without advancing it. This is
    /// cached and thus does not have to be re-parsed.
    pub fn peek(self: *@This()) lex.Ranged(Token) {
        // Return the cached token if possible
        if (self.next_token) |token| return token;

        // Load the next token and backtrack.
        // This could be structured so that next() depends on peek(), but this
        // would mean that there's always hidden and unnecessary backtracking,
        // which is not ideal.
        const start = self.location();
        const token = self.next();

        self.next_token = token;
        self.loc = start;

        return token;
    }

    /// Reads the next token from this iterator.
    pub fn next(self: *@This()) lex.Ranged(Token) {
        // Return the cached token if possible
        if (self.next_token) |token| {
            self.loc = token.range.end;
            self.next_token = null;
            return token;
        }

        // Otherwise, read through tokens until there's a non-comment.
        var token = while (true) {
            self.skipWhile(std.ascii.isWhitespace);
            const token = lex.ranged(self, nextTag);

            if (token.value != .comment) break token;
        };

        if (token.value == .ident) if (Token.keywords.get(token.range.substr(self.src))) |new_token| {
            token.value = new_token;
        };

        return token;
    }
};

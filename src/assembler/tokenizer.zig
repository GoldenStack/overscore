const std = @import("std");
const lex = @import("../lex.zig");

pub const Token = enum {
    // Instructions
    not,
    sys,
    mov,
    @"and",
    @"or",
    add,
    sub,
    mul,
    jz,
    jnz,

    word,
    bytes,
    label,
    end,

    number,
    ident,
    symbol,

    colon,

    opening_square_bracket,
    closing_square_bracket,

    comment,

    newline,
    eof,

    /// Multi-character alphabetic tokens.
    pub const keywords = std.StaticStringMap(Token).initComptime(.{
        .{ "not", .not },
        .{ "sys", .sys },
        .{ "mov", .mov },
        .{ "and", .@"and" },
        .{ "or", .@"or" },
        .{ "add", .add },
        .{ "sub", .sub },
        .{ "mul", .mul },
        .{ "jz", .jz },
        .{ "jnz", .jnz },
        .{ "word", .word },
        .{ "bytes", .bytes },
        .{ "label", .label },
        .{ "end", .end },
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
            '\n' => .newline,
            '[' => .opening_square_bracket,
            ']' => .closing_square_bracket,
            ':' => .colon,
            'a'...'z', 'A'...'Z', '-', '.' => {
                self.skipWhile(struct {
                    fn ident(char: u8) bool {
                        return switch (char) {

                            // Also allow numbers in identifiers
                            'a'...'z', 'A'...'Z', '0'...'9', '-', '.' => true,
                            else => false,
                        };
                    }
                }.ident);

                return .ident;
            },

            '#' => {
                _ = self.next();
                self.skipWhile(std.ascii.isDigit);

                switch (self.peekChar()) {
                    'd', 'b', 'x' => _ = self.nextChar(),
                    else => {},
                }

                return .number;
            },

            '/' => {
                if (self.peekChar() != '/') return .symbol;
                _ = self.nextChar();

                // Skip until newline or EOF
                while (true) {
                    const token = self.peekChar();
                    if (token == '\n' or token == 0) return .comment;
                    _ = self.nextChar(); // Consume character after so the comment doesn't consume the newline
                }
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
            self.skipWhile(struct {
                fn skip(char: u8) bool {
                    return std.ascii.isWhitespace(char) and char != '\n';
                }
            }.skip);
            const token = lex.ranged(self, nextTag);

            if (token.value != .comment) break token;
        };

        if (token.value == .ident) if (Token.keywords.get(token.range.substr(self.src))) |new_token| {
            token.value = new_token;
        };

        return token;
    }
};

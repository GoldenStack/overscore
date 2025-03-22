const std = @import("std");

/// A location in a source string. This contains an absolute position, as well
/// as its row and column.
pub const Location = struct {
    pos: usize,
    row: usize,
    col: usize,
};

/// A token type. This contains all of the possible unique meanings for tokens.
pub const TokenType = enum {
    @"{",
    @"}",
    @"(",
    @")",
    @"fn",
    @"for",
    ident,
    number,
};

/// A token. This contains a type, a raw value, and its position in the source
/// string.
pub const Token = struct {
    type: TokenType,
    value: []const u8,

    start: Location,
    end: Location,
};

/// Returns a tokenizer for the given source string. The tokenizer does not
/// verify the context for any given token; it simply assigns meaning to each of
/// the individual tokens in the source string.
pub fn tokenize(src: []const u8) TokenIterator {
    return TokenIterator.init(src);
}

/// A token iterator over a source string.
pub const TokenIterator = struct {
    src: []const u8,
    pos: usize,

    row: usize,
    col: usize,

    /// Initializes the token iterator for the given source string.
    pub fn init(src: []const u8) @This() {
        return .{
            .src = src,
            .pos = 0,

            .row = 0,
            .col = 0,
        };
    }

    fn location(self: *const @This()) Location {
        return .{
            .pos = self.pos,
            .row = self.row,
            .col = self.col,
        };
    }

    fn peek_char(self: *const @This()) ?u8 {
        if (self.pos >= self.src.len) return null else return self.src[self.pos];
    }

    fn next_char(self: *@This()) ?u8 {
        const char = self.peek_char() orelse return null;

        self.pos += 1;

        if (char == '\n') {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        return char;
    }

    fn next_token(self: *@This()) ?TokenType {
        const char = self.next_char() orelse return null;

        return switch (char) {
            // Fast paths for singular character tokens
            '{' => .@"{",
            '}' => .@"}",
            '(' => .@"(",
            ')' => .@")",

            // All other cases
            else => {
                if (std.ascii.isAlphabetic(char)) {
                    while (self.peek_char()) |c| {
                        if (std.ascii.isAlphanumeric(c)) _ = self.next_char() else break;
                    }

                    return .ident;
                } else if (std.ascii.isDigit(char)) {
                    while (self.peek_char()) |c| {
                        if (std.ascii.isDigit(c)) _ = self.next_char() else break;
                    }

                    return .number;
                } else return .ident;
            },
        };
    }

    /// Reads the next token from this iterator.
    pub fn next(self: *@This()) ?Token {
        // Skip whitespace
        while (self.peek_char()) |c| {
            if (std.ascii.isWhitespace(c)) _ = self.next_char() else break;
        } else return null;

        const start = self.location();

        const @"type" = self.next_token() orelse return null;

        const end = self.location();

        const value = self.src[start.pos..end.pos];

        // Multi-character tokens.
        // Technically this could be optimized as we already check the starting
        // character, but this doesn't particularly matter.
        const types = std.StaticStringMap(TokenType).initComptime(.{
            .{ "fn", .@"fn" },
            .{ "for", .@"for" },
        });

        return .{
            .type = types.get(value) orelse @"type",
            .value = value,

            .start = start,
            .end = end,
        };
    }
};

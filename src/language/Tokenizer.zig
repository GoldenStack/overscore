const std = @import("std");

/// A location in a source string. This contains an absolute position, as well
/// as its row and column.
pub const Location = struct {
    pos: usize,
    row: usize,
    col: usize,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("line {} column {}", .{ self.row, self.col });
    }
};

/// A token type. This contains all of the possible unique meanings for tokens.
pub const TokenType = enum {
    // Special characters
    @"{",
    @"}",
    @"(",
    @")",
    @"=",
    @";",
    @":",
    @",",

    // Kewords
    @"pub",
    @"const",
    @"var",
    unique,
    tagged,
    product,
    sum,
    @"fn",

    // General language constructs
    ident,
    number,
    comment,

    // Meta-tokens
    eof,
};

/// A token. This contains a type, a raw value, and its position in the source
/// string.
pub const Token = struct {
    type: TokenType,
    value: []const u8,

    start: Location,
    end: Location,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("TokenType.{s} \"{s}\" from {} to {}", .{
            @tagName(self.type),
            self.value,
            self.start,
            self.end,
        });
    }
};

/// Returns a tokenizer for the given source string. The tokenizer does not
/// verify the context for any given token; it simply assigns meaning to each of
/// the individual tokens in the source string.
pub fn tokenize(src: [:0]const u8) TokenIterator {
    return TokenIterator.init(src);
}

/// A token iterator over a source string.
pub const TokenIterator = struct {
    src: [:0]const u8,
    pos: usize,

    row: usize,
    col: usize,

    /// Initializes the token iterator for the given source string.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .src = src,
            .pos = 0,

            .row = 1,
            .col = 1,
        };
    }

    /// Returns the location of this tokenizer in the source string.
    pub fn location(self: *const @This()) Location {
        return .{
            .pos = self.pos,
            .row = self.row,
            .col = self.col,
        };
    }

    fn peek_char(self: *const @This()) u8 {
        return self.src[self.pos];
    }

    fn next_char(self: *@This()) u8 {
        const char = self.peek_char();
        if (char == 0) return char;

        self.pos += 1;

        if (char == '\n') {
            self.row += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        return char;
    }

    fn next_token(self: *@This()) TokenType {
        return switch (self.next_char()) {
            // Fast paths for singular character tokens
            0  => .eof,
            '{' => .@"{",
            '}' => .@"}",
            '(' => .@"(",
            ')' => .@")",
            '=' => .@"=",
            ';' => .@";",
            ':' => .@":",
            ',' => .@",",

            // Less fast paths for multi-character non-alphabetic tokens
            '/' => {
                if (self.peek_char() != '/') return .ident;

                // Skip until newline or EOF
                while (true) {
                    const token = self.next_char();
                    if (token == '\n' or token == 0) break;
                }
                return .comment;
            },

            // All other cases
            else => |char| {
                if (std.ascii.isAlphabetic(char)) {
                    while (std.ascii.isAlphanumeric(self.peek_char())) {
                        _ = self.next_char();
                    }

                    return .ident;
                } else if (std.ascii.isDigit(char)) {
                    while (std.ascii.isDigit(self.peek_char())) {
                        _ = self.next_char();
                    }

                    return .number;
                } else return .ident;
            },
        };
    }

    /// Reads the next token from this iterator.
    pub fn next(self: *@This()) Token {
        // Skip whitespace
        while (std.ascii.isWhitespace(self.peek_char())) _ = self.next_char();

        const start = self.location();

        const @"type" = self.next_token();

        const end = self.location();
 
        const value = self.src[start.pos..end.pos];

        // Multi-character alphabetic tokens.
        const types = std.StaticStringMap(TokenType).initComptime(.{
            .{ "pub", .@"pub" },
            .{ "const", .@"const" },
            .{ "var", .@"var" },
            .{ "unique", .unique },
            .{ "tagged", .tagged },
            .{ "product", .product },
            .{ "sum", .sum },
            .{ "fn", .@"fn" },
        });

        return .{
            .type = switch (@"type") {
                .ident => types.get(value) orelse @"type",
                else => @"type",
            },

            .value = value,

            .start = start,
            .end = end,
        };
    }
};

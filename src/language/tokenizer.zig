const std = @import("std");

/// A location in a source string. This contains an absolute position, as well
/// as its row and column.
pub const Location = struct {
    pos: usize,
    row: usize,
    col: usize,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("line {} column {}", .{ self.row, self.col });
    }
};

/// A token. This contains a tag, a raw value, and its position in the source
/// string.
pub const Token = struct {
    tag: Tag,
    value: []const u8,

    start: Location,
    end: Location,

    /// A token tag. This contains all of the possible unique meanings for tokens.
    pub const Tag = enum {
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
        product,
        sum,
        @"fn",
        @"return",

        // General language constructs
        ident,
        number,
        comment,

        // Meta-tokens
        eof,

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.writeAll(@tagName(self));
        }
    };

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("Token.{s} \"{s}\" from {} to {}", .{ self.tag, self.value, self.start, self.end });
    }
};

/// A token iterator over a source string.
pub const Tokenizer = struct {
    src: [:0]const u8,
    loc: Location,

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

    fn peek_char(self: *const @This()) u8 {
        return self.src[self.loc.pos];
    }

    fn next_char(self: *@This()) u8 {
        const char = self.peek_char();
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

    fn next_tag(self: *@This()) Token.Tag {
        return switch (self.next_char()) {
            // Fast paths for singular character tokens
            0 => .eof,
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

            'a'...'z', 'A'...'Z' => {
                self.skip_while(std.ascii.isAlphanumeric);

                return .ident;
            },

            '0'...'9' => {
                self.skip_while(std.ascii.isDigit);

                return .number;
            },

            else => .ident,
        };
    }

    fn skip_while(self: *@This(), function: fn (u8) bool) void {
        while (function(self.peek_char())) _ = self.next_char();
    }

    /// Reads the next token from this iterator.
    pub fn next(self: *@This()) Token {
        self.skip_while(std.ascii.isWhitespace);

        const start = self.loc;

        const tag = self.next_tag();

        const end = self.loc;

        const value = self.src[start.pos..end.pos];

        // Multi-character alphabetic tokens.
        const tags = std.StaticStringMap(Token.Tag).initComptime(.{
            .{ "pub", .@"pub" },
            .{ "const", .@"const" },
            .{ "var", .@"var" },
            .{ "unique", .unique },
            .{ "product", .product },
            .{ "sum", .sum },
            .{ "fn", .@"fn" },
            .{ "return", .@"return" },
        });

        return .{
            .tag = switch (tag) {
                .ident => tags.get(value) orelse tag,
                else => tag,
            },

            .value = value,

            .start = start,
            .end = end,
        };
    }
};

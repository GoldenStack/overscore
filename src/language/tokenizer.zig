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

pub const Range = struct {
    start: Location,
    end: Location,

    pub fn substr(self: @This(), src: []const u8) []const u8 {
        return src[self.start.pos..self.end.pos];
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("from {} to {}", .{ self.start, self.end });
    }
};

pub fn Ranged(T: type) type {
    return struct {
        range: Range,
        value: T,

        pub fn wrap(context: anytype, read_fn: anytype) MapErrorPayload(@TypeOf(read_fn), Ranged) {
            const start: Ranged(Token) = context.peek();

            const value = try read_fn(context);

            return .{
                .range = .{
                    .start = start.range.start,
                    .end = context.location(),
                },
                .value = value,
            };
        }

        pub fn swap(self: @This(), new_value: anytype) Ranged(@TypeOf(new_value)) {
            return .{
                .range = self.range,
                .value = new_value,
            };
        }

        pub fn map(self: @This(), context: anytype, map_fn: anytype) MapErrorPayload(@TypeOf(map_fn), Ranged) {
            return .{
                .range = self.range,
                .value = try map_fn(context, self.value),
            };
        }

        pub fn mapExtend(self: @This(), context: anytype, read_fn: anytype) MapErrorPayload(@TypeOf(read_fn), Ranged) {
            const value = try read_fn(context, self);

            return .{
                .range = .{
                    .start = self.range.start,
                    .end = context.location(),
                },
                .value = value,
            };
        }

        fn MapErrorPayload(mapper: type, mapping: fn (type) type) type {
            const return_type = @typeInfo(mapper).@"fn".return_type.?;

            if (@typeInfo(return_type) == .error_union) {
                const error_union = @typeInfo(return_type).error_union;

                return error_union.error_set!mapping(error_union.payload);
            } else {
                return mapping(return_type);
            }
        }

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{}", .{self.value});
        }
    };
}

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
    // TODO: asterisk_asterisk?
    plus_plus,
    arrow,

    // Kewords
    @"pub",
    @"const",
    @"var",
    word,
    type,

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
            '+' => {
                if (self.peekChar() == '+') {
                    _ = self.nextChar();
                    return .plus_plus;
                } else return .ident;
            },
            '-' => {
                if (self.peekChar() == '>') {
                    _ = self.nextChar();
                    return .arrow;
                } else return .ident;
            },

            // Less fast paths for multi-character non-alphabetic tokens
            '/' => {
                if (self.peekChar() != '/') return .ident;

                // Skip until newline or EOF
                while (true) {
                    const token = self.nextChar();
                    if (token == '\n' or token == 0) break;
                }
                return .comment;
            },

            'a'...'z', 'A'...'Z' => {
                self.skipWhile(std.ascii.isAlphanumeric);

                return .ident;
            },

            '0'...'9' => {
                self.skipWhile(std.ascii.isDigit);

                return .number;
            },

            else => .ident,
        };
    }

    fn skipWhile(self: *@This(), function: fn (u8) bool) void {
        while (function(self.peekChar())) _ = self.nextChar();
    }

    /// Reads the next token from this iterator.
    pub fn next(self: *@This()) Ranged(Token) {
        self.skipWhile(std.ascii.isWhitespace);

        const start = self.loc;
        const tag = self.nextTag();
        const end = self.loc;

        var token = Ranged(Token){
            .value = tag,
            .range = .{
                .start = start,
                .end = end,
            },
        };

        // Multi-character alphabetic tokens.
        const tags = std.StaticStringMap(Token).initComptime(.{
            .{ "pub", .@"pub" },
            .{ "const", .@"const" },
            .{ "var", .@"var" },
            .{ "word", .word },
            .{ "type", .type },
        });

        if (token.value == .ident) if (tags.get(token.range.substr(self.src))) |new_token| {
            token.value = new_token;
        };

        return token;
    }
};

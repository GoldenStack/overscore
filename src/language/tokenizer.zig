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
            const start = context.location();

            const value = try read_fn(context);

            return .{
                .range = .{
                    .start = start,
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

        pub fn map_extend(self: @This(), context: anytype, read_fn: anytype) MapErrorPayload(@TypeOf(read_fn), Ranged) {
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

            const error_union = @typeInfo(return_type).error_union;

            return error_union.error_set!mapping(error_union.payload);
        }

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{}", .{self.value});
        }
    };
}

/// A token. This contains a tag, a raw value, and its position in the source
/// string.
pub const Token = struct {
    tag: Tag,
    value: []const u8,

    range: Range,

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
        @".",

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
        try writer.print("Token.{s} \"{s}\" {}", .{ self.tag, self.value, self.range });
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
            '.' => .@".",

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

        return .{ .tag = switch (tag) {
            .ident => tags.get(value) orelse tag,
            else => tag,
        }, .value = value, .range = .{
            .start = start,
            .end = end,
        } };
    }
};

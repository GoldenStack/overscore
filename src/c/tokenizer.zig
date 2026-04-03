const std = @import("std");
const lex = @import("../lex.zig");
const failure = @import("failure.zig");

pub const Token = enum {
    // Keywords. Defined per ISO C89 6.1.1 Keywords
    auto,
    @"break",
    case,
    char,
    @"const",
    @"continue",
    default,
    do,
    double,
    @"else",
    @"enum",
    @"extern",
    float,
    @"for",
    goto,
    @"if",
    int,
    long,
    register,
    @"return",
    short,
    signed,
    sizeof,
    static,
    @"struct",
    @"switch",
    typedef,
    @"union",
    unsigned,
    void,
    @"volatile",
    @"while",

    // Token types
    identifier,
    integer_constant,
    floating_constant,
    character_constant,
    string_constant,

    // Meta-tokens
    eof,

    /// Multi-character alphabetic tokens.
    pub const keywords = std.StaticStringMap(Token).initComptime(.{
        .{ "auto", .auto },
        .{ "break", .@"break" },
        .{ "case", .case },
        .{ "char", .char },
        .{ "const", .@"const" },
        .{ "continue", .@"continue" },
        .{ "default", .default },
        .{ "do", .do },
        .{ "double", .double },
        .{ "else", .@"else" },
        .{ "enum", .@"enum" },
        .{ "extern", .@"extern" },
        .{ "float", .float },
        .{ "for", .@"for" },
        .{ "goto", .goto },
        .{ "if", .@"if" },
        .{ "int", .int },
        .{ "long", .long },
        .{ "register", .register },
        .{ "return", .@"return" },
        .{ "short", .short },
        .{ "signed", .signed },
        .{ "sizeof", .sizeof },
        .{ "static", .static },
        .{ "struct", .@"struct" },
        .{ "switch", .@"switch" },
        .{ "typedef", .typedef },
        .{ "union", .@"union" },
        .{ "unsigned", .unsigned },
        .{ "void", .void },
        .{ "volatile", .@"volatile" },
        .{ "while", .@"while" },
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
    error_context: ?failure.Error = null,

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

    fn tryChar(self: *@This(), c: u8) bool {
        if (self.peekChar() == c) {
            _ = self.nextChar();
            return true;
        } else return false;
    }

    fn tryChars(self: *@This(), cs: anytype) bool {
        inline for (cs) |c| {
            if (self.tryChar(c)) return true;
        } else return false;
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

    fn nextTag(self: *@This()) failure.Err!Token {
        const start = self.loc;

        return tag: switch (self.nextChar()) {
            0 => .eof,

            '_', 'a'...'z', 'A'...'Z' => {
                // Branch to character constant if an apostrophe is next
                if (self.tryChar('\'')) {
                    continue :tag '\'';
                } else if (self.tryChar('"')) {
                    continue :tag '"';
                }

                // Andrew Kelley is wrong and anonymous functions deserve to exist
                _ = self.skipWhile(struct {
                    fn skip(c: u8) bool {
                        return switch (c) {
                            '_', 'a'...'z', '0'...'9', 'A'...'Z' => true,
                            else => false,
                        };
                    }
                }.skip);
                return .identifier;
            },

            '0' => {
                // Parse an integer constant. Since we know it's an integer, we
                // can skip out on a lot of the complex logic used in
                // recognizing types.
                if (self.tryChars(.{ 'x', 'X' })) { // Hex
                    if (!std.ascii.isHex(self.peekChar())) return failure.fail(self, .{ .expected_hex_digits_after_hex_prefix = .{
                        .hex_prefix = .{
                            .start = start,
                            .end = self.loc,
                        },
                        .after_prefix = .{
                            .start = self.loc,
                            .end = self.loc,
                        },
                    } });
                    _ = self.skipWhile(std.ascii.isHex);
                } else { // Octal
                    _ = self.skipWhile(isOctal);
                }

                // Either suffix order (ul | lu)
                if (self.tryChars(.{ 'u', 'U' })) {
                    _ = self.tryChars(.{ 'l', 'L' });
                } else if (self.tryChars(.{ 'l', 'L' })) {
                    _ = self.tryChars(.{ 'u', 'U' });
                }

                return .integer_constant;
            },

            '.' => {
                // Parse a floating constant. Once again, since we know it's a
                // float, we can just parse it simply.
                if (!self.skipWhile(std.ascii.isDigit)) return failure.fail(self, .{ .expected_either_whole_part_or_fractional_part = .{
                    .floating_constant = .{
                        .start = start,
                        .end = self.loc,
                    },
                } });

                if (self.tryChars(.{ 'e', 'E' })) {
                    _ = self.tryChars(.{ '+', '-' });
                    _ = self.skipWhile(std.ascii.isDigit);
                }

                _ = self.tryChars(.{ 'f', 'l', 'F', 'L' });

                return .floating_constant;
            },

            '1'...'9' => {
                // Parsing should be intelligent as to exclude extraneous
                // suffixes but should also be stupid enough as to not error.
                // It may seem easier to combine floating and integer constants,
                // but to allow all C89-compliant integers, we have to know the
                // floating-point/integer status at tokenization.
                //
                // Unfortunately it would be a great idea to store the parsed
                // state here to reduce code repetition, but that would require
                // making tokens a tagged union instead of a single-byte tag
                // and would likely impact performance in all cases, although
                // admittedly speeding up number parsing and preventing the
                // aforementioned code repetition.

                var numtype: enum { unknown, int, float } = .unknown;

                _ = self.skipWhile(std.ascii.isDigit);

                // At this point, the only way this number can be signaled as
                // an integer is to have an integer suffix.

                if (numtype != .int and self.tryChar('.')) {
                    _ = self.skipWhile(std.ascii.isDigit);
                    numtype = .float;
                }

                if (numtype != .int and self.tryChars(.{ 'e', 'E' })) {
                    _ = self.tryChars(.{ '+', '-' });
                    _ = self.skipWhile(std.ascii.isDigit);
                    numtype = .float;
                }

                // Handle unambiguously integral and floating point suffixes
                // Then handle the 'any' case
                if (numtype != .int and self.tryChars(.{ 'f', 'F' })) {
                    numtype = .float;
                } else if (numtype != .float and self.tryChars(.{ 'u', 'U' })) {
                    numtype = .int;
                    _ = self.tryChars(.{ 'l', 'L' });
                } else if (self.tryChars(.{ 'l', 'L' })) { // Any suffix
                    if (numtype != .float and self.tryChars(.{ 'u', 'U' })) { // Any suffix and then int suffix
                        numtype = .int;
                    }
                }

                return switch (numtype) {
                    .float => .floating_constant,
                    .int, .unknown => .integer_constant,
                };
            },

            '\'' => {
                if (self.tryChar('\'')) return failure.fail(self, .{ .empty_character_constant = .{
                    .character_region = .{
                        .start = start,
                        .end = self.loc,
                    },
                } });

                try self.readInBandEscapedCharacters('\'');

                if (self.tryChar('\'')) {
                    return .character_constant;
                } else {
                    return failure.fail(self, .{ .unclosed_character_constant = .{
                        .character_region = .{
                            .start = start,
                            .end = self.loc,
                        },
                        .last_char = .{
                            .start = self.loc,
                            .end = self.loc,
                        },
                    } });
                }

                return .character_constant;
            },

            '"' => {
                try self.readInBandEscapedCharacters('"');

                if (self.tryChar('"')) {
                    return .string_constant;
                } else {
                    return failure.fail(self, .{ .unclosed_string_constant = .{
                        .string_region = .{
                            .start = start,
                            .end = self.loc,
                        },
                        .last_char = .{
                            .start = self.loc,
                            .end = self.loc,
                        },
                    } });
                }

                return .string_constant;
            },

            else => @panic("TODO"),
        };
    }

    fn isOctal(c: u8) bool {
        return switch (c) {
            '0'...'7' => true,
            else => false,
        };
    }

    fn readInBandEscapedCharacters(self: *@This(), close: u8) failure.Err!void {
        while (self.peekChar() != close) switch (self.peekChar()) {
            '\\' => {
                const escape_start = self.loc;
                _ = self.nextChar();

                switch (self.peekChar()) {
                    // Simple escape sequence
                    '\'', '"', '?', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v' => _ = self.nextChar(),
                    // Octal escape sequence
                    '0'...'7' => {
                        // Octal escape sequences are 1-3 chars.
                        _ = self.nextChar();

                        // Handle the next two simply
                        if (isOctal(self.peekChar())) {
                            _ = self.nextChar();
                            if (isOctal(self.peekChar())) {
                                _ = self.nextChar();
                            }
                        }
                    },
                    // Hex escape sequence
                    'x' => {
                        _ = self.nextChar();

                        if (!std.ascii.isHex(self.peekChar())) return failure.fail(self, .{ .incomplete_hex_escaped_character = .{
                            .backslash = .{
                                .start = escape_start,
                                .end = self.loc,
                            },
                            .after_backslash = .{
                                .start = self.loc,
                                .end = self.loc,
                            },
                        } });

                        _ = self.skipWhile(std.ascii.isHex);
                    },
                    // Invalid escape sequence otherwise
                    else => {
                        const after_escape = self.loc;
                        _ = self.nextChar();
                        return failure.fail(self, .{ .invalid_escape_sequence = .{
                            .escape_sequence = .{
                                .start = escape_start,
                                .end = self.loc,
                            },
                            .after_backslash = .{
                                .start = after_escape,
                                .end = self.loc,
                            },
                        } });
                    },
                }
            },
            '\n', 0 => break,
            else => _ = self.nextChar(),
        };
    }

    fn skipWhile(self: *@This(), function: fn (u8) bool) bool {
        const pos = self.loc.pos;
        while (function(self.peekChar())) _ = self.nextChar();
        return pos != self.loc.pos;
    }

    /// Returns the next token from this iterator without advancing it. This is
    /// cached and thus does not have to be re-parsed.
    pub fn peek(self: *@This()) failure.Err!lex.Ranged(Token) {
        // Return the cached token if possible
        if (self.next_token) |token| return token;

        // Load the next token and backtrack.
        // This could be structured so that next() depends on peek(), but this
        // would mean that there's always hidden and unnecessary backtracking,
        // which is not ideal.
        const start = self.location();
        const token = try self.next();

        self.next_token = token;
        self.loc = start;

        return token;
    }

    /// Reads the next token from this iterator.
    pub fn next(self: *@This()) failure.Err!lex.Ranged(Token) {
        // Return the cached token if possible
        if (self.next_token) |token| {
            self.loc = token.range.end;
            self.next_token = null;
            return token;
        }

        // Otherwise, read through tokens until there's a non-comment.
        _ = self.skipWhile(std.ascii.isWhitespace);
        var token = try lex.ranged(self, nextTag);

        if (token.value == .identifier) if (Token.keywords.get(token.range.substr(self.src))) |new_token| {
            token.value = new_token;
        };

        return token;
    }
};

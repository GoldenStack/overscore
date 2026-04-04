//! Implements the 8 translation phases described in ANSI C89 5.1.1.2.
//! These are implemented lazily, as to allow all phases to occur in one pass.
//! Each phase depends on the previous phase, querying from it when necessary.
//! They could be merged for performance, but that's something to be dealt with
//! in the future.

const std = @import("std");
const lex = @import("../lex.zig");
const failure = @import("failure.zig");

/// The preprocessing token type.
pub const PreprocessingToken = enum {
    builtin_header_name,
    custom_header_name,
    identifier,
    number,
    character_constant,
    string_constant,
    operator_or_punctuator,
    other_symbol,
    whitespace,
    comment,
    eof,
};

fn bottomPhase(self: anytype) *Phase1 {
    return if (@hasField(std.meta.Child(@TypeOf(self)), "previous_phase")) bottomPhase(&self.previous_phase) else self;
}

pub fn loc(self: anytype) lex.Location {
    return bottomPhase(self).location;
}

pub fn fail(self: anytype, @"error": failure.Error) failure.Err {
    bottomPhase(self).error_context = @"error";
    return error.CodeError;
}

pub fn lastError(self: anytype) ?failure.Error {
    return bottomPhase(self).error_context;
}

pub fn skipWhile(self: anytype, function: anytype) bool {
    const pos = loc(self).pos;
    while (function(self.peek())) _ = self.next();
    return pos != loc(self).pos;
}

/// Implements phase 1, where source characters are mapped.
/// This is also the only translation phase to store the source code string.
///
/// Mapped trigraphs:
///   ??= to #
///   ??( to [
///   ??/ to \
///   ??) to ]
///   ??' to ^
///   ??< to {
///   ??! to |
///   ??> to }
///   ??- to ~
pub const Phase1 = struct {
    src: [:0]const u8,
    location: lex.Location = .{
        .pos = 0,
        .row = 1,
        .col = 1,
    },
    error_context: ?failure.Error = null,

    /// Initializes a new phase 1 parser at the start of the source string.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .src = src,
        };
    }

    /// Peeks at the next character in the source string.
    pub fn peek(self: *const @This()) u8 {
        return self.src[self.location.pos];
    }

    /// Pops the next character in the source string.
    /// No-op if already at the end of the string.
    pub fn next(self: *@This()) u8 {
        // Read the next character
        const char = self.peek();
        if (char == 0) return char; // Do not advance past the end

        // Update the current location
        self.location.pos += 1;

        // Wrap over a newline if one was read
        if (char == '\n') {
            self.location.row += 1;
            self.location.col = 1;
        } else {
            self.location.col += 1;
        }

        // Two question marks in a row - try for a trigraph.
        if (char == '?' and self.peek() == '?') {
            // Breaks our rule of 1-character lookahead, but this is fine.
            const trigraph: ?u8 = switch (self.src[self.location.pos + 1]) {
                '=' => '#',
                '(' => '[',
                '/' => '\\',
                ')' => ']',
                '\'' => '^',
                '<' => '{',
                '!' => '|',
                '>' => '}',
                '-' => '~',
                else => null,
            };

            if (trigraph) |new_char| {
                // We could use [self.next()] for this, but we know it will
                // never trigger any logic except for increasing the position,
                // so it may as well be done here.
                self.location.pos += 2;
                return new_char;
            }
        }

        return char;
    }
};

/// Implements phase 2, where newline behaviour is checked & enforced.
/// Specifically, escaped newlines are mapped out and the file must end with a
/// newline.
pub const Phase2 = struct {
    previous_phase: Phase1,
    next_char: ?u8 = null,

    /// Initializes a new phase 2 parser.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .previous_phase = Phase1.init(src),
        };
    }

    /// Peeks at the next character from this phase.
    pub fn peek(self: *@This()) u8 {
        if (self.next_char) |char| return char;

        const start = loc(self);
        const char = self.next();

        self.next_char = char;
        self.previous_phase.location = start;

        return char;
    }

    /// Pops the next character from this phase.
    pub fn next(self: *@This()) u8 {
        const char = self.previous_phase.next();
        self.next_char = null;

        // Test for removed newline
        return if (char == '\\' and self.consume('\n')) self.next() else char;
    }

    /// Consumes the expected character if found. Otherwise, returns false.
    pub fn consume(self: *@This(), c: u8) bool {
        if (self.peek() == c) {
            _ = self.next();
            return true;
        } else return false;
    }

    /// Consumes any of the provided characters if found; false if not.
    pub fn consumeMany(self: *@This(), cs: anytype) bool {
        inline for (cs) |c| {
            if (self.consume(c)) return true;
        } else return false;
    }
};

/// Implements phase 3, where preprocessing tokenization occurs and comments are
/// replaced.
pub const Phase3 = struct {
    previous_phase: Phase2,
    next_token: ?PreprocessingToken = null,

    /// Initializes a new phase 3 parser.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .previous_phase = Phase2.init(src),
        };
    }

    /// Peeks at the next token from this phase.
    pub fn peek(self: *@This()) failure.Err!PreprocessingToken {
        if (self.next_token) |token| return token;

        const start = loc(self);
        const token = try self.next();

        self.next_token = token;
        self.previous_phase.loc = start;

        return token;
    }

    fn isOctal(c: u8) bool {
        return switch (c) {
            '0'...'7' => true,
            else => false,
        };
    }

    fn parseEscapeSequence(self: *@This()) failure.Err!void {
        const before_escape = loc(self);
        _ = self.previous_phase.consume('\\');
        const after_escape = loc(self);

        switch (self.previous_phase.next()) {
            // Simple escape sequence
            '\'', '"', '?', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v' => {},
            // Octal escape sequence
            '0'...'7' => {
                // Octal escape sequences are 1-3 chars.
                // Handle the next two simply.
                if (isOctal(self.previous_phase.peek())) {
                    _ = self.previous_phase.next();
                    if (isOctal(self.previous_phase.peek())) {
                        _ = self.previous_phase.next();
                    }
                }
            },
            // Hex escape sequence
            'x' => {
                const found_hex = skipWhile(&self.previous_phase, std.ascii.isHex);

                if (!found_hex) return fail(self, .{ .incomplete_hex_escaped_character = .{
                    .backslash = before_escape.to(loc(self)),
                    .after_backslash = loc(self).to(loc(self)),
                } });
            },
            // Invalid escape sequence otherwise
            else => return fail(self, .{ .invalid_escape_sequence = .{
                .escape_sequence = before_escape.to(loc(self)),
                .after_backslash = after_escape.to(loc(self)),
            } }),
        }
    }

    /// Returns the next token from this phase.
    pub fn next(self: *@This(), comptime header_name: bool) failure.Err!PreprocessingToken {
        const start = loc(self);

        return tag: switch (self.previous_phase.next()) {
            ' ', '\t'...'\r' => {
                _ = skipWhile(&self.previous_phase, std.ascii.isWhitespace);
                return .whitespace;
            },

            '_', 'a'...'z', 'A'...'Z' => {
                // Andrew Kelley is wrong and anonymous functions deserve to exist
                _ = skipWhile(&self.previous_phase, struct {
                    fn skip(c: u8) bool {
                        return switch (c) {
                            '_', 'a'...'z', '0'...'9', 'A'...'Z' => true,
                            else => false,
                        };
                    }
                }.skip);
                return .identifier;
            },

            '.' => {
                if (std.ascii.isDigit(self.previous_phase.peek())) {
                    continue :tag self.previous_phase.next();
                } else return .operator_or_punctuator;
            },

            '0'...'9' => { // . moves here if a digit is next
                while (true) switch (self.previous_phase.peek()) {
                    '0'...'9', '_', 'a'...'z', 'A'...'Z', '.' => {
                        const c = self.previous_phase.next();
                        if (c == 'e' or c == 'E') {
                            _ = self.previous_phase.consumeMany(.{ '+', '-' });
                        }
                    },
                    else => return .number,
                };
            },

            '\'' => {
                if (self.previous_phase.consume('\'')) return fail(self, .{ .empty_character_constant = .{
                    .character_region = start.to(loc(self)),
                } });

                while (!self.previous_phase.consume('\'')) switch (self.previous_phase.peek()) {
                    '\\' => try self.parseEscapeSequence(),
                    '\n', 0 => return fail(self, .{ .unclosed_character_constant = .{
                        .character_region = start.to(loc(self)),
                        .last_char = loc(self).to(loc(self)),
                    } }),
                    else => _ = self.previous_phase.next(),
                } else return .character_constant;
            },

            '"' => {
                if (header_name) {
                    if (self.previous_phase.consume('"')) return fail(self, .{ .empty_custom_header_name = .{
                        .header_region = start.to(loc(self)),
                    } });

                    while (true) switch (self.previous_phase.next()) {
                        '"' => return .custom_header_name,
                        '\n', 0 => return fail(self, .{ .unclosed_custom_header_name = .{
                            .header_region = start.to(loc(self)),
                            .after_header_region = loc(self).to(loc(self)),
                        } }),
                        else => continue,
                    };
                }

                while (!self.previous_phase.consume('"')) switch (self.previous_phase.peek()) {
                    '\\' => try self.parseEscapeSequence(),
                    '\n', 0 => return fail(self, .{ .unclosed_string_constant = .{
                        .string_region = start.to(loc(self)),
                        .last_char = loc(self).to(loc(self)),
                    } }),
                    else => _ = self.previous_phase.next(),
                } else return .string_constant;
            },

            '<' => {
                if (header_name) {
                    if (self.previous_phase.consume('>')) return fail(self, .{ .empty_builtin_header_name = .{
                        .header_region = start.to(loc(self)),
                    } });

                    while (true) switch (self.previous_phase.next()) {
                        '>' => return .builtin_header_name,
                        '\n', 0 => return fail(self, .{ .unclosed_builtin_header_name = .{
                            .header_region = start.to(loc(self)),
                            .after_header_region = loc(self).to(loc(self)),
                        } }),
                        else => continue,
                    };
                }

                _ = self.previous_phase.consume('<');
                _ = self.previous_phase.consume('=');
                return .operator_or_punctuator;
            },

            '>' => {
                _ = self.previous_phase.consume('>');
                _ = self.previous_phase.consume('=');
                return .operator_or_punctuator;
            },

            '-' => {
                if (!self.previous_phase.consume('>')) {
                    if (!self.previous_phase.consume('-')) {
                        _ = self.previous_phase.consume('=');
                    }
                }

                return .operator_or_punctuator;
            },

            '&', '+', '|' => |c| {
                if (!self.previous_phase.consume('=')) {
                    _ = self.previous_phase.consume(c);
                }
                return .operator_or_punctuator;
            },

            '#' => {
                _ = self.previous_phase.consume('#');
                return .operator_or_punctuator;
            },

            '=', '*', '!', '/', '%', '^' => {
                if (self.previous_phase.consume('*')) {
                    while (true) switch (self.previous_phase.next()) {
                        '*' => if (self.previous_phase.consume('/')) return .comment,
                        0 => return fail(self, .{
                            .unclosed_comment = .{
                                .comment_region = start.to(loc(self)),
                                .end = loc(self).to(loc(self)),
                            },
                        }),
                        else => continue,
                    };
                }

                _ = self.previous_phase.consume('=');
                return .operator_or_punctuator;
            },

            // One unambiguous character
            '[', ']', '(', ')', '{', '}', '~', '?', ':', ';', ',' => .operator_or_punctuator,

            0 => .eof,
            else => .other_symbol,
        };
    }
};

//! Implements the 8 translation phases described in ANSI C89 5.1.1.2.
//! These are implemented lazily, as to allow all phases to occur in one pass.
//! Each phase depends on the previous phase, querying from it when necessary.
//! They could be merged for performance, but that's something to be dealt with
//! in the future.

const std = @import("std");
const lex = @import("../lex.zig");
const failure = @import("failure.zig");

/// Preprocessing-related structures (tokens, AST);
pub const Preprocessing = struct {
    /// The preprocessing token type.
    pub const Token = enum {
        // Tokens with multiple states, requiring usage of the token buffer.
        builtin_header_name,
        custom_header_name,
        identifier,
        string_literal,
        character_constant,
        number,

        // Unit tokens with specific meaning
        newline,
        whitespace,
        eof,

        op_period,
        op_period_period_period,
        op_lt,
        op_lt_lt,
        op_lt_eq,
        op_lt_lt_eq,
        op_gt,
        op_gt_gt,
        op_gt_eq,
        op_gt_gt_eq,
        op_arrow,
        op_minus,
        op_minus_minus,
        op_minus_eq,
        op_and,
        op_and_and,
        op_and_eq,
        op_plus,
        op_plus_plus,
        op_plus_eq,
        op_or,
        op_or_or,
        op_or_eq,
        op_octothorpe,
        op_octothorpe_octothorpe,
        op_eq,
        op_eq_eq,
        op_times,
        op_times_eq,
        op_div,
        op_div_eq,
        op_bang,
        op_bang_eq,
        op_percent,
        op_percent_eq,
        op_caret,
        op_caret_eq,
        op_opening_square_bracket,
        op_closing_square_bracket,
        op_opening_parentheses,
        op_closing_parentheses,
        op_opening_curly_bracket,
        op_closing_curly_bracket,
        op_tilde,
        op_question_mark,
        op_colon,
        op_semicolon,
        op_comma,
    };

    /// A flat line-based data structure for preprocessing. This omits any
    /// structure for performance reasons.
    pub const Line = union(enum) {
        tokens: ?std.ArrayList(Token),
        directive: union(enum) {
            @"if": ConstantExpression,
            elif: ConstantExpression,
            @"else": void,
            endif: void,
            ifdef: Identifier,
            ifndef: Identifier,
            include: std.ArrayList(Token),
            define: struct {
                params: ??std.ArrayList(Identifier),
                replacement_list: ?std.ArrayList(Token),
            },
            undef: Identifier,
            line: std.ArrayList(Token),
            @"error": ?std.ArrayList(Token),
            pragma: ?std.ArrayList(Token),
            empty,
        },
        eof,
    };

    /// TODO: Replace with some actual structure
    pub const ConstantExpression = struct {};
    /// TODO: Replace with some actual structure
    pub const Identifier = struct {};
};

fn bottomPhase(self: anytype) *Phase1 {
    return if (@hasField(std.meta.Child(@TypeOf(self)), "previous_phase")) bottomPhase(&self.previous_phase) else self;
}

pub fn loc(self: anytype) lex.Location {
    return bottomPhase(self).location;
}

pub fn resolve(self: anytype, range: lex.Range) []const u8 {
    return range.substr(bottomPhase(self).src);
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
        if (char == '?' and self.peek() == '?') trigraph: {
            // Breaks our rule of 1-character lookahead, but this is fine.
            const trigraph: u8 = switch (self.src[self.location.pos + 1]) {
                '=' => '#',
                '(' => '[',
                '/' => '\\',
                ')' => ']',
                '\'' => '^',
                '<' => '{',
                '!' => '|',
                '>' => '}',
                '-' => '~',
                else => break :trigraph,
            };

            // We could use [self.next()] for this, but we know it will
            // never trigger any logic except for increasing the position,
            // so it may as well be done here.
            self.location.pos += 2;
            return trigraph;
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

    token_buffer: [1024]u8 = undefined,
    token_buffer_pos: usize = 0,

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
        const char = self.next(false);

        self.next_char = char;
        self.previous_phase.location = start;

        return char;
    }

    /// Pops the next character from this phase.
    pub fn next(self: *@This(), comptime save_in_buffer: bool) u8 {
        const char = self.previous_phase.next();
        self.next_char = null;

        if (char == '\\' and self.previous_phase.peek() == '\n') {
            _ = self.previous_phase.next();
            return self.next(save_in_buffer);
        } else {
            if (save_in_buffer) {
                if (self.token_buffer_pos >= 1024) {
                    @panic("Filled up token buffer");
                } else {
                    self.token_buffer[self.token_buffer_pos] = char;
                    self.token_buffer_pos += 1;
                }
            }

            return char;
        }
    }

    /// Consumes the expected character if found. Otherwise, returns false.
    pub fn consume(self: *@This(), c: u8, comptime save_in_buffer: bool) bool {
        if (self.peek() == c) {
            _ = self.next(save_in_buffer);
            return true;
        } else return false;
    }

    /// Consumes any of the provided characters if found; false if not.
    pub fn consumeMany(self: *@This(), cs: anytype, comptime save_in_buffer: bool) bool {
        inline for (cs) |c| {
            if (self.consume(c, save_in_buffer)) return true;
        } else return false;
    }

    pub fn skipWhile(self: anytype, function: anytype, comptime save_in_buffer: bool) bool {
        const pos = loc(self).pos;
        while (function(self.peek())) _ = self.next(save_in_buffer);
        return pos != loc(self).pos;
    }
};

/// Implements phases 3 and 4, where preprocessing tokenization occurs (phase 3)
/// and where preprocessing directives are expanded (phase 4).
pub const Phase3 = struct {
    previous_phase: Phase2,
    next_token: ?Preprocessing.Token = null,

    /// Initializes a new phase 3 parser.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .previous_phase = Phase2.init(src),
        };
    }

    /// Peeks at the next token from this phase.
    pub fn peek(self: *@This()) failure.Err!Preprocessing.Token {
        if (self.next_token) |token| return token;

        const start = loc(self);
        const token = try self.next(false);

        self.next_token = token;
        self.previous_phase.previous_phase.location = start;

        return token;
    }

    fn isOctal(c: u8) bool {
        return switch (c) {
            '0'...'7' => true,
            else => false,
        };
    }

    fn readUntilNewlineOrEOF(self: *@This()) lex.Location {
        while (true) switch (self.previous_phase.peek()) {
            '\n', '0' => {
                const end = loc(self);
                if (!self.previous_phase.consumeMany(.{ '\n', '0' }, false)) unreachable;

                return end;
            },
            else => _ = self.previous_phase.next(false),
        };
    }

    // TODO: Consider parsing escapes inline
    fn parseEscapeSequence(self: *@This(), comptime save_in_buffer: bool) failure.Err!void {
        if (!save_in_buffer) @compileError("This should always store data");

        const before_escape = loc(self);
        _ = self.previous_phase.consume('\\', save_in_buffer);
        const after_escape = loc(self);

        switch (self.previous_phase.next(save_in_buffer)) {
            // Simple escape sequence
            '\'', '"', '?', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v' => {},
            // Octal escape sequence
            '0'...'7' => {
                // Octal escape sequences are 1-3 chars.
                // Handle the next two simply.
                if (isOctal(self.previous_phase.peek())) {
                    _ = self.previous_phase.next(save_in_buffer);
                    if (isOctal(self.previous_phase.peek())) {
                        _ = self.previous_phase.next(save_in_buffer);
                    }
                }
            },
            // Hex escape sequence
            'x' => {
                const found_hex = self.previous_phase.skipWhile(std.ascii.isHex, save_in_buffer);

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
    pub fn next(self: *@This(), comptime header_name: bool) failure.Err!Preprocessing.Token {
        const start = loc(self);
        // TODO: Check if this affects performance at all.
        self.previous_phase.token_buffer_pos = 0;

        return tag: switch (self.previous_phase.next(true)) {
            '\n' => .newline,

            ' ', '\t', '\x0b', '\x0c', '\r' => {
                // Andrew Kelley is wrong about anonymous functions.
                _ = self.previous_phase.skipWhile(struct {
                    fn check(c: u8) bool {
                        return switch (c) {
                            ' ', '\t', '\x0b', '\x0c', '\r' => true,
                            else => false,
                        };
                    }
                }.check, false);
                return .whitespace;
            },

            '_', 'a'...'z', 'A'...'Z' => {
                while (true) switch (self.previous_phase.peek()) {
                    '_', 'a'...'z', '0'...'9', 'A'...'Z' => _ = try self.next(true),
                    else => return .identifier,
                };
            },

            '.' => {
                if (std.ascii.isDigit(self.previous_phase.peek())) {
                    continue :tag self.previous_phase.next(true);
                } else if (self.previous_phase.consume('.', false)) {
                    const pos = loc(self);
                    if (self.previous_phase.consume('.', false)) {
                        return .op_period_period_period;
                    } else return fail(self, .{ .period_period_is_invalid = .{
                        .after_last_period = loc(self).to(loc(self)),
                        .last_period = pos.to(loc(self)),
                        .region = start.to(loc(self)),
                    } });
                } else return .op_period;
            },

            '0'...'9' => { // . moves here if a digit is next
                while (true) switch (self.previous_phase.peek()) {
                    '0'...'9', '_', 'a'...'z', 'A'...'Z', '.' => {
                        const c = self.previous_phase.next(true);
                        if (c == 'e' or c == 'E') {
                            _ = self.previous_phase.consumeMany(.{ '+', '-' }, true);
                        }
                    },
                    else => return .number,
                };
            },

            '\'' => {
                if (self.previous_phase.consume('\'', true)) return fail(self, .{ .empty_character_constant = .{
                    .character_region = start.to(loc(self)),
                } });

                while (!self.previous_phase.consume('\'', true)) switch (self.previous_phase.peek()) {
                    '\\' => try self.parseEscapeSequence(true),
                    '\n', 0 => return fail(self, .{ .unclosed_character_constant = .{
                        .character_region = start.to(loc(self)),
                        .last_char = loc(self).to(loc(self)),
                    } }),
                    else => _ = self.previous_phase.next(true),
                } else return .character_constant;
            },

            '"' => {
                if (header_name) {
                    if (self.previous_phase.consume('"', true)) return fail(self, .{ .empty_custom_header_name = .{
                        .header_region = start.to(loc(self)),
                    } });

                    while (true) switch (self.previous_phase.next(true)) {
                        '"' => return .custom_header_name,
                        '\n', 0 => return fail(self, .{ .unclosed_custom_header_name = .{
                            .header_region = start.to(loc(self)),
                            .after_header_region = loc(self).to(loc(self)),
                        } }),
                        else => continue,
                    };
                }

                while (!self.previous_phase.consume('"', true)) switch (self.previous_phase.peek()) {
                    '\\' => try self.parseEscapeSequence(true),
                    '\n', 0 => return fail(self, .{ .unclosed_string_constant = .{
                        .string_region = start.to(loc(self)),
                        .last_char = loc(self).to(loc(self)),
                    } }),
                    else => _ = self.previous_phase.next(true),
                } else return .string_literal;
            },

            '<' => {
                if (header_name) {
                    if (self.previous_phase.consume('>', true)) return fail(self, .{ .empty_builtin_header_name = .{
                        .header_region = start.to(loc(self)),
                    } });

                    while (true) switch (self.previous_phase.next(true)) {
                        '>' => return .builtin_header_name,
                        '\n', 0 => return fail(self, .{ .unclosed_builtin_header_name = .{
                            .header_region = start.to(loc(self)),
                            .after_header_region = loc(self).to(loc(self)),
                        } }),
                        else => continue,
                    };

                    comptime unreachable;
                }

                if (self.previous_phase.consume('<', false)) {
                    if (self.previous_phase.consume('=', false)) {
                        return .op_lt_lt_eq;
                    } else {
                        return .op_lt_lt;
                    }
                } else if (self.previous_phase.consume('=', false)) {
                    return .op_lt_eq;
                } else {
                    return .op_lt;
                }
            },

            '>' => if (self.previous_phase.consume('>', false)) {
                if (self.previous_phase.consume('=', false)) {
                    return .op_gt_gt_eq;
                } else {
                    return .op_gt_gt;
                }
            } else if (self.previous_phase.consume('=', false)) {
                return .op_gt_eq;
            } else {
                return .op_gt;
            },

            '&' => if (self.previous_phase.consume('&', false)) {
                return .op_and_and;
            } else if (self.previous_phase.consume('=', false)) {
                return .op_and_eq;
            } else {
                return .op_and;
            },

            '|' => if (self.previous_phase.consume('|', false)) {
                return .op_or_or;
            } else if (self.previous_phase.consume('=', false)) {
                return .op_or_eq;
            } else {
                return .op_or;
            },

            '-' => if (self.previous_phase.consume('>', false)) {
                return .op_arrow;
            } else if (self.previous_phase.consume('-', false)) {
                return .op_minus_minus;
            } else if (self.previous_phase.consume('=', false)) {
                return .op_minus_eq;
            } else {
                return .op_minus;
            },

            '+' => if (self.previous_phase.consume('+', false)) {
                return .op_plus_plus;
            } else if (self.previous_phase.consume('=', false)) {
                return .op_plus_eq;
            } else {
                return .op_plus;
            },

            '/' => {
                if (self.previous_phase.consume('*', false)) {
                    while (true) switch (self.previous_phase.next(false)) {
                        '*' => if (self.previous_phase.consume('/', false)) return .whitespace,
                        0 => return fail(self, .{
                            .unclosed_comment = .{
                                .comment_region = start.to(loc(self)),
                                .end = loc(self).to(loc(self)),
                            },
                        }),
                        else => continue,
                    };
                } else if (self.previous_phase.consume('=', false)) {
                    return .op_div_eq;
                } else {
                    return .op_div;
                }
            },

            '=' => if (self.previous_phase.consume('=', false)) .op_eq_eq else .op_eq,
            '*' => if (self.previous_phase.consume('=', false)) .op_times_eq else .op_times,
            '!' => if (self.previous_phase.consume('=', false)) .op_bang_eq else .op_bang,
            '%' => if (self.previous_phase.consume('=', false)) .op_percent_eq else .op_percent,
            '^' => if (self.previous_phase.consume('=', false)) .op_caret_eq else .op_caret,

            '#' => if (self.previous_phase.consume('#', false)) .op_octothorpe_octothorpe else {
                try self.readDirective(start);
                return self.next(header_name);
            },

            '[' => .op_opening_square_bracket,
            ']' => .op_closing_square_bracket,
            '(' => .op_opening_parentheses,
            ')' => .op_closing_parentheses,
            '{' => .op_opening_curly_bracket,
            '}' => .op_closing_curly_bracket,
            '~' => .op_tilde,
            '?' => .op_question_mark,
            ':' => .op_colon,
            ';' => .op_semicolon,
            ',' => .op_comma,

            0 => .eof,
            else => fail(self, .{ .unexpected_character = .{
                .unexpected = start.to(loc(self)),
            } }),
        };
    }

    fn readDirective(self: *@This(), start: lex.Location) !void {
        const Directive = std.meta.Tag(@FieldType(Preprocessing.Line, "directive"));

        const directive_start = loc(self);

        const directive: Directive = if (self.previous_phase.consume('\n', false)) blk: {
            break :blk .empty;
        } else blk: {
            const options: []const Directive = &.{
                .@"if",
                .elif,
                .@"else",
                .endif,
                .ifdef,
                .ifndef,
                .include,
                .define,
                .undef,
                .line,
                .@"error",
                .pragma,
            };

            break :blk self.requireEnum(Directive, options) orelse return fail(self, .{ .invalid_preprocessing_directive = .{
                .directive = directive_start.to(loc(self)),
            } });
        };

        const directive_end = loc(self);

        _ = directive_end;

        return switch (directive) {
            .@"if" => @panic("TODO: Handle if"),
            .elif => @panic("TODO: Handle elif"),
            .@"else" => @panic("TODO: Handle else"),
            .endif => @panic("TODO: Handle endif"),
            .ifdef => @panic("TODO: Handle ifdef"),
            .ifndef => @panic("TODO: Handle ifndef"),
            .include => @panic("TODO: Handle include"),
            .define => @panic("TODO: Handle define"),
            .undef => @panic("TODO: Handle undef"),
            .line => @panic("TODO: Handle line"),

            // Error on the error directive
            .@"error" => fail(self, .{ .error_directive = .{
                .message = start.to(self.readUntilNewlineOrEOF()),
            } }),

            // Error when pragmas are set
            .pragma => fail(self, .{ .pragmas_are_unhandled = .{
                .pragma = start.to(self.readUntilNewlineOrEOF()),
            } }),

            // If empty, do nothing
            .empty => {},
        };
    }

    /// An option for the comptime algorithm to pick - contains an enum variant
    /// and the remaining string of the variant.
    fn Option(Variant: type) type {
        return struct {
            variant: Variant,
            value: []const u8,
        };
    }

    /// A character entry, containing a key and the list of variants that map to
    /// it.
    fn Entry(Variant: type) type {
        return struct { u8, []const Option(Variant) };
    }

    /// Initializes a list of variants for the given options, all containing
    /// empty values.
    fn initVariants(comptime Variant: type, comptime options: []const Option(Variant)) []Entry(Variant) {
        comptime var prefixes: []const u8 = &.{};

        for (options) |option| {
            for (prefixes) |prefix| {
                if (prefix == option.value[0]) break;
            } else prefixes = prefixes ++ .{option.value[0]};
        }

        comptime var map: [prefixes.len]Entry(Variant) = undefined;

        for (0.., prefixes) |index, prefix| {
            map[index] = .{
                prefix, &.{},
            };
        }

        return &map;
    }

    /// Assembles, from a list of options, a map from each character to the list
    /// of options with the (stripped) character that prefixes it.
    fn stripPrefixMap(comptime Variant: type, comptime options: []const Option(Variant)) []const Entry(Variant) {
        comptime var map: []Entry(Variant) = initVariants(Variant, options);

        for (options) |option| {
            const head = option.value[0];

            const tail: Option(Variant) = .{
                .variant = option.variant,
                .value = option.value[1..],
            };

            for (0.., map) |index, item| {
                if (item[0] == head) {
                    map[index][1] = item[1] ++ .{tail};
                    break;
                }
            } else unreachable;
        }

        return map;
    }

    /// Requires one of the list of variants from the given array of variants.
    fn requireStrings(self: *@This(), comptime Variant: type, comptime options: []const Option(Variant)) ?Variant {
        // Handle and return zero-length strings, signaling a terminal
        // This should be checked first to ensure minimal recursion.
        inline for (options) |option| {
            if (comptime option.value.len == 0) return option.variant;
        }

        // Next, assemble the (comptime) map of the next strings to check.
        // This must be done in batches instead of iteratively because we need
        // to branch as minimally as possible for correctness reasons.
        const map = comptime stripPrefixMap(Variant, options);

        const c = self.previous_phase.next(false);

        inline for (map) |entry| {
            const key, const values = entry;

            if (c == key) {
                return self.requireStrings(Variant, values);
            }
        }

        // If nothing could be fully matched and nothing could be partially
        // matched, we just fail.
        return null;
    }

    /// Given a list of enum variants, requires that one of them is next.
    pub fn requireEnum(self: *@This(), comptime Variant: type, comptime options: []const Variant) ?Variant {
        comptime var options_flat: []const Option(Variant) = &.{};

        comptime for (options) |option| {
            options_flat = options_flat ++ .{Option(Variant){
                .variant = option,
                .value = @tagName(option),
            }};
        };

        return self.requireStrings(Variant, options_flat);
    }
};

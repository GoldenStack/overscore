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
};

/// Implements phase 3, where preprocessing tokenization occurs and comments are
/// replaced.
pub const Phase3 = struct {
    previous_phase: Phase2,
    next_token: ?u8 = null,

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

    /// Returns the next token from this phase.
    pub fn next(self: *@This()) failure.Err!PreprocessingToken {
        const start = loc(self);

        return switch (self.previous_phase.next()) {
            '<' => {
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
            },
            '"' => {
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
            },
            0 => .eof,
            else => @panic("TODO"),
        };
    }
};

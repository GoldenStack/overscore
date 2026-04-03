//! Implements the 8 translation phases described in ANSI C89 5.1.1.2.
//! These are implemented lazily, as to allow all phases to occur in one pass.
//! Each phase depends on the previous phase, querying from it when necessary.

const std = @import("std");
const lex = @import("../lex.zig");
const failure = @import("failure.zig");

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
    loc: lex.Location,

    /// Initializes a new phase 1 parser at the start of the source string.
    pub fn init(src: [:0]const u8) @This() {
        return .{
            .src = src,
            .loc = .{
                .pos = 0,
                .row = 0,
                .col = 0,
            },
        };
    }

    /// Returns the location of this phase.
    pub fn location(self: *const @This()) lex.Location {
        return self.loc;
    }

    /// Peeks at the next character in the source string.
    pub fn peek(self: *const @This()) u8 {
        return self.src[self.loc.pos];
    }

    /// Pops the next character in the source string.
    /// No-op if already at the end of the string.
    pub fn next(self: *@This()) u8 {
        // Read the next character
        const char = self.peek();
        if (char == 0) return char; // Do not advance past the end

        // Update the current location
        self.loc.pos += 1;

        // Wrap over a newline if one was read
        if (char == '\n') {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }

        // Two question marks in a row - try for a trigraph.
        if (char == '?' and self.peek() == '?') {

            // Breaks our rule of 1-character lookahead, but this is fine.
            const trigraph: ?u8 = switch (self.src[self.loc.pos + 1]) {
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
                self.loc.pos += 2;
                return new_char;
            }
        }

        return char;
    }
};

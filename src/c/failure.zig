const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const lex = @import("../lex.zig");
const err = @import("../err.zig");
const Ranged = lex.Ranged;
const Range = lex.Range;

pub const Err = error{CodeError};

pub fn fail(self: anytype, @"error": Error) Err {
    self.error_context = @"error";
    return error.CodeError;
}

pub const Error = union(enum) {
    expected_hex_digits_after_hex_prefix: struct {
        hex_prefix: Range,
        after_prefix: Range,
    },

    period_period_is_invalid: struct {
        region: Range,
        last_period: Range,
        after_last_period: Range,
    },

    unclosed_character_constant: struct {
        character_region: Range,
        last_char: Range,
    },

    empty_character_constant: struct {
        character_region: Range,
    },

    unclosed_string_constant: struct {
        string_region: Range,
        last_char: Range,
    },

    incomplete_hex_escaped_character: struct {
        backslash: Range,
        after_backslash: Range,
    },

    invalid_escape_sequence: struct {
        escape_sequence: Range,
        after_backslash: Range,
    },

    unexpected_character: struct {
        unexpected: Range,
    },

    pub fn display(self: @This(), filename: []const u8, src: []const u8, writer: anytype) !void {
        switch (self) {
            .expected_hex_digits_after_hex_prefix => |e| {
                try err.prefix(filename, e.hex_prefix, .err, writer);
                try writer.writeAll("expected hexadecimal digits after hexadecimal number prefix\n" ++ err.Unbold);
                try err.pointTo(src, e.hex_prefix, writer);

                try err.prefix(filename, e.after_prefix, .note, writer);
                try writer.writeAll("try adding hex digits here\n" ++ err.Unbold);
                try err.pointTo(src, e.after_prefix, writer);
            },

            .period_period_is_invalid => |e| {
                try err.prefix(filename, e.region, .err, writer);
                try writer.writeAll(".. is not a valid operator; did you mean . or ...?\n" ++ err.Unbold);
                try err.pointTo(src, e.region, writer);

                try err.prefix(filename, e.last_period, .note, writer);
                try writer.writeAll("you could try either removing this period..\n" ++ err.Unbold);
                try err.pointTo(src, e.last_period, writer);

                try err.prefix(filename, e.after_last_period, .note, writer);
                try writer.writeAll("..or adding a period here\n" ++ err.Unbold);
                try err.pointTo(src, e.after_last_period, writer);
            },

            .unclosed_character_constant => |e| {
                try err.prefix(filename, e.character_region, .err, writer);
                try writer.writeAll("unclosed character constant\n" ++ err.Unbold);
                try err.pointTo(src, e.character_region, writer);

                try err.prefix(filename, e.last_char, .note, writer);
                try writer.writeAll("try adding an apostrophe here or at an earlier position\n" ++ err.Unbold);
                try err.pointTo(src, e.last_char, writer);
            },

            .empty_character_constant => |e| {
                try err.prefix(filename, e.character_region, .err, writer);
                try writer.writeAll("empty character constant\n" ++ err.Unbold);
                try err.pointTo(src, e.character_region, writer);
            },

            .unclosed_string_constant => |e| {
                try err.prefix(filename, e.string_region, .err, writer);
                try writer.writeAll("unclosed string constant\n" ++ err.Unbold);
                try err.pointTo(src, e.string_region, writer);

                try err.prefix(filename, e.last_char, .note, writer);
                try writer.writeAll("try adding an apostrophe here or at an earlier position\n" ++ err.Unbold);
                try err.pointTo(src, e.last_char, writer);
            },

            .incomplete_hex_escaped_character => |e| {
                try err.prefix(filename, e.backslash, .err, writer);
                try writer.writeAll("expected hex digits after hex escape\n" ++ err.Unbold);
                try err.pointTo(src, e.backslash, writer);

                try err.prefix(filename, e.after_backslash, .note, writer);
                try writer.writeAll("try adding any number of hex digits here\n" ++ err.Unbold);
                try err.pointTo(src, e.after_backslash, writer);
            },

            .invalid_escape_sequence => |e| {
                try err.prefix(filename, e.escape_sequence, .err, writer);
                try writer.writeAll("invalid escape sequence\n" ++ err.Unbold);
                try err.pointTo(src, e.escape_sequence, writer);

                try err.prefix(filename, e.after_backslash, .note, writer);
                try writer.writeAll("try adding a simple escape sequence (e.g. \\n) or hex/octal escaped bytes\n" ++ err.Unbold);
                try err.pointTo(src, e.after_backslash, writer);
            },

            .unexpected_character => |e| {
                try err.prefix(filename, e.unexpected, .err, writer);
                try writer.writeAll("unrecognized character\n" ++ err.Unbold);
                try err.pointTo(src, e.unexpected, writer);
            },
        }
    }
};

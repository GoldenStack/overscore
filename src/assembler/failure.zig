const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const lex = @import("../lex.zig");
const err = @import("../err.zig");
const Ranged = lex.Ranged;
const Range = lex.Range;

pub const Error = union(enum) {
    /// We expected one of several tags, but found an incorrect one.
    expected_tag: struct {
        expected: []const Token,
        found: Ranged(Token),
    },

    /// Number literals must have a base at the end.
    expected_base: Range,

    /// Invalid base X word literal
    invalid_number_literal: struct {
        number: Range,
        base: usize,
    },

    /// Invalid amount of indirection
    invalid_indirection: struct {
        argument: Range,
        expected: []const u8,
        found: usize,
    },

    /// Unknown label
    unknown_label: Range,

    pub fn display(self: @This(), filename: []const u8, src: []const u8, writer: anytype) !void {
        switch (self) {
            .expected_tag => |exp| {
                const expected = exp.expected;

                try err.prefix(filename, exp.found.range, .err, writer);
                try writer.writeAll("expected ");

                switch (expected.len) {
                    0 => @panic("expected at least one argument to expect"),
                    1 => try writer.print("{f}", .{expected[0]}),
                    2 => try writer.print("{f} or {f}", .{ expected[0], expected[1] }),
                    else => {
                        try writer.writeAll("one of ");
                        for (0.., expected) |index, tag| {
                            if (index != 0) try writer.writeAll(", ");
                            try writer.print("{f}", .{tag});
                        }
                    },
                }

                try writer.print(", but found {f}\n" ++ err.Unbold, .{exp.found.value});
                try err.pointTo(src, exp.found.range, writer);
            },
            .expected_base => |base| {
                try err.prefix(filename, base, .err, writer);
                try writer.print("number literals must have a base ('x', 'd', or 'b') at the end if they have multiple digits\n" ++ err.Unbold, .{});
                try err.pointTo(src, base, writer);
            },
            .invalid_number_literal => |invalid| {
                try err.prefix(filename, invalid.number, .err, writer);
                try writer.print("invalid number literal of base {}\n" ++ err.Unbold, .{invalid.base});
                try err.pointTo(src, invalid.number, writer);
            },
            .invalid_indirection => |invalid| {
                try err.prefix(filename, invalid.argument, .err, writer);

                try writer.writeAll("expected ");
                const fields = invalid.expected;
                switch (fields.len) {
                    0 => @panic("expected at least one indirection level to expect"),
                    1 => try writer.print("{}", .{fields[0]}),
                    2 => try writer.print("{} or {}", .{ fields[0], fields[1] }),
                    else => {
                        try writer.writeAll("one of ");
                        for (0.., fields) |index, tag| {
                            if (index != 0) try writer.writeAll(", ");
                            try writer.print("{}", .{tag});
                        }
                    },
                }
                try writer.print(" levels of indirection, but found {}\n" ++ err.Unbold, .{invalid.found});
                try err.pointTo(src, invalid.argument, writer);
            },
            .unknown_label => |unknown| {
                try err.prefix(filename, unknown, .err, writer);
                try writer.print("unknown label '{s}'\n" ++ err.Unbold, .{unknown.substr(src)});
                try err.pointTo(src, unknown, writer);
            },
        }
    }
};

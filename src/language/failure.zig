const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const Range = tokenizer.Range;

const Esc = "\x1B";
const Csi = Esc ++ "[";

const Bold = Csi ++ "1m";
const Unbold = Csi ++ "22m";

const Reset = Csi ++ "0m";

const Red = Csi ++ "31m";
const Cyan = Csi ++ "36m";

pub const Error = union(enum) {
    /// We expected one of several tags, but found an incorrect one.
    expected_tag: struct {
        expected: []const Token,
        found: Ranged(Token),
    },

    /// A number literal was too large.
    number_too_large: Range,

    /// An identifier was declared in a context in which it already exists.
    redeclared_identifier: struct {
        declared: Range,
        redeclared: Range,
    },

    /// An identifier was used but it wasn't declared.
    unknown_identifier: Range,

    /// Expected entirely tagged or entirely untagged fields, but found a mixture.
    expected_homogenous_fields: struct {
        untagged_example: Range,
        tagged_example: Range,
    },

    /// A tag was declared twice in the same container or function.
    duplicate_tag: struct {
        declared: Range,
        redeclared: Range,
    },

    /// Expected only untagged fields, but found a tagged field.
    expected_untagged_fields: struct {
        counterexample: Range,
    },

    /// Expected only tagged fields, but found an untagged field.
    expected_tagged_fields: struct {
        counterexample: Range,
    },

    pub fn display(self: @This(), filename: []const u8, src: []const u8, writer: anytype) !void {
        switch (self) {
            .expected_tag => |exp| {
                const expected = exp.expected;

                try prefix(filename, exp.found.range, .err, writer);
                try writer.writeAll("expected ");

                switch (expected.len) {
                    0 => @panic("expected at least one argument to expect"),
                    1 => try writer.print("{}", .{expected[0]}),
                    2 => try writer.print("{} or {}", .{ expected[0], expected[1] }),
                    else => {
                        try writer.writeAll("one of ");
                        for (0.., expected) |index, tag| {
                            if (index != 0) try writer.writeAll(", ");
                            try writer.print("{}", .{tag});
                        }
                    },
                }

                try writer.print(", but found {}\n" ++ Unbold, .{exp.found.value});
                try point_to(src, exp.found.range, writer);
            },
            .number_too_large => |number| {
                try prefix(filename, number, .err, writer);
                try writer.print("number \"{s}\" too large to store\n" ++ Unbold, .{number.substr(src)});
                try point_to(src, number, writer);
            },
            .redeclared_identifier => |red| {
                try prefix(filename, red.redeclared, .err, writer);
                try writer.print("redeclaration of identifier '{s}'\n" ++ Unbold, .{red.redeclared.substr(src)});
                try point_to(src, red.redeclared, writer);

                try prefix(filename, red.declared, .note, writer);
                try writer.writeAll("identifier initially declared here\n" ++ Unbold);
                try point_to(src, red.declared, writer);
            },
            .unknown_identifier => |unknown| {
                try prefix(filename, unknown, .err, writer);
                try writer.print("use of undeclared identifier '{s}'\n" ++ Unbold, .{unknown.substr(src)});
                try point_to(src, unknown, writer);
            },
            .expected_homogenous_fields => |exp| {
                if (exp.tagged_example.start.pos > exp.untagged_example.start.pos) {
                    try prefix(filename, exp.tagged_example, .err, writer);
                    try writer.writeAll("tagged field declared after untagged field was declared in the same container\n" ++ Unbold);
                    try point_to(src, exp.tagged_example, writer);

                    try prefix(filename, exp.untagged_example, .note, writer);
                    try writer.writeAll("untagged field was declared here\n" ++ Unbold);
                    try point_to(src, exp.untagged_example, writer);
                } else {
                    try prefix(filename, exp.untagged_example, .err, writer);
                    try writer.writeAll("untagged field declared after tagged field was declared in the same container\n" ++ Unbold);
                    try point_to(src, exp.untagged_example, writer);

                    try prefix(filename, exp.tagged_example, .note, writer);
                    try writer.writeAll("tagged field was declared here\n" ++ Unbold);
                    try point_to(src, exp.tagged_example, writer);
                }
            },
            .duplicate_tag => |dup| {
                try prefix(filename, dup.redeclared, .err, writer);
                try writer.print("redeclaration of tag '{s}'\n" ++ Unbold, .{dup.redeclared.substr(src)});
                try point_to(src, dup.redeclared, writer);

                try prefix(filename, dup.declared, .note, writer);
                try writer.writeAll("tag initially declared here\n" ++ Unbold);
                try point_to(src, dup.declared, writer);
            },
            .expected_untagged_fields => |exp| {
                try prefix(filename, exp.counterexample, .err, writer);
                try writer.writeAll("expected untagged fields, but found a tagged one\n" ++ Unbold);
                try point_to(src, exp.counterexample, writer);
            },
            .expected_tagged_fields => |exp| {
                try prefix(filename, exp.counterexample, .err, writer);
                try writer.writeAll("expected tagged fields, but found an untagged one\n" ++ Unbold);
                try point_to(src, exp.counterexample, writer);
            },
        }
    }

    fn line_around(src: []const u8, location: tokenizer.Location) []const u8 {
        const maybeStart = std.mem.lastIndexOfScalar(u8, src[0..location.pos], '\n');
        const maybeEnd = std.mem.indexOfScalarPos(u8, src, location.pos, '\n');

        // Add 1 to cut off the newline
        const start = if (maybeStart) |index| index + 1 else 0;
        const end = maybeEnd orelse src.len;

        return src[start..end];
    }

    fn prefix(filename: []const u8, range: Range, display_type: enum { err, note }, writer: anytype) !void {
        const format = switch (display_type) {
            .err => Red ++ "error",
            .note => Cyan ++ "note",
        };

        try writer.print(Bold ++ "{s}:{}:{}: {s}: " ++ Reset ++ Bold, .{ filename, range.start.row, range.start.col, format });
    }

    fn line_prefix(loc: tokenizer.Location, display_type: enum { line, blank, continued }, writer: anytype) !void {
        const line_print_len = std.math.log10_int(loc.row) + 1;

        switch (display_type) {
            .line => try writer.print(" {} | ", .{loc.row}),
            .blank => {
                try writer.writeByteNTimes(' ', 1 + line_print_len + 1);
                try writer.writeAll("| ");
            },
            .continued => {
                const number_of_periods = @min(3, line_print_len);

                try writer.writeByteNTimes(' ', 1 + line_print_len - number_of_periods);
                try writer.writeByteNTimes('.', number_of_periods);
                try writer.writeAll(" | ");
            },
        }
    }

    fn point_to(src: []const u8, range: Range, writer: anytype) !void {
        const lines_diff = range.end.row -| range.start.row;

        if (lines_diff == 0) {
            try line_prefix(range.start, .line, writer);
            try writer.writeAll(line_around(src, range.start));
            try writer.writeAll("\n");

            try line_prefix(range.start, .blank, writer);

            try writer.writeByteNTimes(' ', range.start.col - 1);
            try writer.writeByteNTimes('^', range.end.col - range.start.col);
            try writer.writeAll("\n");
        } else {
            const first_line = line_around(src, range.start);

            try line_prefix(range.start, .line, writer);
            try writer.writeAll(first_line);
            try writer.writeAll("\n");

            try line_prefix(range.start, if (lines_diff > 1) .continued else .blank, writer);
            try writer.writeByteNTimes(' ', range.start.col - 1);
            try writer.writeAll("^");
            try writer.writeByteNTimes('~', (first_line.len -| 1) - (range.start.col - 1));
            try writer.writeAll("\n");

            try line_prefix(range.end, .line, writer);
            try writer.writeAll(line_around(src, range.end));
            try writer.writeAll("\n");

            try line_prefix(range.end, .blank, writer);
            try writer.writeByteNTimes('~', range.end.col - 1 -| 1);
            try writer.writeAll("^\n");
        }
    }
};

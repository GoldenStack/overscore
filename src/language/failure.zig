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
    number_too_large: Ranged(Token),

    /// An identifier was declared in a context in which it already exists.
    redeclared_identifier: struct {
        declared: Ranged(Token),
        redeclared: Ranged(Token),
    },

    /// An identifier was used but it wasn't declared.
    unknown_identifier: Ranged(Token),

    /// Expected entirely tagged or entirely untagged fields, but found a mixture.
    expected_homogenous_fields: struct {
        untagged_example: tokenizer.Range,
        tagged_example: tokenizer.Range,
    },

    /// A tag was declared twice in the same container or function.
    duplicate_tag: struct {
        declared: Ranged(Token),
        redeclared: Ranged(Token),
    },

    /// Expected only untagged fields, but found a tagged field.
    expected_untagged_fields: struct {
        counterexample: tokenizer.Range,
    },

    /// Expected only tagged fields, but found an untagged field.
    expected_tagged_fields: struct {
        counterexample: tokenizer.Range,
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
                try prefix(filename, number.range, .err, writer);
                try writer.print("number \"{s}\" too large to store\n" ++ Unbold, .{number.range.substr(src)});
                try point_to(src, number.range, writer);
            },
            .redeclared_identifier => |red| {
                try prefix(filename, red.redeclared.range, .err, writer);
                try writer.print("redeclaration of identifier '{s}'\n" ++ Unbold, .{red.redeclared.range.substr(src)});
                try point_to(src, red.redeclared.range, writer);

                try prefix(filename, red.declared.range, .note, writer);
                try writer.writeAll("identifier initially declared here\n" ++ Unbold);
                try point_to(src, red.declared.range, writer);
            },
            .unknown_identifier => |unknown| {
                try prefix(filename, unknown.range, .err, writer);
                try writer.print("use of undeclared identifier '{s}'\n" ++ Unbold, .{unknown.range.substr(src)});
                try point_to(src, unknown.range, writer);
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
                try prefix(filename, dup.redeclared.range, .err, writer);
                try writer.print("redeclaration of tag '{s}'\n" ++ Unbold, .{dup.redeclared.range.substr(src)});
                try point_to(src, dup.redeclared.range, writer);

                try prefix(filename, dup.declared.range, .note, writer);
                try writer.writeAll("tag initially declared here\n" ++ Unbold);
                try point_to(src, dup.declared.range, writer);
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

    const DisplayType = enum {
        err,
        note,
    };

    fn prefix(filename: []const u8, range: Range, display_type: DisplayType, writer: anytype) !void {
        const format = switch (display_type) {
            .err => Red ++ "error",
            .note => Cyan ++ "note",
        };

        try writer.print(Bold ++ "{s}:{}:{}: {s}: " ++ Reset ++ Bold, .{ filename, range.start.row, range.start.col, format });
    }

    fn line_prefix(loc: tokenizer.Location, show_number: bool, writer: anytype) !void {
        if (show_number) {
            try writer.print(" {} | ", .{loc.row});
        } else {
            const line_print_len = std.math.log10_int(loc.row) + 1;

            for (0..1 + line_print_len + 1) |_| try writer.writeAll(" ");
            try writer.writeAll("| ");
        }
    }

    fn point_to(src: []const u8, range: Range, writer: anytype) !void {
        if (range.start.row == range.end.row) {
            try line_prefix(range.start, true, writer);
            try writer.writeAll(line_around(src, range.start));
            try writer.writeAll("\n");

            try line_prefix(range.start, false, writer);

            for (0..range.start.col - 1) |_| try writer.writeAll(" ");
            for (0..range.end.col - range.start.col) |_| try writer.writeAll("^");
            try writer.writeAll("\n");
        } else {
            const first_line = line_around(src, range.start);

            try line_prefix(range.start, true, writer);
            try writer.writeAll(first_line);
            try writer.writeAll("\n");

            try line_prefix(range.start, false, writer);
            for (0..range.start.col - 1) |_| try writer.writeAll(" ");
            try writer.writeAll("^");
            for (range.start.col - 1..first_line.len -| 1) |_| try writer.writeAll("~");
            try writer.writeAll("\n");

            try line_prefix(range.end, true, writer);
            try writer.writeAll(line_around(src, range.end));
            try writer.writeAll("\n");

            try line_prefix(range.end, false, writer);
            for (0..range.end.col - 1 -| 1) |_| try writer.writeAll("~");
            try writer.writeAll("^\n");
        }
    }
};

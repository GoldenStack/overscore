const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const Range = tokenizer.Range;

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

                try prefix(filename, exp.found.range, writer);
                try writer.writeAll("error: expected ");

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

                try writer.print(", but found {}\n", .{exp.found.value});
                try point_to(src, exp.found.range, writer);
            },
            .number_too_large => |number| {
                try prefix(filename, number.range, writer);
                try writer.print("number \"{s}\" too large to store\n", .{number.range.substr(src)});
                try point_to(src, number.range, writer);
            },
            .redeclared_identifier => |red| {
                try prefix(filename, red.redeclared.range, writer);
                try writer.print("error: redeclaration of identifier '{s}'\n", .{red.redeclared.range.substr(src)});
                try point_to(src, red.redeclared.range, writer);

                try prefix(filename, red.declared.range, writer);
                try writer.writeAll("note: identifier initially declared here\n");
                try point_to(src, red.declared.range, writer);
            },
            .unknown_identifier => |unknown| {
                try prefix(filename, unknown.range, writer);
                try writer.print("error: use of undeclared identifier '{s}'\n", .{unknown.range.substr(src)});
                try point_to(src, unknown.range, writer);
            },
            .expected_homogenous_fields => |exp| {
                if (exp.tagged_example.start.pos > exp.untagged_example.start.pos) {
                    try prefix(filename, exp.tagged_example, writer);
                    try writer.writeAll("error: tagged field declared after untagged field was declared in the same container\n");
                    try point_to(src, exp.tagged_example, writer);

                    try prefix(filename, exp.untagged_example, writer);
                    try writer.writeAll("note: untagged field was declared here\n");
                    try point_to(src, exp.untagged_example, writer);
                } else {
                    try prefix(filename, exp.untagged_example, writer);
                    try writer.writeAll("error: untagged field declared after tagged field was declared in the same container\n");
                    try point_to(src, exp.untagged_example, writer);

                    try prefix(filename, exp.tagged_example, writer);
                    try writer.writeAll("note: tagged field was declared here\n");
                    try point_to(src, exp.tagged_example, writer);
                }
            },
            .duplicate_tag => |dup| {
                try prefix(filename, dup.redeclared.range, writer);
                try writer.print("error: redeclaration of tag '{s}'\n", .{dup.redeclared.range.substr(src)});
                try point_to(src, dup.redeclared.range, writer);

                try prefix(filename, dup.declared.range, writer);
                try writer.writeAll("note: tag initially declared here\n");
                try point_to(src, dup.declared.range, writer);
            },
            .expected_untagged_fields => |exp| {
                try prefix(filename, exp.counterexample, writer);
                try writer.writeAll("error: expected untagged fields, but found a tagged one\n");
                try point_to(src, exp.counterexample, writer);
            },
            .expected_tagged_fields => |exp| {
                try prefix(filename, exp.counterexample, writer);
                try writer.writeAll("error: expected tagged fields, but found an untagged one\n");
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

    fn prefix(filename: []const u8, range: Range, writer: anytype) !void {
        try writer.print("{s}:{}:{}: ", .{ filename, range.start.row, range.start.col });
    }

    fn point_to(src: []const u8, range: Range, writer: anytype) !void {
        if (range.start.row == range.end.row) {
            try writer.writeAll(line_around(src, range.start));
            try writer.writeAll("\n");

            for (0..range.start.col - 1) |_| try writer.writeAll(" ");
            for (0..range.end.col - range.start.col) |_| try writer.writeAll("^");
            try writer.writeAll("\n");
        } else {
            const first_line = line_around(src, range.start);

            try writer.writeAll(first_line);
            try writer.writeAll("\n");

            for (0..range.start.col - 1) |_| try writer.writeAll(" ");
            try writer.writeAll("^");
            for (range.start.col - 1..first_line.len -| 1) |_| try writer.writeAll("~");
            try writer.writeAll("\n");
            
            try writer.writeAll(line_around(src, range.end));
            try writer.writeAll("\n");

            for (0..range.end.col - 1 -| 1) |_| try writer.writeAll("~");
            try writer.writeAll("^\n");
        }
    }
};

const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const Error = union(enum) {
    /// We expected one of several tags, but found an incorrect one.
    expected_tag: struct {
        expected: []const tokenizer.Token.Tag,
        found: tokenizer.Token,
    },

    /// A number literal was too large.
    number_too_large: tokenizer.Token,

    /// An identifier was declared in a context in which it already exists.
    redeclared_identifier: struct {
        declared: tokenizer.Token,
        redeclared: tokenizer.Token,
    },

    /// An identifier was used but it wasn't declared.
    unknown_identifier: tokenizer.Token,

    /// Expected entirely tagged or entirely untagged fields, but found a mixture.
    expected_homogenous_fields: struct {
        untagged_example: tokenizer.Range,
        tagged_example: tokenizer.Range,
    },

    /// A tag was declared twice in the same container or function.
    duplicate_tag: struct { tokenizer.Token, tokenizer.Token },

    /// Expected only untagged fields, but found a tagged field.
    expected_untagged_fields: struct {
        counterexample: tokenizer.Range,
    },

    /// Expected only tagged fields, but found an untagged field.
    expected_tagged_fields: struct {
        counterexample: tokenizer.Range,
    },

    fn write_message(self: @This(), writer: anytype) !void {
        switch (self) {
            .expected_tag => |value| {
                const expected = value.expected;

                switch (expected.len) {
                    0 => @panic("expected at least one argument to expect"),
                    1 => try writer.print("expected {}", .{expected[0]}),
                    2 => try writer.print("expected {} or {}", .{ expected[0], expected[1] }),
                    else => {
                        try writer.writeAll("expected one of ");
                        for (0.., expected) |index, tag| {
                            if (index != 0) try writer.writeAll(", ");

                            try writer.print("{}", .{tag});
                        }
                    },
                }

                try writer.print(", but found {}", .{value.found.tag});
            },
            .number_too_large => |number| try writer.print("number \"{s}\" too large to store", .{number}),
            .redeclared_identifier => |red| try writer.print("redeclaration of identifier '{s}'", .{red.redeclared.value}),
            .unknown_identifier => |unknown| try writer.print("use of undeclared identifier '{s}'", .{unknown.value}),
            .expected_homogenous_fields => try writer.writeAll("found a mixture of tagged and untagged fields"),
            .duplicate_tag => |tag| try writer.print("tag '{s}' declared multiple times", .{tag.@"1".value}),
            .expected_untagged_fields => try writer.writeAll("expected only untagged fields, but found a counterexample"),
            .expected_tagged_fields => try writer.writeAll("expected only tagged fields, but found a counterexample"),
        }
    }

    fn primary_token(self: @This()) tokenizer.Range {
        return switch (self) {
            .expected_tag => |tag| tag.found.range,
            .number_too_large => |num| num.range,
            .redeclared_identifier => |red| red.redeclared.range,
            .unknown_identifier => |unk| unk.range,
            .expected_homogenous_fields => |exp|
                if (exp.untagged_example.start.pos > exp.tagged_example.start.pos)
                    exp.untagged_example else exp.tagged_example,
            .duplicate_tag => |tag| tag.@"1".range,
            .expected_untagged_fields => |exp| exp.counterexample,
            .expected_tagged_fields => |exp| exp.counterexample,
        };
    }

    fn line_around(src: []const u8, location: tokenizer.Location) []const u8 {
        const maybeStart = std.mem.lastIndexOfScalar(u8, src[0..location.pos], '\n');
        const maybeEnd = std.mem.indexOfScalarPos(u8, src, location.pos, '\n');

        // Add 1 to cut off the newline
        const start = if (maybeStart) |index| index + 1 else 0;
        const end = maybeEnd orelse src.len;

        return src[start..end];
    }

    fn note(src: []const u8, location: tokenizer.Location, len: usize, writer: anytype) !void {
        try writer.writeAll(line_around(src, location));
        try writer.writeAll("\n");

        for (0..location.col - 1) |_| try writer.writeAll(" ");
        for (0..len) |_| try writer.writeAll("^");
        try writer.writeAll("\n");
    }

    pub fn display(self: @This(), filename: []const u8, src: []const u8, writer: anytype) !void {
        const token = self.primary_token();
        const start = token.start;
        const end = token.end;

        // Print the first line
        try writer.print("{s}:{}:{}: error: ", .{filename, start.row, start.col});
        try self.write_message(writer);
        try writer.writeAll("\n");

        // Point to the error location
        try note(src, start, end.col - start.col, writer);
    }

};

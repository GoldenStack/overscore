const std = @import("std");
const lex = @import("lex.zig");

pub const Esc = "\x1B";
pub const Csi = Esc ++ "[";

pub const Bold = Csi ++ "1m";
pub const Unbold = Csi ++ "22m";

pub const Reset = Csi ++ "0m";

pub const Red = Csi ++ "31m";
pub const Cyan = Csi ++ "36m";

/// The error set of errors that can occur while dealing with code.
pub const Err = error{
    CodeError,
    OutOfMemory,
};

/// Gets the line around the given location as a slice.
fn lineAround(src: []const u8, location: lex.Location) []const u8 {
    const maybeStart = std.mem.lastIndexOfScalar(u8, src[0..location.pos], '\n');
    const maybeEnd = std.mem.indexOfScalarPos(u8, src, location.pos, '\n');

    // Add 1 to cut off the newline
    const start = if (maybeStart) |index| index + 1 else 0;

    // If there's no end, take the end of the file.
    const end = maybeEnd orelse src.len;

    return src[start..end];
}

/// Displays the line prefix for a given filename, range of source code in the
/// file, and the type (error or just a note), into the given writer.
pub fn prefix(filename: []const u8, range: lex.Range, display_type: enum { err, note }, writer: anytype) !void {
    const format = switch (display_type) {
        .err => Red ++ "error",
        .note => Cyan ++ "note",
    };

    try writer.print(Bold ++ "{s}:{}:{}: {s}: " ++ Reset ++ Bold, .{ filename, range.start.row, range.start.col, format });
}

/// Writes the prefix for how a line should be displayed into the given writer.
fn linePrefix(loc: lex.Location, line_print_len: usize, display_type: enum { line, blank, continued }, writer: anytype) !void {
    switch (display_type) {
        .line => {
            const number_len = std.math.log10_int(loc.row);
            const spaces = @max(1, line_print_len - number_len);

            try writer.splatByteAll(' ', spaces);
            try writer.print("{} | ", .{loc.row});
        },
        .blank => {
            try writer.splatByteAll(' ', 1 + line_print_len + 1);
            try writer.writeAll("| ");
        },
        .continued => {
            const number_of_periods = @min(3, line_print_len);

            try writer.splatByteAll(' ', 1 + line_print_len - number_of_periods);
            try writer.splatByteAll('.', number_of_periods);
            try writer.writeAll(" | ");
        },
    }
}

/// Points to a specified range inside a source file, writing the result into
/// the given writer.
pub fn pointTo(src: []const u8, range: lex.Range, writer: anytype) !void {
    const lines_diff = range.end.row -| range.start.row;

    const line_print_len = std.math.log10_int(@max(range.start.row, range.end.row)) + 1;

    // Don't display a continuation between lines if there's just one line.
    if (lines_diff == 0) {
        try linePrefix(range.start, line_print_len, .line, writer);
        try writer.writeAll(lineAround(src, range.start));
        try writer.writeAll("\n");

        try linePrefix(range.start, line_print_len, .blank, writer);

        try writer.splatByteAll(' ', range.start.col - 1);

        // Handle zero-long token (EOF). This is only necessary in this branch.
        const point_len = @max(1, range.end.col - range.start.col);
        try writer.splatByteAll('^', point_len);
        try writer.writeAll("\n");
    } else {
        const first_line = lineAround(src, range.start);

        try linePrefix(range.start, line_print_len, .line, writer);
        try writer.writeAll(first_line);
        try writer.writeAll("\n");

        try linePrefix(range.start, line_print_len, if (lines_diff > 1) .continued else .blank, writer);
        try writer.splatByteAll(' ', range.start.col - 1);
        try writer.writeAll("^");
        try writer.splatByteAll('~', (first_line.len -| 1) - (range.start.col - 1));
        try writer.writeAll("\n");

        try linePrefix(range.end, line_print_len, .line, writer);
        try writer.writeAll(lineAround(src, range.end));
        try writer.writeAll("\n");

        try linePrefix(range.end, line_print_len, .blank, writer);
        try writer.splatByteAll('~', range.end.col - 1 -| 1);
        try writer.writeAll("^\n");
    }
}

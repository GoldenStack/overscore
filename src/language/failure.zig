const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const Range = tokenizer.Range;
const ir = @import("Ir.zig").ir;

const Esc = "\x1B";
const Csi = Esc ++ "[";

const Bold = Csi ++ "1m";
const Unbold = Csi ++ "22m";

const Reset = Csi ++ "0m";

const Red = Csi ++ "31m";
const Cyan = Csi ++ "36m";

/// The error set of errors that can occur while dealing with code.
pub const ErrorSet = error{
    CodeError,
    OutOfMemory,
};

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

    /// A member name was used twice in the same container.
    duplicate_member: struct {
        declared: Range,
        redeclared: Range,
    },

    /// An identifier was used but it wasn't declared.
    unknown_identifier: Range,

    /// An expression depends on itself.
    dependency_loop: Range,

    /// An expression was supposed to have a type but had a different one.
    mismatched_type: struct {
        expected_type: []const u8,
        found_type: []const u8,
        has_wrong_type: Range,
        expected_type_declared: Range,
    },

    /// Expected a type expression, but found something that isn't a type.
    expected_type_expression: Range,

    /// Tried to access the member of an instance of a type that doesn't support
    /// member access.
    unsupported_member_access: struct {
        type: []const u8,
        member: Range, // TODO: Point to the entire member access, e.g. `a.b`.
    },

    /// Tried to access a member of a container that doesn't exist.
    unknown_member: struct { // TODO: Also point to type declaration if it exists
        type: []const u8,
        member: Range, // TODO: Point to the entire member access, e.g. `a.b`.
    },

    /// Tried to access the member of a container, but it was private.
    private_member: struct { // TODO: Also point to type declaration if it exists
        declaration: Range,
        member: Range, // TODO: Point to the entire member access, e.g. `a.b`.
    },

    /// Tried to dereference an expression that's not a pointer.
    dereferenced_non_pointer: struct {
        expr: Range,
        type: []const u8,
    },

    /// Parentheses are required to disambiguate confusing operator precedence
    /// Stolen from [Zig#114](https://github.com/ziglang/zig/issues/114).
    mixed_precedence: struct {
        expr: Range,
        operator: Range,
    },

    /// You can only multiply or add decls.
    can_only_multiply_or_add_decls: struct {
        invalid_field: Range,
        typedef: Range,
    },

    /// Cannot coerce one type to another type.
    cannot_coerce: struct {
        from: []const u8,
        to: []const u8,
        context: Range,
    },

    // /// Expected entirely tagged or entirely untagged fields, but found a mixture.
    // expected_homogenous_fields: struct {
    //     untagged_example: Range,
    //     tagged_example: Range,
    // },

    // /// Expected only untagged fields, but found a tagged field.
    // expected_untagged_fields: struct {
    //     counterexample: Range,
    // },

    // /// Expected only tagged fields, but found an untagged field.
    // expected_tagged_fields: struct {
    //     counterexample: Range,
    // },

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
                try pointTo(src, exp.found.range, writer);
            },
            .number_too_large => |number| {
                try prefix(filename, number, .err, writer);
                try writer.print("number \"{s}\" too large to store\n" ++ Unbold, .{number.substr(src)});
                try pointTo(src, number, writer);
            },
            .redeclared_identifier => |red| {
                try prefix(filename, red.redeclared, .err, writer);
                try writer.print("redeclaration of identifier '{s}'\n" ++ Unbold, .{red.redeclared.substr(src)});
                try pointTo(src, red.redeclared, writer);

                try prefix(filename, red.declared, .note, writer);
                try writer.writeAll("identifier initially declared here\n" ++ Unbold);
                try pointTo(src, red.declared, writer);
            },
            .duplicate_member => |dup| {
                try prefix(filename, dup.redeclared, .err, writer);
                try writer.print("duplicate member name '{s}'\n" ++ Unbold, .{dup.redeclared.substr(src)});
                try pointTo(src, dup.redeclared, writer);

                try prefix(filename, dup.declared, .note, writer);
                try writer.writeAll("name initially declared here\n" ++ Unbold);
                try pointTo(src, dup.declared, writer);
            },
            .unknown_identifier => |unknown| {
                try prefix(filename, unknown, .err, writer);
                try writer.print("use of undeclared identifier '{s}'\n" ++ Unbold, .{unknown.substr(src)});
                try pointTo(src, unknown, writer);
            },
            .dependency_loop => |dep| {
                try prefix(filename, dep, .err, writer);
                try writer.print("expression depends on itself\n" ++ Unbold, .{});
                try pointTo(src, dep, writer);
            },
            .mismatched_type => |mis| {
                try prefix(filename, mis.has_wrong_type, .err, writer);
                try writer.print("expected expression to be of type '{s}', found '{s}'\n" ++ Unbold, .{ mis.expected_type, mis.found_type });
                try pointTo(src, mis.has_wrong_type, writer);

                try prefix(filename, mis.expected_type_declared, .note, writer);
                try writer.print("type declared here\n" ++ Unbold, .{});
                try pointTo(src, mis.expected_type_declared, writer);
            },
            .expected_type_expression => |exp| {
                try prefix(filename, exp, .err, writer);
                try writer.writeAll("expected type expression, but found a value that isn't a type\n" ++ Unbold);
                try pointTo(src, exp, writer);
            },
            .unsupported_member_access => |access| {
                try prefix(filename, access.member, .err, writer);
                try writer.print("expression of type '{s}' does not support member access \n" ++ Unbold, .{access.type});
                try pointTo(src, access.member, writer);
            },
            .unknown_member => |unknown| {
                try prefix(filename, unknown.member, .err, writer);
                try writer.print("no member named '{s}' on type '{s}'\n" ++ Unbold, .{ unknown.member.substr(src), unknown.type });
                try pointTo(src, unknown.member, writer);
            },
            .private_member => |private| {
                try prefix(filename, private.member, .err, writer);
                try writer.print("member '{s}' is private\n" ++ Unbold, .{private.member.substr(src)});
                try pointTo(src, private.member, writer);

                try prefix(filename, private.declaration, .note, writer);
                try writer.writeAll("member declared here\n" ++ Unbold);
                try pointTo(src, private.declaration, writer);
            },
            .dereferenced_non_pointer => |deref| {
                try prefix(filename, deref.expr, .err, writer);
                try writer.print("cannot dereference non-pointer type '{s}'\n" ++ Unbold, .{deref.type});
                try pointTo(src, deref.expr, writer);
            },
            .mixed_precedence => |mixed| {
                try prefix(filename, mixed.expr, .err, writer);
                try writer.print("parentheses are required to disambiguate confusing operator precedence with operator {s}\n" ++ Unbold, .{mixed.operator.substr(src)});
                try pointTo(src, mixed.operator, writer);
            },
            .can_only_multiply_or_add_decls => |can| {
                try prefix(filename, can.invalid_field, .err, writer);
                try writer.writeAll("can only multiply or add declarations\n" ++ Unbold);
                try pointTo(src, can.invalid_field, writer);

                try prefix(filename, can.typedef, .note, writer);
                try writer.writeAll("type declared here\n" ++ Unbold);
                try pointTo(src, can.typedef, writer);
            },
            .cannot_coerce => |coerce| {
                try prefix(filename, coerce.context, .err, writer);
                try writer.print("cannot coerce from type '{s}' to '{s}' \n" ++ Unbold, .{ coerce.from, coerce.to });
                try pointTo(src, coerce.context, writer);
            },
            // .expected_homogenous_fields => |exp| {
            //     if (exp.tagged_example.start.pos > exp.untagged_example.start.pos) {
            //         try prefix(filename, exp.tagged_example, .err, writer);
            //         try writer.writeAll("tagged field declared after untagged field was declared in the same container\n" ++ Unbold);
            //         try pointTo(src, exp.tagged_example, writer);

            //         try prefix(filename, exp.untagged_example, .note, writer);
            //         try writer.writeAll("untagged field was declared here\n" ++ Unbold);
            //         try pointTo(src, exp.untagged_example, writer);
            //     } else {
            //         try prefix(filename, exp.untagged_example, .err, writer);
            //         try writer.writeAll("untagged field declared after tagged field was declared in the same container\n" ++ Unbold);
            //         try pointTo(src, exp.untagged_example, writer);

            //         try prefix(filename, exp.tagged_example, .note, writer);
            //         try writer.writeAll("tagged field was declared here\n" ++ Unbold);
            //         try pointTo(src, exp.tagged_example, writer);
            //     }
            // },
            // .expected_untagged_fields => |exp| {
            //     try prefix(filename, exp.counterexample, .err, writer);
            //     try writer.writeAll("expected untagged fields, but found a tagged one\n" ++ Unbold);
            //     try pointTo(src, exp.counterexample, writer);
            // },
            // .expected_tagged_fields => |exp| {
            //     try prefix(filename, exp.counterexample, .err, writer);
            //     try writer.writeAll("expected tagged fields, but found an untagged one\n" ++ Unbold);
            //     try pointTo(src, exp.counterexample, writer);
            // },
        }
    }

    fn lineAround(src: []const u8, location: tokenizer.Location) []const u8 {
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

    fn linePrefix(loc: tokenizer.Location, line_print_len: usize, display_type: enum { line, blank, continued }, writer: anytype) !void {
        switch (display_type) {
            .line => {
                const number_len = std.math.log10_int(loc.row);
                const spaces = @max(1, line_print_len - number_len);

                try writer.writeByteNTimes(' ', spaces);
                try writer.print("{} | ", .{loc.row});
            },
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

    fn pointTo(src: []const u8, range: Range, writer: anytype) !void {
        const lines_diff = range.end.row -| range.start.row;

        const line_print_len = std.math.log10_int(@max(range.start.row, range.end.row)) + 1;

        if (lines_diff == 0) {
            try linePrefix(range.start, line_print_len, .line, writer);
            try writer.writeAll(lineAround(src, range.start));
            try writer.writeAll("\n");

            try linePrefix(range.start, line_print_len, .blank, writer);

            try writer.writeByteNTimes(' ', range.start.col - 1);
            try writer.writeByteNTimes('^', range.end.col - range.start.col);
            try writer.writeAll("\n");
        } else {
            const first_line = lineAround(src, range.start);

            try linePrefix(range.start, line_print_len, .line, writer);
            try writer.writeAll(first_line);
            try writer.writeAll("\n");

            try linePrefix(range.start, line_print_len, if (lines_diff > 1) .continued else .blank, writer);
            try writer.writeByteNTimes(' ', range.start.col - 1);
            try writer.writeAll("^");
            try writer.writeByteNTimes('~', (first_line.len -| 1) - (range.start.col - 1));
            try writer.writeAll("\n");

            try linePrefix(range.end, line_print_len, .line, writer);
            try writer.writeAll(lineAround(src, range.end));
            try writer.writeAll("\n");

            try linePrefix(range.end, line_print_len, .blank, writer);
            try writer.writeByteNTimes('~', range.end.col - 1 -| 1);
            try writer.writeAll("^\n");
        }
    }
};

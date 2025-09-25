const std = @import("std");
const Token = @import("tokenizer.zig").Token;
const lex = @import("../lex.zig");
const err = @import("../err.zig");
const Ranged = lex.Ranged;
const Range = lex.Range;
const ir = @import("Ir.zig").ir;

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
    expected_type_expression: struct {
        has_wrong_type: Range,
        found_type: []const u8,
    },

    /// Tried to access the member of an instance of a type that doesn't support
    /// member access.
    unsupported_member_access: struct {
        type: []const u8,
        member: Range, // TODO: Point to the entire member access, e.g. `a.b`.
    },

    /// Tried to access a pointer to the member of an instance of a type that
    /// isn't a pointer to something that supports member access.
    unsupported_indirect_member_access: struct {
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

    /// You can only cons definitions.
    can_only_cons_definitions: struct {
        invalid_field: Range,
        typedef: Range,
    },

    /// Cannot coerce one type to another type.
    cannot_coerce: struct {
        from: []const u8,
        to: []const u8,
        context: Range,
    },

    /// The left side to declarations and definitions must be a raw literal.
    literal_is_required_for_definitions_and_declarations: struct {
        not_literal: Range,
    },

    pub fn display(self: @This(), filename: []const u8, src: []const u8, writer: anytype) !void {
        switch (self) {
            .expected_tag => |exp| {
                const expected = exp.expected;

                try err.prefix(filename, exp.found.range, .err, writer);
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

                try writer.print(", but found {}\n" ++ err.Unbold, .{exp.found.value});
                try err.pointTo(src, exp.found.range, writer);
            },
            .number_too_large => |number| {
                try err.prefix(filename, number, .err, writer);
                try writer.print("number \"{s}\" too large to store\n" ++ err.Unbold, .{number.substr(src)});
                try err.pointTo(src, number, writer);
            },
            .redeclared_identifier => |red| {
                try err.prefix(filename, red.redeclared, .err, writer);
                try writer.print("redeclaration of identifier '{s}'\n" ++ err.Unbold, .{red.redeclared.substr(src)});
                try err.pointTo(src, red.redeclared, writer);

                try err.prefix(filename, red.declared, .note, writer);
                try writer.writeAll("identifier initially declared here\n" ++ err.Unbold);
                try err.pointTo(src, red.declared, writer);
            },
            .duplicate_member => |dup| {
                try err.prefix(filename, dup.redeclared, .err, writer);
                try writer.print("duplicate member name '{s}'\n" ++ err.Unbold, .{dup.redeclared.substr(src)});
                try err.pointTo(src, dup.redeclared, writer);

                try err.prefix(filename, dup.declared, .note, writer);
                try writer.writeAll("name initially declared here\n" ++ err.Unbold);
                try err.pointTo(src, dup.declared, writer);
            },
            .unknown_identifier => |unknown| {
                try err.prefix(filename, unknown, .err, writer);
                try writer.print("use of undeclared identifier '{s}'\n" ++ err.Unbold, .{unknown.substr(src)});
                try err.pointTo(src, unknown, writer);
            },
            .dependency_loop => |dep| {
                try err.prefix(filename, dep, .err, writer);
                try writer.print("expression depends on itself\n" ++ err.Unbold, .{});
                try err.pointTo(src, dep, writer);
            },
            .mismatched_type => |mis| {
                try err.prefix(filename, mis.has_wrong_type, .err, writer);
                try writer.print("expected expression to be of type '{s}', found '{s}'\n" ++ err.Unbold, .{ mis.expected_type, mis.found_type });
                try err.pointTo(src, mis.has_wrong_type, writer);

                try err.prefix(filename, mis.expected_type_declared, .note, writer);
                try writer.print("type declared here\n" ++ err.Unbold, .{});
                try err.pointTo(src, mis.expected_type_declared, writer);
            },
            .expected_type_expression => |exp| {
                try err.prefix(filename, exp.has_wrong_type, .err, writer);
                try writer.print("expected type expression, but found a value with type '{s}' instead\n" ++ err.Unbold, .{exp.found_type});
                try err.pointTo(src, exp.has_wrong_type, writer);
            },
            .unsupported_member_access => |access| {
                try err.prefix(filename, access.member, .err, writer);
                try writer.print("expression of type '{s}' does not support member access \n" ++ err.Unbold, .{access.type});
                try err.pointTo(src, access.member, writer);
            },
            .unsupported_indirect_member_access => |access| {
                try err.prefix(filename, access.member, .err, writer);
                try writer.print("expected a pointer to a type that supports member access, but found expression of type '{s}'\n" ++ err.Unbold, .{access.type});
                try err.pointTo(src, access.member, writer);
            },
            .unknown_member => |unknown| {
                try err.prefix(filename, unknown.member, .err, writer);
                try writer.print("no member named '{s}' on type '{s}'\n" ++ err.Unbold, .{ unknown.member.substr(src), unknown.type });
                try err.pointTo(src, unknown.member, writer);
            },
            .private_member => |private| {
                try err.prefix(filename, private.member, .err, writer);
                try writer.print("member '{s}' is private\n" ++ err.Unbold, .{private.member.substr(src)});
                try err.pointTo(src, private.member, writer);

                try err.prefix(filename, private.declaration, .note, writer);
                try writer.writeAll("member declared here\n" ++ err.Unbold);
                try err.pointTo(src, private.declaration, writer);
            },
            .dereferenced_non_pointer => |deref| {
                try err.prefix(filename, deref.expr, .err, writer);
                try writer.print("cannot dereference non-pointer type '{s}'\n" ++ err.Unbold, .{deref.type});
                try err.pointTo(src, deref.expr, writer);
            },
            .mixed_precedence => |mixed| {
                try err.prefix(filename, mixed.expr, .err, writer);
                try writer.print("parentheses are required to disambiguate confusing operator precedence with operator {s}\n" ++ err.Unbold, .{mixed.operator.substr(src)});
                try err.pointTo(src, mixed.operator, writer);
            },
            .can_only_multiply_or_add_decls => |can| {
                try err.prefix(filename, can.invalid_field, .err, writer);
                try writer.writeAll("can only multiply or add declarations\n" ++ err.Unbold);
                try err.pointTo(src, can.invalid_field, writer);

                try err.prefix(filename, can.typedef, .note, writer);
                try writer.writeAll("type declared here\n" ++ err.Unbold);
                try err.pointTo(src, can.typedef, writer);
            },
            .can_only_cons_definitions => |can| {
                try err.prefix(filename, can.invalid_field, .err, writer);
                try writer.writeAll("can only cons definitions\n" ++ err.Unbold);
                try err.pointTo(src, can.invalid_field, writer);

                try err.prefix(filename, can.typedef, .note, writer);
                try writer.writeAll("type declared here\n" ++ err.Unbold);
                try err.pointTo(src, can.typedef, writer);
            },
            .cannot_coerce => |coerce| {
                try err.prefix(filename, coerce.context, .err, writer);
                try writer.print("cannot coerce from type '{s}' to '{s}'\n" ++ err.Unbold, .{ coerce.from, coerce.to });
                try err.pointTo(src, coerce.context, writer);
            },
            .literal_is_required_for_definitions_and_declarations => |required| {
                try err.prefix(filename, required.not_literal, .err, writer);
                try writer.writeAll("the expression before the colon in definitions and declarations must be a standalone literal (e.g. '.foo')\n" ++ err.Unbold);
                try err.pointTo(src, required.not_literal, writer);
            },
        }
    }
};

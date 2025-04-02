const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Error = @import("failure.zig").Error;
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;

const Parser = @This();

pub const Container = struct {
    variant: ContainerVariant,
    fields: std.ArrayList(Ranged(Field)),
    decls: std.ArrayList(Ranged(ContainerDecl)),
};

pub const ContainerVariant = enum {
    sum,
    product,
};

pub const TaggedStatus = enum {
    untagged,
    tagged,
};

pub const Field = union(TaggedStatus) {
    untagged: Ranged(Expr),
    tagged: NamedExpr,
};

pub const NamedExpr = struct {
    name: Ranged(Token),
    value: Ranged(Expr),
};

pub const ContainerDecl = struct {
    access: Access,
    decl: Ranged(Decl),
};

pub const Decl = struct {
    mutability: Mutability,
    name: Ranged(Token),
    value: Ranged(Expr),
};

pub const Access = enum {
    private,
    public,
};

pub const Mutability = enum {
    constant,
    variable,
};

pub const Expr = union(enum) {
    function: struct {
        parameters: std.ArrayList(Ranged(Field)),
        @"return": *Ranged(Expr),
        body: ?Ranged(Block),
    },
    call: struct {
        function: *Ranged(Expr),
        arguments: std.ArrayList(Ranged(Expr)),
    },
    container: Ranged(Container),
    ident: Ranged(Token),
    block: Ranged(Block),
    number: u32,
    parentheses: *Ranged(Expr),
    unique: *Ranged(Expr),
    property: struct {
        container: *Ranged(Expr),
        property: Ranged(Token),
    },
};

pub const Block = struct {
    stmts: std.ArrayList(Ranged(Stmt)),
};

pub const Stmt = union(enum) {
    decl: Decl,
    @"return": Ranged(Expr),
};

/// The error set of errors that can occur while parsing.
pub const ParsingError = error{
    ParsingError,
    OutOfMemory,
};

src: [:0]const u8,
tokens: tokenizer.Tokenizer,
allocator: std.mem.Allocator,

next_token: ?Ranged(Token) = null,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.ParsingError, this value is significant. Otherwise, it may
/// contain anything.
error_context: ?Error = null,

pub fn init(tokens: tokenizer.Tokenizer, allocator: std.mem.Allocator) @This() {
    return .{
        .src = tokens.src,
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn read_root(self: *@This()) ParsingError!Ranged(Container) {
    return Ranged(Container).wrap(self, struct {
        fn read(parser: *Parser) ParsingError!Container {
            return parser.read_container_contents(.product);
        }
    }.read);
}

pub fn read_container(self: *@This()) ParsingError!Container {
    const token = try self.expect_many(&.{ .sum, .product });

    const variant: ContainerVariant = switch (token.value) {
        .sum => .sum,
        .product => .product,
        else => unreachable,
    };

    _ = try self.expect(.opening_curly_bracket);

    const container = try self.read_container_contents(variant);

    _ = try self.expect(.closing_curly_bracket);

    return container;
}

pub fn read_container_contents(self: *@This(), variant: ContainerVariant) ParsingError!Container {
    var container: Container = .{
        .variant = variant,
        .fields = std.ArrayList(Ranged(Field)).init(self.allocator),
        .decls = std.ArrayList(Ranged(ContainerDecl)).init(self.allocator),
    };

    const read = struct {
        fn read(parser: *Parser, context: *Container) ParsingError!void {
            switch (parser.peek().value) {
                .@"pub", .@"const", .@"var" => {
                    try context.decls.append(try Ranged(ContainerDecl).wrap(parser, read_container_decl));
                },
                else => {
                    try context.fields.append(try Ranged(Field).wrap(parser, read_field));

                    // This makes the last comma mandatory in file containers
                    if (parser.peek().value != .closing_curly_bracket) _ = try parser.expect(.comma);
                },
            }
        }
    }.read;

    try self.read_iterated_until(null, .closing_curly_bracket, &container, read);

    return container;
}

pub fn read_container_decl(self: *@This()) ParsingError!ContainerDecl {
    const access: Access = if (self.peek().value == .@"pub") .public else .private;
    if (access == .public) _ = self.next();

    return .{
        .access = access,
        .decl = try Ranged(Decl).wrap(self, read_decl),
    };
}

pub fn read_decl(self: *@This()) ParsingError!Decl {
    const token = try self.expect_many(&.{ .@"const", .@"var" });

    const mutability: Mutability = if (token.value == .@"const") .constant else .variable;

    const name = try self.expect(.ident);

    _ = try self.expect(.equals);

    const value = try self.read_expr();

    _ = try self.expect(.semicolon);

    return .{
        .mutability = mutability,
        .name = name,
        .value = value,
    };
}

pub fn read_expr_ptr(self: *@This()) ParsingError!*Ranged(Expr) {
    const ptr = try self.allocator.create(Ranged(Expr));
    ptr.* = try self.read_expr();
    return ptr;
}

pub fn read_expr(self: *@This()) ParsingError!Ranged(Expr) {
    var info = try Ranged(Expr).wrap(self, read_expr_raw);

    // Handle non-prefix operators
    while (true) {
        info = switch (self.peek().value) {
            .opening_parentheses => try info.map_extend(self, read_parameters),
            .period => try info.map_extend(self, read_property),
            else => break,
        };
    }

    return info;
}

fn read_expr_raw(self: *@This()) ParsingError!Expr {
    return switch (self.peek().value) {
        .@"fn" => try self.read_function(),
        .sum, .product => .{ .container = try Ranged(Container).wrap(self, read_container) },
        .ident => .{ .ident = self.next() },
        .opening_curly_bracket => .{ .block = try Ranged(Block).wrap(self, read_block) },
        .number => .{ .number = try self.read_number() },
        .opening_parentheses => .{ .parentheses = parens: {
            _ = self.next();

            const expr = try self.read_expr_ptr();

            _ = try self.expect(.closing_parentheses);
            break :parens expr;
        } },
        .unique => .{ .unique = unique: {
            _ = self.next();

            break :unique try self.read_expr_ptr();
        } },
        else => return self.fail_expected(&.{ .@"fn", .unique, .sum, .product, .ident, .opening_curly_bracket, .number, .unique }),
    };
}

pub fn read_property(self: *@This(), container: Ranged(Expr)) ParsingError!Expr {
    _ = try self.expect(.period);
    const property = try self.expect(.ident);

    const ptr = try self.allocator.create(Ranged(Expr));
    ptr.* = container;

    return .{ .property = .{
        .container = ptr,
        .property = property,
    } };
}

pub fn read_parameters(self: *@This(), function: Ranged(Expr)) ParsingError!Expr {
    _ = try self.expect(.opening_parentheses);

    var args = std.ArrayList(Ranged(Expr)).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Ranged(Expr))) ParsingError!void {
            try context.append(try parser.read_expr());
        }
    }.read;

    try self.read_iterated_until(.comma, .closing_parentheses, &args, read);

    _ = try self.expect(.closing_parentheses);

    const ptr = try self.allocator.create(Ranged(Expr));
    ptr.* = function;

    return .{ .call = .{
        .function = ptr,
        .arguments = args,
    } };
}

pub fn read_function(self: *@This()) ParsingError!Expr {
    _ = try self.expect(.@"fn");
    _ = try self.expect(.opening_parentheses);

    var params = std.ArrayList(Ranged(Field)).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Ranged(Field))) ParsingError!void {
            try context.append(try Ranged(Field).wrap(parser, read_field));
        }
    }.read;

    try self.read_iterated_until(.comma, .closing_parentheses, &params, read);

    _ = try self.expect(.closing_parentheses);

    const ret = try self.read_expr_ptr();

    const body = if (self.peek().value == .opening_curly_bracket)
        try Ranged(Block).wrap(self, read_block)
    else
        null;

    return .{ .function = .{
        .parameters = params,
        .@"return" = ret,
        .body = body,
    } };
}

pub fn read_block(self: *@This()) ParsingError!Block {
    _ = try self.expect(.opening_curly_bracket);

    var stmts = std.ArrayList(Ranged(Stmt)).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Ranged(Stmt))) ParsingError!void {
            try context.append(try Ranged(Stmt).wrap(parser, read_stmt));
        }
    }.read;

    try self.read_iterated_until(null, .closing_curly_bracket, &stmts, read);

    _ = try self.expect(.closing_curly_bracket);

    return .{ .stmts = stmts };
}

pub fn read_field(self: *@This()) ParsingError!Field {
    const expr = try self.read_expr();

    if (self.peek().value == .colon) { // Tagged
        if (expr.value == .ident) {
            _ = self.next(); // Read the colon

            const value = try self.read_expr();
            return .{ .tagged = .{
                .name = expr.value.ident,
                .value = value,
            } };
        } else return self.fail_expected(&.{.ident});
    } else { // Untagged
        return .{ .untagged = expr };
    }

    const name = try self.expect(.ident);

    _ = try self.expect(.colon);

    const value = try self.read_expr();

    return .{
        .name = name,
        .value = value,
    };
}

pub fn read_stmt(self: *@This()) ParsingError!Stmt {
    return switch (self.peek().value) {
        .@"return" => {
            _ = self.next();

            const expr = try self.read_expr();

            _ = try self.expect(.semicolon);

            return .{ .@"return" = expr };
        },
        else => .{ .decl = try self.read_decl() },
    };
}

pub fn read_number(self: *@This()) ParsingError!u32 {
    const token = try self.expect(.number);

    return std.fmt.parseUnsigned(u32, token.range.substr(self.src), 10) catch self.fail(.{ .number_too_large = token.range });
}

fn read_iterated_until(self: *@This(), comptime maybe_sep: ?Token, end: Token, context: anytype, reader: fn (*@This(), @TypeOf(context)) ParsingError!void) !void {
    // We just started reading, so we don't need a separator
    var sep_last_iter = true;

    while (true) {

        // Exit if the next token doesn't exist or if it's `end`
        const token = self.peek();
        if (token.value == .eof or token.value == end) return;

        // If there wasn't a separator, fail, having expected one
        // This check is placed after the exits so that a separator is optional
        // for the last argument
        if (maybe_sep) |sep| if (!sep_last_iter) return self.fail_expected(&.{sep});

        // Add the item and reset the separator tracker
        try reader(self, context);

        if (maybe_sep == null) continue;

        // Read the next token if it's the separator
        if (self.peek().value == maybe_sep) {
            _ = self.next();
            sep_last_iter = true;
        } else sep_last_iter = false;
    }
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": Error) error{ParsingError} {
    self.error_context = @"error";
    return error.ParsingError;
}

pub fn fail_expected(self: *@This(), comptime tags: []const Token) error{ParsingError} {
    return self.fail(.{ .expected_tag = .{
        .expected = tags,
        .found = self.peek(),
    } });
}

/// Reads a token, returning an error if it's not equal to the given tag.
pub fn expect(self: *@This(), comptime token: Token) !Ranged(Token) {
    return self.expect_many(&.{token});
}

/// Reads a token, returning an error if it's not one of the given tags.
pub fn expect_many(self: *@This(), comptime tags: []const Token) !Ranged(Token) {
    const next_tag = self.peek().value;

    inline for (tags) |tag| {
        if (tag == next_tag) return self.next();
    } else return self.fail_expected(tags);
}

/// Returns the next token from the backing token iterator without advancing the
/// iterator itself. This value is cached, so peeking does not require
/// re-reading.
pub fn peek(self: *@This()) Ranged(Token) {
    // Return the cached token if possible
    if (self.next_token) |token| return token;

    // Load the next token and backtrack.
    // This could be structured so that next() depends on peek(), but this would
    // mean that there's always hidden and unnecessary backtracking, which is
    // not ideal.
    const token = self.next();

    self.next_token = token;
    self.tokens.loc = token.range.start;

    return token;
}

/// Reads the next token from the backing token iterator.
pub fn next(self: *@This()) Ranged(Token) {
    // Return the cached token if possible
    if (self.next_token) |token| {
        self.tokens.loc = token.range.end;
        self.next_token = null;
        return token;
    }

    // Skip tokens until there's a non-comment
    while (true) {
        const token = self.tokens.next();
        // Comments mean nothing for now
        if (token.value != .comment) return token;
    }
}

pub fn location(self: *@This()) tokenizer.Location {
    return self.tokens.loc;
}

pub fn print_container(src: []const u8, container: Container, writer: anytype) anyerror!void {
    try writer.print("{s} {{ ", .{@tagName(container.variant)});

    for (container.fields.items) |field| {
        try print_field(src, field.value, writer);
        try writer.writeAll(", ");
    }

    for (container.decls.items) |decl| {
        try print_container_decl(src, decl.value, writer);
        try writer.writeAll(" ");
    }

    try writer.writeAll("}");
}

fn print_field(src: []const u8, field: Field, writer: anytype) anyerror!void {
    switch (field) {
        .untagged => |expr| try print_expr(src, expr.value, writer),
        .tagged => |named| try print_named_expr(src, named, writer),
    }
}

fn print_container_decl(src: []const u8, decl: ContainerDecl, writer: anytype) anyerror!void {
    try writer.print("{s} ", .{@tagName(decl.access)});
    try print_decl(src, decl.decl.value, writer);
}

fn print_decl(src: []const u8, decl: Decl, writer: anytype) anyerror!void {
    try writer.print("{s} {s} = ", .{ @tagName(decl.mutability), decl.name.range.substr(src) });
    try print_expr(src, decl.value.value, writer);
    try writer.writeAll(";");
}

fn print_named_expr(src: []const u8, named: NamedExpr, writer: anytype) anyerror!void {
    try writer.print("{s}: ", .{named.name.range.substr(src)});
    try print_expr(src, named.value.value, writer);
}

fn print_expr(src: []const u8, expr: Expr, writer: anytype) anyerror!void {
    switch (expr) {
        .function => |function| {
            try writer.writeAll("fn (");
            for (function.parameters.items) |param| {
                try print_field(src, param.value, writer);
                try writer.writeAll(", ");
            }
            try writer.writeAll(") ");
            try print_expr(src, function.@"return".value, writer);
            if (function.body) |block| {
                try writer.writeAll(" ");
                try print_block(src, block.value, writer);
            }
        },
        .call => |call| {
            try print_expr(src, call.function.value, writer);
            try writer.writeAll("(");
            for (call.arguments.items) |argument| {
                try print_expr(src, argument.value, writer);
                try writer.writeAll(", ");
            }
            try writer.writeAll(")");
        },
        .container => |container| try print_container(src, container.value, writer),
        .ident => |token| try writer.writeAll(token.range.substr(src)),
        .block => |block| try print_block(src, block.value, writer),
        .number => |number| try writer.print("{}", .{number}),
        .parentheses => |parens| {
            try writer.writeAll("(");
            try print_expr(src, parens.value, writer);
            try writer.writeAll(")");
        },
        .unique => |unique| {
            try writer.writeAll("unique ");
            try print_expr(src, unique.value, writer);
        },
        .property => |property| {
            try print_expr(src, property.container.value, writer);
            try writer.print(" {s}", .{property.property.range.substr(src)});
        },
    }
}

fn print_block(src: []const u8, block: Block, writer: anytype) anyerror!void {
    try writer.writeAll("{ ");

    for (block.stmts.items) |stmt| {
        try print_stmt(src, stmt.value, writer);
        try writer.writeAll(" ");
    }

    try writer.writeAll("}");
}

fn print_stmt(src: []const u8, stmt: Stmt, writer: anytype) anyerror!void {
    switch (stmt) {
        .decl => |decl| try print_decl(src, decl, writer),
        .@"return" => |ret| {
            try writer.writeAll("return ");
            try print_expr(src, ret.value, writer);
            try writer.writeAll(";");
        },
    }
}

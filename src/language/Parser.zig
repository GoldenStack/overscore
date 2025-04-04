const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Error = @import("failure.zig").Error;
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;

const Parser = @This();

pub const Container = struct {
    variant: ContainerVariant,
    fields: Fields,
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

pub const Fields = union(TaggedStatus) {
    untagged: std.ArrayList(Ranged(Expr)),
    tagged: std.ArrayList(Ranged(NamedExpr)),
};

/// Represents a field, while parsing. Not directly used in any AST structure.
const Field = union(TaggedStatus) {
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
        parameters: Fields,
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
    distinct: *Ranged(Expr),
    property: struct {
        container: *Ranged(Expr),
        property: Ranged(Token),
    },
    @"if": struct {
        condition: *Ranged(Expr),
        then: *Ranged(Expr),
        @"else": ?*Ranged(Expr),
    },
    do_while: struct {
        do: *Ranged(Expr),
        @"while": *Ranged(Expr),
    },
    while_do: struct {
        do: *Ranged(Expr),
        @"while": *Ranged(Expr),
    },
    @"return": *Ranged(Expr),
};

pub const Block = struct {
    stmts: std.ArrayList(Ranged(Stmt)),
};

pub const Stmt = union(enum) {
    decl: Ranged(Decl),
    expr: Ranged(Expr),
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
    var ctx = .{
        .fields = std.ArrayList(Ranged(Field)).init(self.allocator),
        .decls = std.ArrayList(Ranged(ContainerDecl)).init(self.allocator),
    };

    const read = struct {
        fn read(parser: *Parser, context: *@TypeOf(ctx)) ParsingError!void {
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

    try self.read_iterated_until(null, .closing_curly_bracket, &ctx, read);

    return .{
        .variant = variant,
        .fields = try self.homogenize_fields(ctx.fields, null, .untagged),
        .decls = ctx.decls,
    };
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
        .distinct => .{ .distinct = distinct: {
            _ = self.next();

            break :distinct try self.read_expr_ptr();
        } },
        .@"if" => try self.read_if(),
        .do => try self.read_do_while(),
        .@"while" => try self.read_while_do(),
        .@"return" => {
            _ = self.next();
            return .{ .@"return" = try self.read_expr_ptr() };
        },
        else => return self.fail_expected(&.{ .@"fn", .distinct, .sum, .product, .ident, .opening_curly_bracket, .number, .opening_parentheses, .@"if", .do, .@"while", .@"return" }),
    };
}

pub fn read_do_while(self: *@This()) ParsingError!Expr {
    _ = try self.expect(.do);

    const do = try self.read_expr_ptr();

    _ = try self.expect(.@"while");

    const @"while" = try self.read_expr_ptr();

    return .{ .do_while = .{
        .do = do,
        .@"while" = @"while",
    } };
}

pub fn read_while_do(self: *@This()) ParsingError!Expr {
    _ = try self.expect(.@"while");

    const @"while" = try self.read_expr_ptr();
    
    _ = try self.expect(.do);

    const do = try self.read_expr_ptr();

    return .{ .while_do = .{
        .do = do,
        .@"while" = @"while",
    } };
}

pub fn read_if(self: *@This()) ParsingError!Expr {
    _ = try self.expect(.@"if");

    const condition = try self.read_expr_ptr();

    _ = try self.expect(.then);

    const then = try self.read_expr_ptr();

    const @"else" = if (self.peek().value == .@"else") value: {
        _ = try self.expect(.@"else");

        break :value try self.read_expr_ptr();
    } else null;

    return .{ .@"if" = .{
        .condition = condition,
        .then = then,
        .@"else" = @"else",
    } };

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
        .parameters = try if (body == null)
            self.enforce_untagged_fields(params)
        else
            self.enforce_tagged_fields(params),
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

fn homogenize_fields(self: *@This(), fields: std.ArrayList(Ranged(Field)), expected: ?TaggedStatus, default: TaggedStatus) ParsingError!Fields {
    if (fields.items.len == 0) {
        return switch (expected orelse default) {
            .untagged => .{ .untagged = std.ArrayList(Ranged(Expr)).init(self.allocator) },
            .tagged => .{ .tagged = std.ArrayList(Ranged(NamedExpr)).init(self.allocator) },
        };
    }

    return switch (expected orelse @as(TaggedStatus, fields.items[0].value)) {
        .untagged => try self.enforce_untagged_fields(fields),
        .tagged => self.enforce_tagged_fields(fields),
    };
}

fn enforce_untagged_fields(self: *@This(), fields: std.ArrayList(Ranged(Field))) ParsingError!Fields {
    var list = std.ArrayList(Ranged(Expr)).init(self.allocator);

    if (fields.items.len > 0 and fields.items[0].value != .untagged) {
        return self.fail(.{ .expected_untagged_fields = .{
            .counterexample = fields.items[0].range,
        } });
    }

    for (fields.items) |item| {
        switch (item.value) {
            .untagged => |untagged| try list.append(untagged),
            .tagged => return self.fail(.{ .expected_homogenous_fields = .{
                .untagged_example = fields.items[0].value.untagged.range,
                .tagged_example = item.range,
            } }),
        }
    }

    return .{ .untagged = list };
}

fn enforce_tagged_fields(self: *@This(), fields: std.ArrayList(Ranged(Field))) ParsingError!Fields {
    var list = std.ArrayList(Ranged(NamedExpr)).init(self.allocator);

    if (fields.items.len > 0 and fields.items[0].value != .tagged) {
        return self.fail(.{ .expected_tagged_fields = .{
            .counterexample = fields.items[0].range,
        } });
    }

    for (fields.items) |item| {
        switch (item.value) {
            .untagged => return self.fail(.{ .expected_homogenous_fields = .{
                .untagged_example = item.range,
                .tagged_example = fields.items[0].range,
            } }),
            .tagged => |tagged| {
                for (list.items) |field| {
                    if (std.mem.eql(u8, field.value.name.range.substr(self.src), tagged.name.range.substr(self.src))) {
                        return self.fail(.{ .duplicate_tag = .{
                            .declared = field.value.name.range,
                            .redeclared = tagged.name.range,
                        } });
                    }
                }

                try list.append(item.swap(NamedExpr{
                    .name = tagged.name,
                    .value = tagged.value,
                }));
            },
        }
    }

    return .{ .tagged = list };
}


pub fn read_stmt(self: *@This()) ParsingError!Stmt {
    return switch (self.peek().value) {
        .@"var", .@"const" => .{ .decl = try Ranged(Decl).wrap(self, read_decl) },
        else => .{ .expr = expr: {
            const expr = try self.read_expr();
            _ = try self.expect(.semicolon);
            break :expr expr;
        } },
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
    const start = self.location();
    const token = self.next();

    self.next_token = token;
    self.tokens.loc = start;

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
    try print_fields(src, container.fields, writer);

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

fn print_fields(src: []const u8, fields: Fields, writer: anytype) anyerror!void {
    switch (fields) {
        .untagged => |untagged| {
            for (untagged.items) |field| {
                try print_expr(src, field.value, writer);
                try writer.writeAll(", ");
            }
        },
        .tagged => |tagged| {
            for (tagged.items) |field| {
                try print_named_expr(src, field.value, writer);
                try writer.writeAll(", ");
            }
        },
    }
}

fn print_expr(src: []const u8, expr: Expr, writer: anytype) anyerror!void {
    switch (expr) {
        .function => |function| {
            try writer.writeAll("fn (");
            try print_fields(src, function.parameters, writer);
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
        .distinct => |distinct| {
            try writer.writeAll("distinct ");
            try print_expr(src, distinct.value, writer);
        },
        .property => |property| {
            try print_expr(src, property.container.value, writer);
            try writer.print(" {s}", .{property.property.range.substr(src)});
        },
        .@"if" => |i| {
            try writer.writeAll("if ");
            try print_expr(src, i.condition.value, writer);
            try writer.writeAll(" then ");
            try print_expr(src, i.then.value, writer);
            if (i.@"else") |value| {
                try writer.writeAll(" else ");
                try print_expr(src, value.value, writer);
            }
        },
        .do_while => |do_while| {
            try writer.writeAll("do ");
            try print_expr(src, do_while.do.value, writer);
            try writer.writeAll(" while ");
            try print_expr(src, do_while.@"while".value, writer);
        },
        .while_do => |while_do| {
            try writer.writeAll("while ");
            try print_expr(src, while_do.@"while".value, writer);
            try writer.writeAll(" do ");
            try print_expr(src, while_do.do.value, writer);
        },
        .@"return" => |ret| {
            try writer.writeAll("return ");
            try print_expr(src, ret.value, writer);
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
        .decl => |decl| try print_decl(src, decl.value, writer),
        .expr => |expr| {
            try print_expr(src, expr.value, writer);
            try writer.writeByte(';');
        },
    }
}

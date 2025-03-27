const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const Container = struct {
    unique: bool,
    variant: ContainerVariant,
    fields: Fields,
    decls: std.ArrayList(ContainerDecl),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.unique) try writer.writeAll("unique ");
        if (self.fields == .tagged) try writer.writeAll("tagged ");

        try writer.print("{s} {{ {}", .{ @tagName(self.variant), self.fields });

        for (self.decls.items) |decl| try writer.print("{} ", .{decl});

        try writer.writeAll("}");
    }
};

pub const ContainerVariant = enum {
    sum,
    product,
};

pub const FieldsTag = enum {
    untagged,
    tagged,
};

pub const Fields = union(FieldsTag) {
    untagged: std.ArrayList(Expr),
    tagged: std.ArrayList(NamedExpr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .untagged => |fields| for (fields.items) |item| try writer.print("{}, ", .{item}),
            .tagged => |fields| {
                for (fields.items) |item| {
                    try writer.print("{s}: {}, ", .{ item.name.value, item.value });
                }
            },
        }
    }
};

pub const NamedExpr = struct {
    name: tokenizer.Token,
    value: Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}: {}", .{ self.name.value, self.value });
    }
};

pub const ContainerDecl = struct {
    access: Access,
    decl: Decl,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {}", .{ @tagName(self.access), self.decl });
    }
};

pub const Decl = struct {
    mutability: Mutability,
    name: tokenizer.Token,
    value: Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {s} = {};", .{ @tagName(self.mutability), self.name.value, self.value });
    }
};

pub const Access = enum {
    private,
    public,
};

pub const Mutability = enum {
    constant,
    variable,
};

pub const ExprTag = enum {
    function,
    call,
    container,
    ident,
    block,
    number,
    parentheses,
};

pub const Expr = union(ExprTag) {
    function: Function,
    call: Call,
    container: Container,
    ident: tokenizer.Token,
    block: Block,
    number: u128,
    parentheses: *Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .function => |function| try writer.print("{}", .{function}),
            .call => |call| try writer.print("{}", .{call}),
            .container => |container| try writer.print("{}", .{container}),
            .ident => |token| try writer.print("{s}", .{token.value}),
            .block => |block| try writer.print("{}", .{block}),
            .number => |number| try writer.print("{}", .{number}),
            .parentheses => |parens| try writer.print("({})", .{parens}),
        }
    }
};

pub const Function = struct {
    parameters: std.ArrayList(NamedExpr),
    @"return": *Expr,
    body: Block,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("fn (");

        for (self.parameters.items) |param| try writer.print("{}, ", .{param});

        try writer.print(") {} {}", .{ self.@"return", self.body });
    }
};

pub const Call = struct {
    function: *Expr,
    arguments: std.ArrayList(Expr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{}(", .{self.function});

        for (self.arguments.items) |argument| try writer.print("{}, ", .{argument});

        try writer.writeAll(")");
    }
};

pub const Block = struct {
    stmts: std.ArrayList(Stmt),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("{ ");

        for (self.stmts.items) |stmt| try writer.print("{} ", .{stmt});

        try writer.writeAll("}");
    }
};

pub const StmtTag = enum {
    decl,
    @"return",
};

pub const Stmt = union(StmtTag) {
    decl: Decl,
    @"return": Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .decl => |decl| try writer.print("{}", .{decl}),
            .@"return" => |ret| try writer.print("return {};", .{ret}),
        }
    }
};

/// Every possible error that can occur while parsing.
pub const Error = enum {
    expected_tag,
    number_too_large,
};

/// The context for an error that can occur while parsing.
pub const ErrorContext = union(Error) {
    expected_tag: struct {
        expected: []const tokenizer.Token.Tag,
        found: tokenizer.Token.Tag,
    },
    number_too_large: []const u8,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .expected_tag => |value| {
                const expected = value.expected;

                switch (expected.len) {
                    0 => @panic("Expected at least one argument to expect"),
                    1 => try writer.print("Expected {}", .{expected[0]}),
                    2 => try writer.print("Expected {} or {}", .{ expected[0], expected[1] }),
                    else => {
                        for (0.., expected) |index, tag| {
                            if (index != 0) try writer.writeAll(", ");

                            try writer.print("{}", .{tag});
                        }
                    },
                }

                try writer.print(", found {}", .{value.found});
            },
            .number_too_large => |number| try writer.print("Number \"{s}\" too large to store ", .{number}),
        }
    }
};

/// The error set of errors that can occur while parsing.
pub const ParsingError = error{
    ParsingError,
    OutOfMemory,
};

tokens: tokenizer.Tokenizer,
allocator: std.mem.Allocator,

next_token: ?tokenizer.Token = null,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.ParsingError, this value is significant. Otherwise, it may
/// contain anything.
error_context: ?ErrorContext = null,

pub fn init(tokens: tokenizer.Tokenizer, allocator: std.mem.Allocator) @This() {
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn read_root(self: *@This()) ParsingError!Container {
    return self.read_tagged_container(true, .product);
}

pub fn read_container(self: *@This()) ParsingError!Container {
    var token = try self.expect_many(&.{ .unique, .tagged, .sum, .product });

    const unique = token.tag == .unique;
    if (unique) token = try self.expect_many(&.{ .tagged, .sum, .product });

    const tagged = token.tag == .tagged;
    if (tagged) token = try self.expect_many(&.{ .sum, .product });

    const variant: ContainerVariant = switch (token.tag) {
        .sum => .sum,
        .product => .product,
        else => unreachable,
    };

    _ = try self.expect(.@"{");

    const container = try if (tagged) self.read_tagged_container(unique, variant) else self.read_untagged_container(unique, variant);

    _ = try self.expect(.@"}");

    return container;
}

pub fn read_untagged_container(self: *@This(), unique: bool, variant: ContainerVariant) ParsingError!Container {
    const Parser = @This();
    var container: Container = .{
        .unique = unique,
        .variant = variant,
        .decls = std.ArrayList(ContainerDecl).init(self.allocator),
        .fields = .{ .untagged = std.ArrayList(Expr).init(self.allocator) },
    };

    const read = struct {
        fn read(parser: *Parser, context: *Container) ParsingError!void {
            switch (parser.peek().tag) {
                .@"pub", .@"const", .@"var" => try context.decls.append(try parser.read_container_decl()),
                else => {
                    try context.fields.untagged.append(try parser.read_expr());

                    if (parser.peek().tag != .@"}") _ = try parser.expect(.@",");
                },
            }
        }
    }.read;

    try self.read_iterated_until(null, .@"}", &container, read);

    return container;
}

pub fn read_tagged_container(self: *@This(), unique: bool, variant: ContainerVariant) ParsingError!Container {
    const Parser = @This();
    var container: Container = .{
        .unique = unique,
        .variant = variant,
        .decls = std.ArrayList(ContainerDecl).init(self.allocator),
        .fields = .{ .tagged = std.ArrayList(NamedExpr).init(self.allocator) },
    };

    const read = struct {
        fn read(parser: *Parser, context: *Container) ParsingError!void {
            const token = parser.peek();

            switch (token.tag) {
                .@"pub", .@"const", .@"var" => try context.decls.append(try parser.read_container_decl()),
                .ident => {
                    try context.fields.tagged.append(try parser.read_named_expr());

                    // This makes the last field mandatory in file containers
                    if (parser.peek().tag != .@"}") _ = try parser.expect(.@",");
                },
                else => return parser.fail_expected(&.{ .@"{", .@"pub", .@"const", .@"var", .ident }),
            }
        }
    }.read;

    try self.read_iterated_until(null, .@"}", &container, read);

    return container;
}

pub fn read_container_decl(self: *@This()) ParsingError!ContainerDecl {
    const access: Access = if (self.peek().tag == .@"pub") .public else .private;
    if (access == .public) _ = self.next();

    return .{
        .access = access,
        .decl = try self.read_decl(),
    };
}

pub fn read_decl(self: *@This()) ParsingError!Decl {
    const token = try self.expect_many(&.{ .@"const", .@"var" });

    const mutability: Mutability = if (token.tag == .@"const") .constant else .variable;

    const name = try self.expect(.ident);

    _ = try self.expect(.@"=");

    const value = try self.read_expr();

    _ = try self.expect(.@";");

    return .{ .mutability = mutability, .name = name, .value = value };
}

pub fn read_expr(self: *@This()) ParsingError!Expr {
    var expr: Expr = switch (self.peek().tag) {
        .@"fn" => .{ .function = try self.read_function() },
        .unique, .tagged, .sum, .product => .{ .container = try self.read_container() },
        .ident => .{ .ident = self.next() },
        .@"{" => .{ .block = try self.read_block() },
        .number => .{ .number = try self.read_number() },
        .@"(" => .{ .parentheses = parens: {
            _ = self.next();

            const boxed = try self.allocator.create(Expr);
            boxed.* = try self.read_expr();

            _ = try self.expect(.@")");
            break :parens boxed;
        } },
        else => return self.fail_expected(&.{ .@"fn", .unique, .tagged, .sum, .product, .ident, .@"{", .number }),
    };

    // Handle postfix operator (function calling)
    while (self.peek().tag == .@"(") {
        expr = .{ .call = try self.read_parameters(expr) };
    }

    return expr;
}

pub fn read_parameters(self: *@This(), function: Expr) ParsingError!Call {
    _ = try self.expect(.@"(");

    const Parser = @This();

    var args = std.ArrayList(Expr).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Expr)) ParsingError!void {
            try context.append(try parser.read_expr());
        }
    }.read;

    try self.read_iterated_until(.@",", .@")", &args, read);

    _ = try self.expect(.@")");
    
    const boxed = try self.allocator.create(Expr);
    boxed.* = function;

    return .{
        .function = boxed,
        .arguments = args,
    };
}

pub fn read_function(self: *@This()) ParsingError!Function {
    _ = try self.expect(.@"fn");
    _ = try self.expect(.@"(");

    const Parser = @This();

    var params = std.ArrayList(NamedExpr).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(NamedExpr)) ParsingError!void {
            try context.append(try parser.read_named_expr());
        }
    }.read;

    try self.read_iterated_until(.@",", .@")", &params, read);

    _ = try self.expect(.@")");

    const boxed = try self.allocator.create(Expr);
    boxed.* = try self.read_expr();

    const body = try self.read_block();

    return .{
        .parameters = params,
        .@"return" = boxed,
        .body = body,
    };
}

pub fn read_block(self: *@This()) ParsingError!Block {
    _ = try self.expect(.@"{");

    const Parser = @This();

    var stmts = std.ArrayList(Stmt).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Stmt)) ParsingError!void {
            try context.append(try parser.read_statement());
        }
    }.read;

    try self.read_iterated_until(null, .@"}", &stmts, read);

    _ = try self.expect(.@"}");

    return .{ .stmts = stmts };
}

pub fn read_named_expr(self: *@This()) ParsingError!NamedExpr {
    const name = try self.expect(.ident);

    _ = try self.expect(.@":");

    const value = try self.read_expr();

    return .{
        .name = name,
        .value = value,
    };
}

pub fn read_statement(self: *@This()) ParsingError!Stmt {
    return switch (self.peek().tag) {
        .@"return" => {
            _ = self.next();

            const expr = try self.read_expr();

            _ = try self.expect(.@";");

            return .{ .@"return" = expr };
        },
        else => .{ .decl = try self.read_decl() },
    };
}

pub fn read_number(self: *@This()) ParsingError!u128 {
    const token = try self.expect(.number);

    return std.fmt.parseUnsigned(u128, token.value, 10) catch self.fail(.{ .number_too_large = token.value });
}

fn read_iterated_until(self: *@This(), comptime maybe_sep: ?tokenizer.Token.Tag, end: tokenizer.Token.Tag, context: anytype, reader: fn (*@This(), @TypeOf(context)) ParsingError!void) !void {
    // We just started reading, so we don't need a separator
    var sep_last_iter = true;

    while (true) {

        // Exit if the next token doesn't exist or if it's `end`
        const token = self.peek();
        if (token.tag == .eof or token.tag == end) return;

        // If there wasn't a separator, fail, having expected one
        // This check is placed after the exits so that a separator is optional
        // for the last argument
        if (maybe_sep) |sep| if (!sep_last_iter) return self.fail_expected(&.{sep});

        // Add the item and reset the separator tracker
        try reader(self, context);

        if (maybe_sep == null) continue;

        // Read the next token if it's the separator
        if (self.peek().tag == maybe_sep) {
            _ = self.next();
            sep_last_iter = true;
        } else sep_last_iter = false;
    }
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": ErrorContext) error{ParsingError} {
    self.error_context = @"error";
    return error.ParsingError;
}

pub fn fail_expected(self: *@This(), comptime tags: []const tokenizer.Token.Tag) error{ParsingError} {
    return self.fail(.{ .expected_tag = .{
        .expected = tags,
        .found = self.peek().tag,
    } });
}

/// Reads a token, returning an error if it's not equal to the given tag.
pub fn expect(self: *@This(), comptime tag: tokenizer.Token.Tag) !tokenizer.Token {
    return self.expect_many(&.{tag});
}

/// Reads a token, returning an error if it's not one of the given tags.
pub fn expect_many(self: *@This(), comptime tags: []const tokenizer.Token.Tag) !tokenizer.Token {
    const next_tag = self.peek().tag;

    inline for (tags) |tag| {
        if (tag == next_tag) return self.next();
    } else return self.fail_expected(tags);
}

/// Returns the next token from the backing token iterator without advancing the
/// iterator itself. This value is cached, so peeking does not require
/// re-reading.
pub fn peek(self: *@This()) tokenizer.Token {
    // Return the cached token if possible
    if (self.next_token) |token| return token;

    // Load the next token and backtrack.
    // This could be structured so that next() depends on peek(), but this would
    // mean that there's always hidden and unnecessary backtracking, which is
    // not ideal.
    const token = self.next();

    self.next_token = token;
    self.tokens.loc = token.start;

    return token;
}

/// Reads the next token from the backing token iterator.
pub fn next(self: *@This()) tokenizer.Token {
    // Return the cached token if possible
    if (self.next_token) |token| {
        self.tokens.loc = token.end;
        self.next_token = null;
        return token;
    }

    // Skip tokens until there's a non-comment
    while (true) {
        const token = self.tokens.next();
        // Comments mean nothing for now
        if (token.tag != .comment) return token;
    }
}

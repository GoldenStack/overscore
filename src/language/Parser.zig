const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

pub const Container = struct {
    unique: bool,
    variant: ContainerVariant,
    fields: Fields,
    members: std.ArrayList(ContainerDecl),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.unique) try writer.writeAll("unique ");
        if (self.fields == .tagged) try writer.writeAll("tagged ");

        try writer.print("{s} {{ {}", .{ @tagName(self.variant), self.fields });

        for (self.members.items) |member| try writer.print("{} ", .{member});

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
    tagged: std.StringHashMap(Expr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .untagged => |fields| for (fields.items) |item| try writer.print("{}, ", .{item}),
            .tagged => |fields| {
                var iter = fields.iterator();
                while (iter.next()) |item| try writer.print("{s}: {}, ", .{ item.key_ptr.*, item.value_ptr });
            },
        }
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
    name: Tokenizer.Token,
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
    container,
    ident,
    block,
};

pub const Expr = union(ExprTag) {
    function: Function,
    container: Container,
    ident: Tokenizer.Token,
    block: Block,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .function => |function| try writer.print("{}", .{function}),
            .container => |container| try writer.print("{}", .{container}),
            .ident => |token| try writer.print("{s}", .{token.value}),
            .block => |block| try writer.print("{}", .{block}),
        }
    }
};

pub const Function = struct {
    parameters: std.ArrayList(Expr),
    @"return": *Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("fn (");

        for (self.parameters.items) |param| try writer.print("{},", .{param});

        try writer.print(") {}", .{self.@"return"});
    }
};

pub const Block = struct {
    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = self;
        try writer.writeAll("{}");
    }
};

/// Every possible error that can occur while parsing.
pub const Error = enum {
    expected_token,
    expected_tokens,
    number_too_large,
};

/// The context for an error that can occur while parsing.
pub const ErrorContext = union(Error) {
    expected_token: Tokenizer.TokenType,
    expected_tokens: []const Tokenizer.TokenType,
    number_too_large: []const u8,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .expected_token => |token| try writer.print("Expected {s}", .{@tagName(token)}),
            .expected_tokens => |tokens| {
                try writer.writeAll("Expected one of ");

                for (0.., tokens) |index, token| {
                    if (index != 0) try writer.writeAll(", ");

                    try writer.print("{s}", .{@tagName(token)});
                }
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

tokens: Tokenizer.TokenIterator,
allocator: std.mem.Allocator,

next_token: ?Tokenizer.Token = null,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.ParsingError, this value is significant. Otherwise, it may
/// contain anything.
error_context: ?ErrorContext = null,

pub fn init(tokens: Tokenizer.TokenIterator, allocator: std.mem.Allocator) @This() {
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn read_root(self: *@This()) ParsingError!Container {
    return self.read_tagged_container(true, .product);
}

pub fn read_container(self: *@This()) ParsingError!Container {
    var token = try self.expect_many(.{ .unique, .tagged, .sum, .product });

    const unique = token.type == .unique;
    if (unique) token = try self.expect_many(.{ .tagged, .sum, .product });

    const tagged = token.type == .tagged;
    if (tagged) token = try self.expect_many(.{ .sum, .product });

    const variant: ContainerVariant = switch (token.type) {
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
    var ctx = .{
        .members = std.ArrayList(ContainerDecl).init(self.allocator),
        .fields = std.ArrayList(Expr).init(self.allocator),
    };

    const read = struct {
        fn read(parser: *Parser, context: *@TypeOf(ctx)) ParsingError!void {
            switch (parser.peek().type) {
                .@"pub", .@"const", .@"var" => try context.members.append(try parser.read_container_decl()),
                else => {
                    try context.fields.append(try parser.read_expr());

                    if (parser.peek().type != .@"}") _ = try parser.expect(.@",");
                },
            }
        }
    }.read;

    try self.read_iterated_until(null, .@"}", &ctx, read);

    return .{
        .unique = unique,
        .variant = variant,
        .fields = .{ .untagged = ctx.fields },
        .members = ctx.members,
    };
}

pub fn read_tagged_container(self: *@This(), unique: bool, variant: ContainerVariant) ParsingError!Container {
    const Parser = @This();
    var ctx = .{
        .members = std.ArrayList(ContainerDecl).init(self.allocator),
        .fields = std.StringHashMap(Expr).init(self.allocator),
    };

    const read = struct {
        fn read(parser: *Parser, context: *@TypeOf(ctx)) ParsingError!void {
            const token = parser.peek();

            switch (token.type) {
                .@"pub", .@"const", .@"var" => try context.members.append(try parser.read_container_decl()),
                .ident => {
                    _ = parser.next(); // Read the token
                    _ = try parser.expect(.@":");

                    try context.fields.put(token.value, try parser.read_expr());

                    if (parser.peek().type != .@"}") _ = try parser.expect(.@",");
                },
                else => return parser.fail(.{ .expected_tokens = &.{ .@"{", .@"pub", .@"const", .@"var", .ident } }),
            }
        }
    }.read;

    try self.read_iterated_until(null, .@"}", &ctx, read);

    return .{
        .unique = unique,
        .variant = variant,
        .fields = .{ .tagged = ctx.fields },
        .members = ctx.members,
    };
}

pub fn read_container_decl(self: *@This()) ParsingError!ContainerDecl {
    const access: Access = if (self.peek().type == .@"pub") .public else .private;
    if (access == .public) _ = self.next();

    return .{
        .access = access,
        .decl = try self.read_decl(),
    };
}

pub fn read_decl(self: *@This()) ParsingError!Decl {
    const token = try self.expect_many(.{ .@"const", .@"var" });

    const mutability: Mutability = if (token.type == .@"const") .constant else .variable;

    const name = try self.expect(.ident);

    _ = try self.expect(.@"=");

    const value = try self.read_expr();

    _ = try self.expect(.@";");

    return .{ .mutability = mutability, .name = name, .value = value };
}

pub fn read_expr(self: *@This()) ParsingError!Expr {
    return switch (self.peek().type) {
        .@"fn" => .{ .function = try self.read_function() },
        .unique, .tagged, .sum, .product => .{ .container = try self.read_container() },
        .ident => .{ .ident = self.next() },
        .@"{" => .{ .block = try self.read_block() },
        else => return self.fail(.{ .expected_tokens = &.{ .@"fn", .unique, .tagged, .sum, .product, .ident, .@"{" } }),
    };
}

pub fn read_function(self: *@This()) ParsingError!Function {
    _ = try self.expect(.@"fn");
    _ = try self.expect(.@"(");

    const Parser = @This();

    var params = std.ArrayList(Expr).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Expr)) ParsingError!void {
            try context.append(try parser.read_expr());
        }
    }.read;

    try self.read_iterated_until(.@",", .@")", &params, read);

    _ = try self.expect(.@")");

    const boxed = try self.allocator.create(Expr);
    boxed.* = try self.read_expr();

    return .{
        .parameters = params,
        .@"return" = boxed,
    };
}

pub fn read_block(self: *@This()) ParsingError!Block {
    _ = try self.expect(.@"{");
    _ = try self.expect(.@"}");

    return .{};
}

pub fn read_number(self: *@This()) ParsingError!u32 {
    const token = try self.expect(.number);

    return std.fmt.parseUnsigned(u32, token.value, 10) catch self.fail(.{ .number_too_large = token.value });
}

fn read_iterated_until(self: *@This(), comptime maybe_sep: ?Tokenizer.TokenType, end: Tokenizer.TokenType, context: anytype, reader: fn (*@This(), @TypeOf(context)) ParsingError!void) !void {
    // We just started reading, so we don't need a separator
    var sep_last_iter = true;

    while (true) {

        // Exit if the next token doesn't exist or if it's `end`
        const token = self.peek();
        if (token.type == .eof or token.type == end) return;

        // If there wasn't a separator, fail, having expected one
        // This check is placed after the exits so that a separator is optional
        // for the last argument
        if (maybe_sep) |sep| if (!sep_last_iter) return self.fail(.{ .expected_token = sep });

        // Add the item and reset the separator tracker
        try reader(self, context);

        if (maybe_sep == null) continue;

        // Read the next token if it's the separator
        if (self.peek().type == maybe_sep) {
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

/// Reads a token, expecting it to be the provided type, returning an error if
/// it's not.
pub fn expect(self: *@This(), @"type": Tokenizer.TokenType) !Tokenizer.Token {
    return if (self.peek().type == @"type")
        self.next()
    else
        self.fail(.{ .expected_token = @"type" });
}

/// Reads a token, expecting it to be one of the provided token types, returning
/// an error if it's not one of those types.
pub fn expect_many(self: *@This(), types: anytype) !Tokenizer.Token {
    return if (std.mem.indexOfScalar(Tokenizer.TokenType, &types, self.peek().type) != null)
        self.next()
    else
        return self.fail(.{ .expected_tokens = &types });
}

/// Returns the next token from the backing token iterator without advancing the
/// iterator itself. This value is cached, so peeking does not require
/// re-reading.
pub fn peek(self: *@This()) Tokenizer.Token {
    // Return the cached token if possible
    if (self.next_token) |token| return token;

    // Load the next token and backtrack.
    // This could be structured so that next() depends on peek(), but this would
    // mean that there's always hidden and unnecessary backtracking, which is
    // not ideal.
    const token = self.next();

    self.next_token = token;
    self.tokens.pos = token.start.pos;

    return token;
}

/// Reads the next token from the backing token iterator.
pub fn next(self: *@This()) Tokenizer.Token {
    // Return the cached token if possible
    if (self.next_token) |token| {
        self.tokens.pos = token.end.pos;
        self.next_token = null;
        return token;
    }

    // Skip tokens until there's a non-comment
    while (true) {
        const token = self.tokens.next();
        // Comments mean nothing for now
        if (token.type != .comment) return token;
    }
}

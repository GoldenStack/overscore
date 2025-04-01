const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Error = @import("failure.zig").Error;

const Parser = @This();

pub const Container = struct {
    variant: ContainerVariant,
    fields: std.ArrayList(Range(Field)),
    decls: std.ArrayList(Range(ContainerDecl)),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {{ ", .{@tagName(self.variant)});

        for (self.fields.items) |field| try writer.print("{}, ", .{field});

        for (self.decls.items) |decl| try writer.print("{} ", .{decl});

        try writer.writeAll("}");
    }
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
    untagged: Range(Expr),
    tagged: NamedExpr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .untagged => |field| try writer.print("{}", .{field}),
            .tagged => |field| try writer.print("{}", .{field}),
        }
    }
};

pub const NamedExpr = struct {
    name: tokenizer.Token,
    value: Range(Expr),

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
    decl: Range(Decl),

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
    value: Range(Expr),

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

pub const Expr = union(enum) {
    function: struct {
        parameters: std.ArrayList(Range(Field)),
        @"return": *Range(Expr),
        body: ?Range(Block),
    },
    call: struct {
        function: *Range(Expr),
        arguments: std.ArrayList(Range(Expr)),
    },
    container: Range(Container),
    ident: tokenizer.Token,
    block: Range(Block),
    number: u32,
    parentheses: *Range(Expr),
    unique: *Range(Expr),
    property: struct {
        container: *Range(Expr),
        property: tokenizer.Token,
    },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .function => |function| {
                try writer.writeAll("fn (");
                for (function.parameters.items) |param| try writer.print("{}, ", .{param});
                try writer.print(") {}", .{function.@"return"});
                if (function.body) |block| try writer.print(" {}", .{block});
            },
            .call => |call| {
                try writer.print("{}(", .{call.function});
                for (call.arguments.items) |argument| try writer.print("{}, ", .{argument});
                try writer.writeAll(")");
            },
            .container => |container| try writer.print("{}", .{container}),
            .ident => |token| try writer.print("{s}", .{token.value}),
            .block => |block| try writer.print("{}", .{block}),
            .number => |number| try writer.print("{}", .{number}),
            .parentheses => |parens| try writer.print("({})", .{parens}),
            .unique => |unique| try writer.print("unique {}", .{unique}),
            .property => |property| try writer.print("{}.{s}", .{ property.container, property.property }),
        }
    }
};

pub const Block = struct {
    stmts: std.ArrayList(Range(Stmt)),

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

pub const Stmt = union(enum) {
    decl: Decl,
    @"return": Range(Expr),

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
error_context: ?Error = null,

pub fn init(tokens: tokenizer.Tokenizer, allocator: std.mem.Allocator) @This() {
    return .{
        .tokens = tokens,
        .allocator = allocator,
    };
}

pub fn read_root(self: *@This()) ParsingError!Range(Container) {
    return Range(Container).wrap(self, struct {
        fn read(parser: *Parser) ParsingError!Container {
            return parser.read_container_contents(.product);
        }
    }.read);
}

pub fn read_container(self: *@This()) ParsingError!Container {
    const token = try self.expect_many(&.{ .sum, .product });

    const variant: ContainerVariant = switch (token.tag) {
        .sum => .sum,
        .product => .product,
        else => unreachable,
    };

    _ = try self.expect(.@"{");

    const container = try self.read_container_contents(variant);

    _ = try self.expect(.@"}");

    return container;
}

pub fn read_container_contents(self: *@This(), variant: ContainerVariant) ParsingError!Container {
    var container: Container = .{
        .variant = variant,
        .fields = std.ArrayList(Range(Field)).init(self.allocator),
        .decls = std.ArrayList(Range(ContainerDecl)).init(self.allocator),
    };

    const read = struct {
        fn read(parser: *Parser, context: *Container) ParsingError!void {
            switch (parser.peek().tag) {
                .@"pub", .@"const", .@"var" => {
                    try context.decls.append(try Range(ContainerDecl).wrap(parser, read_container_decl));
                },
                else => {
                    try context.fields.append(try Range(Field).wrap(parser, read_field));

                    // This makes the last comma mandatory in file containers
                    if (parser.peek().tag != .@"}") _ = try parser.expect(.@",");
                },
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
        .decl = try Range(Decl).wrap(self, read_decl),
    };
}

pub fn read_decl(self: *@This()) ParsingError!Decl {
    const token = try self.expect_many(&.{ .@"const", .@"var" });

    const mutability: Mutability = if (token.tag == .@"const") .constant else .variable;

    const name = try self.expect(.ident);

    _ = try self.expect(.@"=");

    const value = try self.read_expr();

    _ = try self.expect(.@";");

    return .{
        .mutability = mutability,
        .name = name,
        .value = value,
    };
}

pub fn read_expr_ptr(self: *@This()) ParsingError!*Range(Expr) {
    const ptr = try self.allocator.create(Range(Expr));
    ptr.* = try self.read_expr();
    return ptr;
}

pub fn read_expr(self: *@This()) ParsingError!Range(Expr) {
    var info = try Range(Expr).wrap(self, read_expr_raw);

    // Handle non-prefix operators
    while (true) {
        info = switch (self.peek().tag) {
            .@"(" => try info.extend(Expr, self, read_parameters),
            .@"." => try info.extend(Expr, self, read_property),
            else => break,
        };
    }

    return info;
}

fn read_expr_raw(self: *@This()) ParsingError!Expr {
    return switch (self.peek().tag) {
        .@"fn" => try self.read_function(),
        .sum, .product => .{ .container = try Range(Container).wrap(self, read_container) },
        .ident => .{ .ident = self.next() },
        .@"{" => .{ .block = try Range(Block).wrap(self, read_block) },
        .number => .{ .number = try self.read_number() },
        .@"(" => .{ .parentheses = parens: {
            _ = self.next();

            const expr = try self.read_expr_ptr();

            _ = try self.expect(.@")");
            break :parens expr;
        } },
        .unique => .{ .unique = unique: {
            _ = self.next();

            break :unique try self.read_expr_ptr();
        } },
        else => return self.fail_expected(&.{ .@"fn", .unique, .sum, .product, .ident, .@"{", .number, .unique }),
    };
}

pub fn read_property(self: *@This(), container: Range(Expr)) ParsingError!Expr {
    _ = try self.expect(.@".");
    const property = try self.expect(.ident);

    const ptr = try self.allocator.create(Range(Expr));
    ptr.* = container;

    return .{ .property = .{
        .container = ptr,
        .property = property,
    } };
}

pub fn read_parameters(self: *@This(), function: Range(Expr)) ParsingError!Expr {
    _ = try self.expect(.@"(");

    var args = std.ArrayList(Range(Expr)).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Range(Expr))) ParsingError!void {
            try context.append(try parser.read_expr());
        }
    }.read;

    try self.read_iterated_until(.@",", .@")", &args, read);

    _ = try self.expect(.@")");

    const ptr = try self.allocator.create(Range(Expr));
    ptr.* = function;

    return .{ .call = .{
        .function = ptr,
        .arguments = args,
    } };
}

pub fn read_function(self: *@This()) ParsingError!Expr {
    _ = try self.expect(.@"fn");
    _ = try self.expect(.@"(");

    var params = std.ArrayList(Range(Field)).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Range(Field))) ParsingError!void {
            try context.append(try Range(Field).wrap(parser, read_field));
        }
    }.read;

    try self.read_iterated_until(.@",", .@")", &params, read);

    _ = try self.expect(.@")");

    const ret = try self.read_expr_ptr();

    const body = if (self.peek().tag == .@"{")
        try Range(Block).wrap(self, read_block)
    else
        null;

    return .{ .function = .{
        .parameters = params,
        .@"return" = ret,
        .body = body,
    } };
}

pub fn read_block(self: *@This()) ParsingError!Block {
    _ = try self.expect(.@"{");

    var stmts = std.ArrayList(Range(Stmt)).init(self.allocator);

    const read = struct {
        fn read(parser: *Parser, context: *std.ArrayList(Range(Stmt))) ParsingError!void {
            try context.append(try Range(Stmt).wrap(parser, read_stmt));
        }
    }.read;

    try self.read_iterated_until(null, .@"}", &stmts, read);

    _ = try self.expect(.@"}");

    return .{ .stmts = stmts };
}

pub fn read_field(self: *@This()) ParsingError!Field {
    const expr = try self.read_expr();

    if (self.peek().tag == .@":") { // Tagged
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

    _ = try self.expect(.@":");

    const value = try self.read_expr();

    return .{
        .name = name,
        .value = value,
    };
}

pub fn read_stmt(self: *@This()) ParsingError!Stmt {
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

pub fn read_number(self: *@This()) ParsingError!u32 {
    const token = try self.expect(.number);

    return std.fmt.parseUnsigned(u32, token.value, 10) catch self.fail(.{ .number_too_large = token });
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
pub fn fail(self: *@This(), @"error": Error) error{ParsingError} {
    self.error_context = @"error";
    return error.ParsingError;
}

pub fn fail_expected(self: *@This(), comptime tags: []const tokenizer.Token.Tag) error{ParsingError} {
    return self.fail(.{ .expected_tag = .{
        .expected = tags,
        .found = self.peek(),
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
    self.tokens.loc = token.range.start;

    return token;
}

/// Reads the next token from the backing token iterator.
pub fn next(self: *@This()) tokenizer.Token {
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
        if (token.tag != .comment) return token;
    }
}

pub fn Range(T: type) type {
    return struct {
        range: tokenizer.Range,
        value: T,

        pub fn wrap(parser: *Parser, read: fn (*Parser) ParsingError!T) ParsingError!@This() {
            const start = parser.tokens.loc;

            const value = try read(parser);

            return .{
                .range = .{
                    .start = start,
                    .end = parser.tokens.loc,
                },
                .value = value,
            };
        }
        
        pub fn map(self: @This(), context: anytype, mapping: anytype) MapErrorSuccess(@TypeOf(mapping), Range) {
            return .{
                .range = self.range,
                .value = try mapping(context, self.value),
            };
        }

        fn MapErrorSuccess(mapper: type, mapping: fn(type) type) type {
            const return_type = @typeInfo(mapper).@"fn".return_type.?;

            const error_union = @typeInfo(return_type).error_union;

            return error_union.error_set!mapping(error_union.payload);
        }

        pub fn swap(self: @This(), new_value: anytype) Range(@TypeOf(new_value)) {
            return .{
                .range = self.range,
                .value = new_value,
            };
        }

        pub fn extend(self: @This(), N: type, parser: *Parser, read: fn (*Parser, @This()) ParsingError!N) ParsingError!Range(N) {
            const value = try read(parser, self);

            return .{
                .range = .{
                    .start = self.range.start,
                    .end = parser.tokens.loc,
                },
                .value = value,
            };
        }

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{}", .{self.value});
        }
    };
}

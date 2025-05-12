const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;

/// The abstract syntax tree.
pub const ast = struct {
    /// A container, containing a list of definitions.
    pub const Container = struct {
        defs: std.ArrayList(Ranged(Def)),
    };

    /// A definition of a name, consisting of an access modifier, a mutability
    /// modifier, a name, an optional type, and value.
    pub const Def = struct {
        access: Access,
        mutability: Mutability,
        name: Ranged(Token),

        type: ?Ranged(Expr),
        value: Ranged(Expr),
    };

    /// A declaration of a name, consisting of a name and a type, but no value.
    pub const Decl = struct {
        name: Ranged(Token),

        type: Ranged(*Expr),
    };

    /// An access modifier - either public or private.
    pub const Access = enum {
        private,
        public,
    };

    /// A mutability modifier - either constant or variable.
    pub const Mutability = enum {
        constant,
        variable,
    };

    pub const Expr = union(enum) {
        /// The primitive value that the CPU can handle, an unsigned 32-bit integer.
        word: u32,

        /// An integer type, the CPU "word". For now, integers are always unsigned
        /// 32-bit integers, so they are referred to as words to reflect the
        /// intention of this to be changed.
        word_type,

        decl: Decl,

        product: std.ArrayList(Ranged(Expr)),

        sum: std.ArrayList(Ranged(Expr)),

        /// A pointer type.
        pointer_type: Ranged(*Expr),

        /// A type.
        ///
        /// When the value of an expression is a type, the type of the expression is
        /// `type`. This does lead to unsoundness in the type system due to
        /// Russell's paradox, likely the only point of unsoundness, but I don't
        /// really care.
        type,

        /// A container. See `Container` documentation for more details.
        container: Container,

        /// An identifier that contains a value.
        ident: Ranged(Token),

        /// An expression that is being dereferenced.
        dereference: Ranged(*Expr),

        /// An expression that has been wrapped with parentheses.
        parentheses: Ranged(*Expr),

        /// Represents accessing a member of a container.
        member_access: struct {
            container: Ranged(*Expr),
            member: Ranged(Token),
        },
    };

    pub fn printContainer(src: []const u8, container: Container, writer: anytype) anyerror!void {
        try writer.writeAll("{ ");

        for (container.defs.items) |def| {
            try printDef(src, def.value, writer);
            try writer.writeByte(' ');
        }

        try writer.writeByte('}');
    }

    pub fn printDef(src: []const u8, def: Def, writer: anytype) anyerror!void {
        if (def.access == .public) try writer.writeAll("pub ");

        try writer.writeAll(switch (def.mutability) {
            .constant => "const",
            .variable => "var",
        });
        try writer.writeByte(' ');

        try printToken(src, def.name, writer);

        if (def.type) |@"type"| {
            try writer.writeAll(": ");
            try printExpr(src, @"type".value, writer);
        }

        try writer.writeAll(" = ");
        try printExpr(src, def.value.value, writer);
        try writer.writeAll(";");
    }

    pub fn printDecl(src: []const u8, decl: Decl, writer: anytype) anyerror!void {
        try printToken(src, decl.name, writer);

        try writer.writeAll(": ");
        try printExpr(src, decl.type.value.*, writer);
    }

    pub fn printExpr(src: []const u8, expr: Expr, writer: anytype) anyerror!void {
        switch (expr) {
            .word => |word| try writer.print("{}", .{word}),
            .word_type => try writer.writeAll("word"),
            .type => try writer.writeAll("type"),
            .decl => |decl| try printDecl(src, decl, writer),
            .product => |decls| {
                try writer.writeAll("(");
                for (0.., decls.items) |index, decl| {
                    if (index != 0) try writer.writeAll(" ** ");
                    try printExpr(src, decl.value, writer);
                }
                try writer.writeAll(")");
            },
            .sum => |decls| {
                try writer.writeAll("(");
                for (0.., decls.items) |index, decl| {
                    if (index != 0) try writer.writeAll(" ++ ");
                    try printExpr(src, decl.value, writer);
                }
                try writer.writeAll(")");
            },
            .pointer_type => |ptr| {
                try writer.writeByte('*');
                try printExpr(src, ptr.value.*, writer);
            },
            .container => |container| try printContainer(src, container, writer),
            .ident => |ident| try printToken(src, ident, writer),
            .dereference => |deref| {
                try printExpr(src, deref.value.*, writer);
                try writer.writeAll(".*");
            },
            .parentheses => |parens| {
                try writer.writeByte('(');
                try printExpr(src, parens.value.*, writer);
                try writer.writeByte(')');
            },
            .member_access => |member| {
                try printExpr(src, member.container.value.*, writer);
                try writer.writeByte('.');
                try printToken(src, member.member, writer);
            },
        }
    }

    pub fn printToken(src: []const u8, token: Ranged(Token), writer: anytype) anyerror!void {
        try writer.writeAll(token.range.substr(src));
    }
};

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

tokens: tokenizer.Tokenizer,
next_token: ?Ranged(Token) = null,

pub fn init(allocator: std.mem.Allocator, tokens: tokenizer.Tokenizer) @This() {
    return .{
        .src = tokens.src,
        .tokens = tokens,
        .allocator = allocator,
    };
}

/// Reads the root container from this parser. This will consume the entire
/// sorce file unless there is an error.
pub fn readRoot(self: *@This()) Err!Ranged(ast.Expr) {
    return Ranged(ast.Expr).wrap(self, readRootRaw);
}

fn readRootRaw(self: *@This()) Err!ast.Expr {
    var defs = std.ArrayList(Ranged(ast.Def)).init(self.allocator);

    while (self.peek().value != .eof) {
        try defs.append(try Ranged(ast.Def).wrap(self, readDef));
    }

    return .{ .container = .{ .defs = defs } };
}

/// Reads a container from this parser.
pub fn readContainer(self: *@This()) Err!ast.Container {
    _ = try self.expect(.container);
    _ = try self.expect(.opening_curly_bracket);

    var defs = std.ArrayList(Ranged(ast.Def)).init(self.allocator);

    while (self.peek().value != .closing_curly_bracket) {
        try defs.append(try Ranged(ast.Def).wrap(self, readDef));
    }

    _ = try self.expect(.closing_curly_bracket);

    return .{
        .defs = defs,
    };
}

/// Parses a name definition from this parser.
pub fn readDef(self: *@This()) Err!ast.Def {
    const access: ast.Access = if (self.consume(.@"pub")) |_| .public else .private;

    const mutability: ast.Mutability = switch ((try self.expectMany(&.{ .@"const", .@"var" })).value) {
        .@"const" => .constant,
        .@"var" => .variable,
        else => unreachable,
    };

    const name = try self.expect(.ident);

    // Type specifier is mandatory for now.
    const @"type" = if (self.peek().value == .colon) type_specifier: {
        _ = try self.expect(.colon);

        break :type_specifier try self.readExpr();
    } else null;

    _ = try self.expect(.equals);

    const value = try self.readExpr();

    _ = try self.expect(.semicolon);

    return .{
        .access = access,
        .mutability = mutability,
        .name = name,
        .type = @"type",
        .value = value,
    };
}

fn box(self: *@This(), T: type, ranged: Ranged(T)) Err!Ranged(*T) {
    const ptr = try self.allocator.create(T);
    ptr.* = ranged.value;
    return ranged.swap(ptr);
}

fn composeExpr1(self: *@This()) Err!Ranged(ast.Expr) {
    var expr = try self.composeExpr2();

    while (true) {
        expr = sw: switch (self.peek().value) {
            .asterisk => {
                const first = try self.expect(.asterisk);
                const second = try self.expect(.asterisk);

                switch (expr.value) {
                    .product => {
                        try expr.value.product.append(try self.composeExpr2());
                        break :sw expr;
                    },
                    .sum => return self.fail(.{ .mixed_precedence = .{
                        .expr = expr.range,
                        .operator = .{ .start = first.range.start, .end = second.range.end },
                    } }),
                    else => {
                        var product = std.ArrayList(Ranged(ast.Expr)).init(self.allocator);
                        try product.append(expr);
                        try product.append(try self.composeExpr2());
                        break :sw expr.swap(ast.Expr{ .product = product });
                    },
                }
            },
            .plus_plus => { // TODO: Deduplicate code
                const operator = try self.expect(.plus_plus);

                switch (expr.value) {
                    .sum => {
                        try expr.value.sum.append(try self.composeExpr2());
                        break :sw expr;
                    },
                    .product => return self.fail(.{ .mixed_precedence = .{
                        .expr = expr.range,
                        .operator = operator.range,
                    } }),
                    else => {
                        var sum = std.ArrayList(Ranged(ast.Expr)).init(self.allocator);
                        try sum.append(expr);
                        try sum.append(try self.composeExpr2());
                        break :sw expr.swap(ast.Expr{ .sum = sum });
                    },
                }
            },
            else => break,
        };
    }

    return expr;
}

fn composeExpr2(self: *@This()) Err!Ranged(ast.Expr) {
    var expr = try Ranged(ast.Expr).wrap(self, readExprRaw);

    while (true) {
        expr = sw: switch (self.peek().value) {
            .period => try expr.mapExtend(self, readMemberAccess),
            .period_asterisk => try expr.mapExtend(self, readDereference),
            .colon => {
                if (expr.value != .ident) break :sw expr;

                break :sw try expr.mapExtend(self, readDecl);
            },
            else => break,
        };
    }

    return expr;
}

/// Assumes `ident.value == .ident`.
fn readDecl(self: *@This(), ident: Ranged(ast.Expr)) Err!ast.Expr {
    const name = ident.value.ident;

    _ = try self.expect(.colon);

    const @"type" = try Ranged(ast.Expr).wrap(self, readExprRaw);

    return .{ .decl = .{
        .name = name,
        .type = try self.box(ast.Expr, @"type"),
    } };
}

fn readExprRaw(self: *@This()) Err!ast.Expr {
    return switch (self.peek().value) {
        // Read the word type
        .word => {
            _ = try self.expect(.word);
            return .word_type;
        },

        // Read the type type
        .type => {
            _ = try self.expect(.type);
            return .type;
        },

        // Read a pointer type
        .asterisk => {
            _ = try self.expect(.asterisk);

            const child = try Ranged(ast.Expr).wrap(self, readExprRaw);

            return .{ .pointer_type = try self.box(ast.Expr, child) };
        },

        // Read a container
        .container => .{ .container = try self.readContainer() },

        // Read a word
        .number => .{ .word = try self.readWord() },

        // Read an identifier
        .ident => .{ .ident = try self.expect(.ident) },

        // Read an expression surrounded with parentheses
        .opening_parentheses => try self.readParentheses(),

        else => self.failExpected(&.{ .word, .type, .asterisk, .container, .number, .ident, .opening_parentheses }),
    };
}

/// Parses an expression from this parser.
pub fn readExpr(self: *@This()) Err!Ranged(ast.Expr) {
    return try self.composeExpr1();
}

fn readMemberAccess(self: *@This(), base: Ranged(ast.Expr)) Err!ast.Expr {
    _ = try self.expect(.period);

    const member = try self.expect(.ident);

    const ptr = try self.allocator.create(ast.Expr);
    ptr.* = base.value;

    return .{ .member_access = .{
        .container = base.swap(ptr),
        .member = member,
    } };
}

fn readDereference(self: *@This(), base: Ranged(ast.Expr)) Err!ast.Expr {
    _ = try self.expect(.period_asterisk);

    const ptr = try self.allocator.create(ast.Expr);
    ptr.* = base.value;

    return .{ .dereference = base.swap(ptr) };
}

/// Reads an expression surrounded in parentheses from this parser.
fn readParentheses(self: *@This()) Err!ast.Expr {
    _ = try self.expect(.opening_parentheses);

    const expr = try self.readExpr();

    _ = try self.expect(.closing_parentheses);

    return .{ .parentheses = try self.box(ast.Expr, expr) };
}

/// Parses a word from this parser.
pub fn readWord(self: *@This()) Err!u32 {
    const token = try self.expect(.number);

    return std.fmt.parseUnsigned(u32, token.range.substr(self.src), 10) catch self.fail(.{ .number_too_large = token.range });
}

/// Consumes the next token from this parser if it's the provided tag. Returns
/// `null` otherwise.
pub fn consume(self: *@This(), comptime token: Token) ?Ranged(Token) {
    return if (self.peek().value == token)
        self.next()
    else
        null;
}

/// Reads a token, returning an error if it's not equal to the given tag.
pub fn expect(self: *@This(), comptime token: Token) !Ranged(Token) {
    return self.expectMany(&.{token});
}

/// Reads a token, returning an error if it's not one of the given tags.
pub fn expectMany(self: *@This(), comptime tags: []const Token) !Ranged(Token) {
    const next_tag = self.peek().value;

    inline for (tags) |tag| {
        if (tag == next_tag) return self.next();
    } else return self.failExpected(tags);
}

/// Fails with an error message saying that the given tags were expected.
pub fn failExpected(self: *@This(), comptime tags: []const Token) error{CodeError} {
    return self.fail(.{ .expected_tag = .{
        .expected = tags,
        .found = self.peek(),
    } });
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": failure.Error) error{CodeError} {
    self.error_context = @"error";
    return error.CodeError;
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

/// Returns the location of the tokenizer.
pub fn location(self: *@This()) tokenizer.Location {
    return self.tokens.loc;
}

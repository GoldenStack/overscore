const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const failure = @import("failure.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;

/// The abstract syntax tree.
pub const ast = struct {
    /// A container, containing a list of declarations.
    pub const Container = struct {
        decls: std.ArrayList(Ranged(ContainerDecl)),
    };

    /// A declaration in a container. This is equivalent to a normal declaration
    /// except that it also has an access modifier.
    pub const ContainerDecl = struct {
        access: Access,
        decl: Decl,
    };

    /// A declaration, consisting of a mutability modifier, a name, a type
    /// (explicit for now), and a value;
    pub const Decl = struct {
        mutability: Mutability,
        name: Ranged(Token),

        type: Ranged(Expr),
        value: Ranged(Expr),
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

    /// Type expressions. These cannot be reduced any further, but their constituent
    /// parts (parameters, declarations, values, etc) may need to be. They must be
    /// fully evaluated during compile time.
    pub const Type = union(enum) {
        /// An integer type, the CPU "word". For now, integers are always unsigned
        /// 32-bit integers, so they are referred to as words to reflect the
        /// intention of this to be changed.
        word,

        /// A container. See `Container` documentation for more details.
        container: Container,

        /// A type.
        ///
        /// When the value of an expression is a type, the type of the expression is
        /// `type`. This does lead to unsoundness in the type system due to
        /// Russell's paradox, likely the only point of unsoundness, but I don't
        /// really care.
        type,
    };

    pub const Expr = union(enum) {
        /// The primitive value that the CPU can handle, an unsigned 32-bit integer.
        word: u32,

        /// A type - see `Type`.
        ///
        /// This is not wrapped in a range because the values in `Type` are
        /// conveniently stored separately, not because there's some special keyword
        /// indicating that a type expression will follow.
        type: Type,

        /// An identifier that contains a value.
        ident: Ranged(Token),

        /// An expression that has been wrapped with parentheses.
        parentheses: Ranged(*Expr),

        /// Represents accessing a member of a container.
        member_access: struct {
            container: Ranged(*Expr),
            member: Ranged(Token),
        }
    };

    pub fn printContainer(src: []const u8, container: Container, writer: anytype) anyerror!void {
        try writer.writeAll("{ ");

        for (container.decls.items) |decl| {
            try printContainerDecl(src, decl.value, writer);
            try writer.writeByte(' ');
        }

        try writer.writeByte('}');
    }

    pub fn printContainerDecl(src: []const u8, decl: ContainerDecl, writer: anytype) anyerror!void {
        if (decl.access == .public) try writer.writeAll("pub ");

        try printDecl(src, decl.decl, writer);
    }

    pub fn printDecl(src: []const u8, decl: Decl, writer: anytype) anyerror!void {
        try writer.writeAll(switch (decl.mutability) {
            .constant => "const",
            .variable => "var",
        });
        try writer.writeByte(' ');

        try printToken(src, decl.name, writer);
        try writer.writeAll(": ");

        try printExpr(src,decl.type.value, writer);
        try writer.writeAll(" = ");

        try printExpr(src, decl.value.value, writer);
        try writer.writeAll(";");
    }

    pub fn printExpr(src: []const u8, expr: Expr, writer: anytype) anyerror!void {
        switch (expr) {
            .word => |word| try writer.print("{}", .{word}),
            .type => |@"type"| try printType(src, @"type", writer),
            .ident => |ident| try printToken(src, ident, writer),
            .parentheses => |parens| {
                try writer.writeByte('(');
                try printExpr(src, parens.value.*, writer);
                try writer.writeByte(')');
            },
            .member_access => |member| {
                try printExpr(src, member.container.value.*, writer);
                try writer.writeByte('.');
                try printToken(src, member.member, writer);
            }
        }
    }

    pub fn printType(src: []const u8, @"type": Type, writer: anytype) anyerror!void {
        switch (@"type") {
            .word => try writer.writeAll("word"),
            .type => try writer.writeAll("type"),
            .container => |container| try printContainer(src, container, writer),
        }
    }

    pub fn printToken(src: []const u8, token: Ranged(Token), writer: anytype) anyerror!void {
        try writer.writeAll(token.range.substr(src));
    }

};

/// The error set of errors that can occur while parsing the AST.
/// `error.SyntaxError` represents an error in the syntax; all other errors are
/// abnormal behaviour.
pub const Error = error{
    /// Represents an error that occurred while parsing. This is "normal
    /// behaviour".
    SyntaxError,

    OutOfMemory,
};

src: [:0]const u8,
tokens: tokenizer.Tokenizer,
allocator: std.mem.Allocator,

next_token: ?Ranged(Token) = null,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.SyntaxErrpr, this value is significant. Otherwise, it may
/// contain anything.
error_context: ?failure.Error = null,

pub fn init(allocator: std.mem.Allocator, tokens: tokenizer.Tokenizer) @This() {
    return .{
        .src = tokens.src,
        .tokens = tokens,
        .allocator = allocator,
    };
}

/// Reads the root container from this parser. This will consume the entire
/// sorce file unless there is an error.
pub fn readRoot(self: *@This()) Error!ast.Container {
    var decls = std.ArrayList(Ranged(ast.ContainerDecl)).init(self.allocator);

    while (self.peek().value != .eof) {
        try decls.append(try Ranged(ast.ContainerDecl).wrap(self, readContainerDecl));
    }

    return .{
        .decls = decls,
    };
}

/// Reads a container from this parser.
pub fn readContainer(self: *@This()) Error!ast.Container {
    _ = try self.expect(.container);
    _ = try self.expect(.opening_curly_bracket);

    var decls = std.ArrayList(Ranged(ast.ContainerDecl)).init(self.allocator);

    while (self.peek().value != .closing_curly_bracket) {
        try decls.append(try Ranged(ast.ContainerDecl).wrap(self, readContainerDecl));
    }

    _ = try self.expect(.closing_curly_bracket);

    return .{
        .decls = decls,
    };
}

/// Parses a container declaration from this parser.
pub fn readContainerDecl(self: *@This()) Error!ast.ContainerDecl {
    const access: ast.Access = if (self.consume(.@"pub")) |_| .public else .private;

    return .{
        .access = access,
        .decl = try self.readDecl(),
    };
}

/// Parses a declaration from this parser.
pub fn readDecl(self: *@This()) Error!ast.Decl {
    const mutability: ast.Mutability = switch ((try self.expectMany(&.{ .@"const", .@"var" })).value) {
        .@"const" => .constant,
        .@"var" => .variable,
        else => unreachable,
    };

    const name = try self.expect(.ident);

    // Type specifier is mandatory for now.
    _ = try self.expect(.colon);

    const @"type" = try Ranged(ast.Expr).wrap(self, readExpr);

    _ = try self.expect(.equals);

    const value = try Ranged(ast.Expr).wrap(self, readExpr);

    _ = try self.expect(.semicolon);

    return .{
        .mutability = mutability,
        .name = name,
        .type = @"type",
        .value = value,
    };
}

// TODO: Either guarantee that it evaluates to a value, or make separate
// functions for guaranteed values vs non-guaranteed values.

pub fn readExprPtr(self: *@This()) Error!*ast.Expr {
    const ptr = try self.allocator.create(ast.Expr);
    ptr.* = try self.readExpr();
    return ptr;
}

/// Parses an expression from this parser.
pub fn readExpr(self: *@This()) Error!ast.Expr {
    var expr = try Ranged(ast.Expr).wrap(self, readExprRaw);

    while (true) {
        expr = switch (self.peek().value) {
            .period => try expr.mapExtend(self, readMemberAccess),
            else => break,
        };
    }

    return expr.value;
}

fn readExprRaw(self: *@This()) Error!ast.Expr {
    return switch (self.peek().value) {
        // Read a type
        .word, .type, .container => .{ .type = try self.readType() },

        // Read a word
        .number => .{ .word = try self.readWord() },

        // Read an identifier
        .ident => .{ .ident = try self.expect(.ident) },

        // Read an expression surrounded with parentheses
        .opening_parentheses => try self.readParentheses(),

        else => self.failExpected(&.{ .word, .type, .container, .number, .ident, .opening_parentheses }),
    };
}

fn readMemberAccess(self: *@This(), base: Ranged(ast.Expr)) Error!ast.Expr {
    _ = try self.expect(.period);

    const member = try self.expect(.ident);

    const ptr = try self.allocator.create(ast.Expr);
    ptr.* = base.value;

    return .{ .member_access = .{
        .container = base.swap(ptr),
        .member = member,
    } };
}

/// Parses a type from this parser.
pub fn readType(self: *@This()) Error!ast.Type {
    return switch (self.peek().value) {
        .word => {
            _ = try self.expect(.word);
            return .word;
        },
        .type => {
            _ = try self.expect(.type);
            return .type;
        },
        .container => .{ .container = try self.readContainer() },
        else => self.failExpected(&.{ .word, .type, .container }),
    };
}

/// Reads an expression surrounded in parentheses from this parser.
pub fn readParentheses(self: *@This()) Error!ast.Expr {
    _ = try self.expect(.opening_parentheses);

    const expr = try Ranged(*ast.Expr).wrap(self, readExprPtr);

    _ = try self.expect(.closing_parentheses);

    return .{ .parentheses = expr };
}

/// Parses a word from this parser.
pub fn readWord(self: *@This()) Error!u32 {
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
pub fn failExpected(self: *@This(), comptime tags: []const Token) error{SyntaxError} {
    return self.fail(.{ .expected_tag = .{
        .expected = tags,
        .found = self.peek(),
    } });
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": failure.Error) error{SyntaxError} {
    self.error_context = @"error";
    return error.SyntaxError;
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


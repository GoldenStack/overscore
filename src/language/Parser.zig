const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const failure = @import("failure.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;

/// The abstract syntax tree.
pub const ast = struct {

    /// A container, containing a list of declarations.
    pub const Container = struct {
        decls: std.ArrayList(Ranged(Decl)),
    };

    /// A declaration, consisting of an access modifier, a mutability modifier, a
    /// name, a type (explicit for now), and a value;
    pub const Decl = struct {
        access: Access,
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
    };

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
pub fn read_root(self: *@This()) Error!ast.Container {
    var decls = std.ArrayList(Ranged(ast.Decl)).init(self.allocator);

    while (self.peek().value != .eof) {
        try decls.append(try Ranged(ast.Decl).wrap(self, read_decl));
    }

    return .{
        .decls = decls,
    };
}

/// Parses a declaration from this parser.
pub fn read_decl(self: *@This()) Error!ast.Decl {
    const access: ast.Access = if (self.consume(.@"pub")) |_| .public else .private;

    const mutability: ast.Mutability = switch ((try self.expect_many(&.{ .@"const", .@"var" })).value) {
        .@"const" => .constant,
        .@"var" => .variable,
        else => unreachable,
    };

    const name = try self.expect(.ident);

    // Type specifier is mandatory for now.
    _ = try self.expect(.colon);

    const @"type" = try Ranged(ast.Expr).wrap(self, read_expr);

    _ = try self.expect(.equals);

    const value = try Ranged(ast.Expr).wrap(self, read_expr);

    _ = try self.expect(.semicolon);

    return .{
        .access = access,
        .mutability = mutability,
        .name = name,
        .@"type" = @"type",
        .value = value,
    };
}

// TODO: Either guarantee that it evaluates to a value, or make separate
// functions for guaranteed values vs non-guaranteed values.

/// Parses an expression from this parser.
pub fn read_expr(self: *@This()) Error!ast.Expr {
    return switch (self.peek().value) {
        // Read a type
        .word, .type => .{ .type = try self.read_type() },

        // Read a word
        .number => .{ .word = try self.read_word() },

        // Read an identifier
        .ident => .{ .ident = try self.expect(.ident) },

        else => self.fail_expected(&.{ .word, .type, .number }),
    };
}

/// Parses a type from this parser.
pub fn read_type(self: *@This()) Error!ast.Type {
    return switch (self.peek().value) {
        .word => {
            _ = self.consume(.word); // TODO: Add and switch to .skip() ?
            return .word;
        },
        .type => {
            _ = self.consume(.type); // TODO: Add and swithc to .skip() ?
            return .type;
        },
        else => self.fail_expected(&.{.type}),
    };
}

/// Parses a word from this parser.
pub fn read_word(self: *@This()) Error!u32 {
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
    return self.expect_many(&.{token});
}

/// Reads a token, returning an error if it's not one of the given tags.
pub fn expect_many(self: *@This(), comptime tags: []const Token) !Ranged(Token) {
    const next_tag = self.peek().value;

    inline for (tags) |tag| {
        if (tag == next_tag) return self.next();
    } else return self.fail_expected(tags);
}

/// Fails with an error message saying that the given tags were expected.
pub fn fail_expected(self: *@This(), comptime tags: []const Token) error{SyntaxError} {
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

// Functions for printing containers
// TODO: Separate this into the `Print` namespace.

pub fn print_container(self: *const @This(), container: ast.Container, writer: anytype) anyerror!void {
    try writer.writeAll("{ ");

    for (container.decls.items) |decl| {
        try self.print_decl(decl.value, writer);
        try writer.writeByte(' ');
    }

    try writer.writeByte('}');
}

pub fn print_decl(self: *const @This(), decl: ast.Decl, writer: anytype) anyerror!void {
    if (decl.access == .public) try writer.writeAll("pub ");

    try writer.writeAll(switch (decl.mutability) {
        .constant => "const",
        .variable => "var",
    });
    try writer.writeByte(' ');

    try self.print_token(decl.name, writer);
    try writer.writeAll(": ");

    try self.print_expr(decl.type.value, writer);
    try writer.writeAll(" = ");

    try self.print_expr(decl.value.value, writer);
    try writer.writeAll(";");
}

pub fn print_expr(self: *const @This(), expr: ast.Expr, writer: anytype) anyerror!void {
    switch (expr) {
        .word => |word| try writer.print("{}", .{word}),
        .type => |@"type"| try self.print_type(@"type", writer),
        .ident => |ident| try self.print_token(ident, writer),
    }
}

pub fn print_type(self: *const @This(), @"type": ast.Type, writer: anytype) anyerror!void {
    _ = self;
    switch (@"type") {
        .word => try writer.writeAll("word"),
        .type => try writer.writeAll("type"),
    }
}

pub fn print_token(self: *const @This(), token: Ranged(Token), writer: anytype) anyerror!void {
    try writer.writeAll(token.range.substr(self.src));
}

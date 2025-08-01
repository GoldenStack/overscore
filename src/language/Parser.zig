const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;

/// The abstract syntax tree.
pub const ast = struct {
    /// A definition of a name, consisting of an access modifier, a mutability
    /// modifier, a name, an optional type, and value.
    pub const Def = struct {
        access: Access,
        mutability: Mutability,
        name: Ranged(Token),

        type: ?Ranged(*Expr),
        value: Ranged(*Expr),
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

    /// The level of indirection of some data.
    pub const Indirection = enum {
        direct,
        indirect,
    };

    pub const Expr = union(enum) {
        /// The primitive value that the CPU can handle, an unsigned 32-bit integer.
        word: u32,

        /// An integer type, the CPU "word". For now, integers are always unsigned
        /// 32-bit integers, so they are referred to as words to reflect the
        /// intention of this to be changed.
        word_type,

        decl: Decl,

        def: Def,

        product: std.ArrayList(Ranged(Expr)),

        sum: std.ArrayList(Ranged(Expr)),

        container: std.ArrayList(Ranged(Expr)),

        /// A pointer type.
        pointer_type: Ranged(*Expr),

        /// A type.
        ///
        /// When the value of an expression is a type, the type of the expression is
        /// `type`. This does lead to unsoundness in the type system due to
        /// Russell's paradox, likely the only point of unsoundness, but I don't
        /// really care.
        type,

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
            indirection: Indirection,
        },
    };

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
            try printExpr(src, @"type".value.*, writer);
        }

        try writer.writeAll(" = ");
        try printExpr(src, def.value.value.*, writer);
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
            .def => |def| try printDef(src, def, writer),
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
            .container => |defs| {
                try writer.writeAll("{ ");

                for (defs.items) |def| {
                    try printExpr(src, def.value, writer);
                    try writer.writeByte(' ');
                }

                try writer.writeByte('}');
            },
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
                try writer.writeAll(switch (member.indirection) {
                    .direct => ".",
                    .indirect => "->",
                });
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
/// source file unless there is an error.
pub fn readRoot(self: *@This()) Err!Ranged(ast.Expr) {
    const expr = try self.readExpr();

    _ = try self.expect(.eof);

    return expr;
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

        break :type_specifier try self.box(ast.Expr, try self.composeLowPriorityInfixExpressions());
    } else null;

    _ = try self.expect(.equals);

    const value = try self.box(ast.Expr, try self.composeLowPriorityInfixExpressions());

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

fn composeMediumPriorityInfixExpressions(self: *@This()) Err!Ranged(ast.Expr) {
    var expr = try self.composeLowPriorityInfixExpressions();

    // Must have the same names as the Expr tags.
    const Infix = enum { sum, product, container };

    loop: while (true) {
        // Figure out which operation we are, or exit.
        const op: Infix = switch (self.peek().value) {
            .plus_plus => .sum,
            .asterisk => .product,
            .semicolon => .container,
            else => break,
        };

        // Parse the operation and find its range.
        const op_range: tokenizer.Range = switch (op) {
            .sum => (try self.expect(.plus_plus)).range,
            .product => weird_product: {
                const first = try self.expect(.asterisk);
                const second = try self.expect(.asterisk);

                break :weird_product .{ .start = first.range.start, .end = second.range.end };
            },
            .container => (try self.expect(.semicolon)).range,
        };

        // Iterate through fields so we can find the right name.
        inline for (std.meta.fields(Infix)) |field| @"continue": {
            // Make sure we've found the right operator type of the existing expression.
            if (@field(ast.Expr, field.name) != expr.value) break :@"continue";

            // Allow trailing operators as syntax sugar, but make sure not to allow
            // single-element lists on accident.
            if (@field(expr.value, field.name).items.len > 1) switch (self.peek().value) {
                .eof, .closing_parentheses, .closing_curly_bracket => break,
                else => {},
            };

            // If the previous operation has the same type as the new one, add it
            if (std.mem.eql(u8, field.name, @tagName(expr.value))) {
                try @field(expr.value, field.name).append(try self.composeLowPriorityInfixExpressions());
                continue :loop;
            } else return self.fail(.{
                .mixed_precedence = .{ // If not, mixed precendece error!
                    .expr = expr.range,
                    .operator = op_range,
                },
            });
        } else {
            // If there are no infix expressions on the existing expression,
            // create a new list.
            var exprs = std.ArrayList(Ranged(ast.Expr)).init(self.allocator);
            try exprs.append(expr);
            try exprs.append(try self.composeLowPriorityInfixExpressions());

            const newExpr = switch (op) {
                inline else => |tag| @unionInit(ast.Expr, @tagName(tag), exprs),
            };

            // TODO: Incorrect range
            expr = expr.swap(newExpr);
        }
    }

    return expr;
}

fn composeLowPriorityInfixExpressions(self: *@This()) Err!Ranged(ast.Expr) {
    var expr = try Ranged(ast.Expr).wrap(self, readExprRaw);

    while (true) {
        expr = sw: switch (self.peek().value) {
            .period, .arrow => try expr.mapExtend(self, readMemberAccess),
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

        .@"pub", .@"const", .@"var" => .{ .def = try self.readDef() },

        // Read a word
        .number => .{ .word = try self.readWord() },

        // Read an identifier
        .ident => .{ .ident = try self.expect(.ident) },

        // Read an expression surrounded with parentheses
        .opening_parentheses => try self.readParentheses(),

        else => self.failExpected(&.{ .word, .type, .asterisk, .@"pub", .@"const", .@"var", .number, .ident, .opening_parentheses }),
    };
}

/// Parses an expression from this parser.
pub fn readExpr(self: *@This()) Err!Ranged(ast.Expr) {
    return try self.composeMediumPriorityInfixExpressions();
}

fn readMemberAccess(self: *@This(), base: Ranged(ast.Expr)) Err!ast.Expr {
    const token = try self.expectMany(&.{ .period, .arrow });

    const member = try self.expect(.ident);

    const ptr = try self.allocator.create(ast.Expr);
    ptr.* = base.value;

    return .{ .member_access = .{
        .container = base.swap(ptr),
        .member = member,
        .indirection = switch (token.value) {
            .period => .direct,
            .arrow => .indirect,
            else => unreachable,
        },
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

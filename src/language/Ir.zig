const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;
const Range = tokenizer.Range;

const ast = @import("Parser.zig").ast;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;

/// Represents an index into the local expr array.
pub const Index = struct {
    index: usize,
};

/// Represents an index to a specific type in the local expr array.
pub fn IndexOf(variant: ir.Expr.Tag) type {
    return struct {
        comptime variant: ir.Expr.Tag = variant,
        index: Index,
    };
}

/// The intermediate representation (IR).
///
/// Documentation is omitted as it's meant to be essentially a clone of `ast`.
/// However, changes relative to the AST are still documented.
pub const ir = struct {
    pub const Container = struct {
        // Stores the parent scope of this container. This allows namespace
        // resolution.
        parent: ?IndexOf(.container),

        defs: std.StringArrayHashMap(IndexOf(.def)),
    };

    pub const Def = struct {
        access: ast.Access,
        mutability: ast.Mutability,
        name: Range,
        type: ?Index,
        value: Index,

        /// Whether or not this definition is in the process of being evaluated.
        /// When a definition needs to be evaluated while it's already being
        /// evaluated, this means its value depends on itself, and thus a
        /// dependency loop exists.
        evaluating: bool = false,

        /// Whether or not type checking has started for this definition.
        /// Generally it's not a good idea to do the same thing for evaluation
        /// (trusting that something has been evaluated is error-prone) but this
        /// works for type checking.
        type_checked: bool = false,
    };

    pub const Decl = struct {
        name: Range,
        type: Index,

        /// Whether or not this definition is in the process of being evaluated.
        /// When a definition needs to be evaluated while it's already being
        /// evaluated, this means its value depends on itself, and thus a
        /// dependency loop exists.
        evaluating: bool = false,
    };

    pub const Expr = union(Tag) {
        pub const Tag = enum { word, word_type, def, decl, product, sum, pointer_type, type, container, pointer, dereference, parentheses, member_access };
        word: u32,
        word_type,
        def: Def,
        decl: Decl,
        product: std.ArrayList(Index),
        sum: std.ArrayList(Index),
        pointer_type: Index,
        type,
        container: Container,
        pointer: IndexOf(.def),
        dereference: Index,
        parentheses: Index,
        member_access: struct {
            container: Index,
            member: Range,
        },
    };
};

pub const Value = struct {
    expr_range: Range,
    expr: ir.Expr,

    /// The type of the expression. This must always align with the expression
    /// value and thus by typed manually.
    ///
    /// Must be an evaluated type expression (word_type, decl, product, sum, pointer_type, type, container).
    type: ?Index,
};

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

values: std.ArrayList(Value),

count: u64 = 0,

// TODO: Will need to refactor when functions are added
current: ?IndexOf(.container) = null,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .values = std.ArrayList(Value).init(allocator),
    };
}

pub fn indexPush(self: *@This(), expr: ir.Expr, expr_range: Range) error{OutOfMemory}!Index {
    const index = self.values.items.len;
    try self.values.append(.{
        .expr = expr,
        .expr_range = expr_range,
        .type = null,
    });
    return .{ .index = index };
}

pub fn convertExpr(self: *@This(), expr: Ranged(ast.Expr)) Err!Index {
    const index = try self.indexPush(undefined, expr.range);

    const value: ir.Expr = value: switch (expr.value) {
        .word_type => .word_type,
        .type => .type,

        .word => |word| .{ .word = word },

        .decl => |decl| .{ .decl = try self.convertDecl(decl) },
        .product => |product| .{ .product = try self.convertExprList(product) },
        .sum => |sum| .{ .sum = try self.convertExprList(sum) },

        .pointer_type => |ptr| .{ .pointer_type = try self.convertExprPtr(ptr) },
        .ident => |ident| .{ .pointer = try self.lookupNameExpected(ident) },
        .dereference => |deref| .{ .dereference = try self.convertExprPtr(deref) },
        .parentheses => |parens| continue :value parens.value.*,

        .member_access => |access| .{ .member_access = .{
            .container = try self.convertExprPtr(access.container),
            .member = access.member.range,
        } },

        .container => |container| {
            try self.convertContainer(container, index);
            return index; // Prevents duplicate writes
        },
    };

    self.indexGet(index).expr = value;
    return index;
}

fn convertExprPtr(self: *@This(), expr: Ranged(*ast.Expr)) Err!Index {
    return self.convertExpr(expr.swap(expr.value.*));
}

fn convertExprList(self: *@This(), exprs: std.ArrayList(Ranged(ast.Expr))) Err!std.ArrayList(Index) {
    var out_exprs = std.ArrayList(Index).init(self.allocator);

    for (exprs.items) |item| {
        try out_exprs.append(try self.convertExpr(item));
    }

    return out_exprs;
}

pub fn convertContainer(self: *@This(), container: ast.Container, index: Index) Err!void {
    var defs = std.StringArrayHashMap(IndexOf(.def)).init(self.allocator);

    // Add references with no values
    for (container.defs.items) |def| {
        const name = def.value.name;
        const key = name.range.substr(self.src);

        const raw_index = try self.indexPush(.{ .def = undefined }, def.range);

        const def_index: IndexOf(.def) = .{ .index = raw_index };

        try self.lookupNameCurrentContainerForDefine(name, &defs);
        try defs.putNoClobber(key, def_index);
    }

    self.indexGet(index).expr = .{ .container = .{
        .defs = defs,
        .parent = self.current,
    } };

    self.current = .{ .index = index };

    // Add the values of the references
    for (container.defs.items) |def| {
        const key = def.value.name.range.substr(self.src);
        const stored_def = self.indexOfGet(.container, self.current.?).defs.get(key) orelse unreachable; // We just added it, so it must exist

        const converted = try self.convertDef(def.value);
        self.indexOfGet(.def, stored_def).* = converted;
    }

    // We set current above, so it must be valid
    self.current = self.indexOfGet(.container, self.current.?).parent;
}

pub fn convertDef(self: *@This(), def: ast.Def) Err!ir.Def {
    return .{
        .access = def.access,
        .mutability = def.mutability,
        .name = def.name.range,
        .type = if (def.type) |@"type"| try self.convertExpr(@"type") else null,
        .value = try self.convertExpr(def.value),
    };
}

pub fn convertDecl(self: *@This(), decl: ast.Decl) Err!ir.Decl {
    return .{
        .name = decl.name.range,
        .type = try self.convertExprPtr(decl.type),
    };
}

pub fn lookupName(self: *@This(), name: Ranged(Token)) ?IndexOf(.def) {
    var current = self.current;

    while (current) |scope| {
        const container = self.indexOfGet(.container, scope);

        if (container.defs.get(name.range.substr(self.src))) |def| {
            return def;
        }

        current = container.parent;
    } else return null;
}

pub fn lookupNameExpected(self: *@This(), name: Ranged(Token)) Err!IndexOf(.def) {
    const index = self.lookupName(name);

    return index orelse self.fail(.{ .unknown_identifier = name.range });
}

pub fn lookupNameForDefine(self: *@This(), name: Ranged(Token)) Err!void {
    const maybe_index = self.lookupName(name);

    if (maybe_index) |index| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = self.indexOfGet(.def, index).name,
            .redeclared = name.range,
        } });
    }
}

pub fn lookupNameCurrentContainerForDefine(self: *@This(), name: Ranged(Token), defs: *std.StringArrayHashMap(IndexOf(.def))) Err!void {
    const value = name.range.substr(self.src);

    // This used to be merged into one with a `defs.getOrPut`, which has better
    // performance, but unfortunately has different semantics (would require
    // removing on fail, etc.).
    if (defs.get(value)) |def| {
        return self.fail(.{ .duplicate_member_name = .{
            .declared = self.indexOfGet(.def, def).name,
            .redeclared = name.range,
        } });
    }

    try self.lookupNameForDefine(name);
}

pub fn indexGet(self: *@This(), index: Index) *Value {
    return &self.values.items[index.index];
}

pub fn indexOfGet(self: *@This(), comptime variant: std.meta.Tag(ir.Expr), index: IndexOf(variant)) *std.meta.TagPayload(ir.Expr, index.variant) {
    return &@field(self.indexGet(index.index).expr, @tagName(index.variant));
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{CodeError} {
    self.error_context = @"error";
    return error.CodeError;
}

pub fn printContainer(self: *const @This(), container: ir.Container, writer: anytype) anyerror!void {
    try writer.writeAll("container { ");

    var iter = container.defs.iterator();
    while (iter.next()) |def| {
        try self.printExpr(def.value_ptr.index, writer);
        try writer.writeByte(' ');
    }

    try writer.writeByte('}');
}

pub fn printInterface(self: *const @This(), interface: ir.Interface, writer: anytype) anyerror!void {
    try writer.writeAll(switch (interface.variant) {
        .product => "product { ",
        .sum => "sum { ",
    });

    var iter = interface.decls.iterator();
    while (iter.next()) |decl| {
        try self.printDecl(decl.value_ptr.value, writer);
        try writer.writeByte(' ');
    }

    try writer.writeByte('}');
}

pub fn printDef(self: *const @This(), def: ir.Def, writer: anytype) anyerror!void {
    if (def.access == .public) try writer.writeAll("pub ");

    try writer.writeAll(switch (def.mutability) {
        .constant => "const",
        .variable => "var",
    });
    try writer.writeByte(' ');

    try self.printRange(def.name, writer);

    if (def.type) |type_specifier| {
        try writer.writeAll(": ");
        try self.printExpr(type_specifier, writer);
    }

    try writer.writeAll(" = ");
    try self.printExpr(def.value, writer);
    try writer.writeAll(";");
}

pub fn printDecl(self: *const @This(), decl: ir.Decl, writer: anytype) anyerror!void {
    try self.printRange(decl.name, writer);
    try writer.writeAll(": ");
    try self.printExpr(decl.type, writer);
}

pub fn printExpr(self: *const @This(), index: Index, writer: anytype) anyerror!void {
    if (self.count > 100) return error.abc;
    @constCast(self).count += 1;

    const expr = self.values.items[index.index];

    switch (expr.expr) {
        .word => |word| try writer.print("{}", .{word}),
        .word_type => try writer.writeAll("word"),
        .type => try writer.writeAll("type"),
        .product => |decls| {
            try writer.writeAll("(");
            for (0.., decls.items) |i, decl| {
                if (i != 0) try writer.writeAll(" ** ");
                try self.printExpr(decl, writer);
            }
            try writer.writeAll(")");
        },
        .sum => |decls| {
            try writer.writeAll("(");
            for (0.., decls.items) |i, decl| {
                if (i != 0) try writer.writeAll(" ++ ");
                try self.printExpr(decl, writer);
            }
            try writer.writeAll(")");
        },
        .def => |def| try self.printDef(def, writer),
        .decl => |decl| try self.printDecl(decl, writer),
        .pointer_type => |ptr| {
            try writer.writeByte('*');
            try self.printExpr(ptr, writer);
        },
        .container => |container| try self.printContainer(container, writer),
        .pointer => |ptr| {
            try self.printRange(expr.expr_range, writer);
            try writer.print("<{}>", .{ptr.index.index});
        },
        .dereference => |deref| {
            try self.printExpr(deref, writer);
            try writer.writeAll(".*");
        },
        .parentheses => |parens| {
            try writer.writeByte('(');
            try self.printExpr(parens, writer);
            try writer.writeByte(')');
        },
        .member_access => |member| {
            try self.printExpr(member.container, writer);
            try writer.writeByte('.');
            try self.printRange(member.member, writer);
        },
    }
}

pub fn printRange(self: *const @This(), range: tokenizer.Range, writer: anytype) anyerror!void {
    try writer.writeAll(range.substr(self.src));
}

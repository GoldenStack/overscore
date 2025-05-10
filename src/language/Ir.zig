const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

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

        defs: std.StringArrayHashMap(Ranged(IndexOf(.def))),
    };

    pub const Def = struct {
        access: ast.Access,
        mutability: ast.Mutability,
        name: Ranged(Token),
        type: ?Ranged(Index),
        value: Ranged(Index),

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
        name: Ranged(Token),
        type: Ranged(Index),

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
        product: std.ArrayList(Ranged(Index)),
        sum: std.ArrayList(Ranged(Index)),
        pointer_type: Ranged(Index),
        type,
        container: Container,
        pointer: Ranged(IndexOf(.def)),
        dereference: Ranged(Index),
        parentheses: Ranged(Index),
        member_access: struct {
            container: Ranged(Index),
            member: Ranged(Token),
        },
    };
};

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

exprs: std.ArrayList(ir.Expr),

// TODO: Will need to refactor when functions are added
current: ?IndexOf(.container) = null,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .exprs = std.ArrayList(ir.Expr).init(allocator),
    };
}

pub fn convertContainer(self: *@This(), container: ast.Container, index: IndexOf(.container)) Err!void {
    var defs = std.StringArrayHashMap(Ranged(IndexOf(.def))).init(self.allocator);

    // Add references with no values
    for (container.defs.items) |def| {
        const name = def.value.name;
        const key = name.range.substr(self.src);

        const def_index = try self.indexOfPush(.def, undefined);

        try self.lookupNameCurrentContainerForDefine(name, &defs);
        try defs.putNoClobber(key, def.swap(def_index));
    }

    self.indexOfGet(.container, index).* = .{
        .defs = defs,
        .parent = self.current,
    };

    self.current = index;

    // Add the values of the references
    for (container.defs.items) |def| {
        const key = def.value.name.range.substr(self.src);
        const stored_def = self.indexOfGet(.container, index).defs.get(key) orelse unreachable; // We just added it, so it must exist

        const converted = try self.convertDef(def.value);
        self.indexOfGet(.def, stored_def.value).* = converted;
    }

    // We set current above, so it must be valid
    self.current = self.indexOfGet(.container, self.current.?).parent;
}

pub fn convertDef(self: *@This(), def: ast.Def) Err!ir.Def {
    return .{
        .access = def.access,
        .mutability = def.mutability,
        .name = def.name,
        .type = if (def.type) |@"type"| try @"type".map(self, convertExpr) else null,
        .value = try def.value.map(self, convertExpr),
    };
}

pub fn convertDecl(self: *@This(), decl: ast.Decl) Err!ir.Decl {
    return .{
        .name = decl.name,
        .type = try decl.type.map(self, convertExprPtr),
    };
}

fn convertExprList(self: *@This(), exprs: std.ArrayList(Ranged(ast.Expr))) Err!std.ArrayList(Ranged(Index)) {
    var out_exprs = std.ArrayList(Ranged(Index)).init(self.allocator);

    for (exprs.items) |item| {
        try out_exprs.append(try item.map(self, convertExpr));
    }

    return out_exprs;
}

fn convertExprPtr(self: *@This(), expr: *ast.Expr) Err!Index {
    return self.convertExpr(expr.*);
}

fn convertExpr(self: *@This(), expr: ast.Expr) Err!Index {
    if (expr == .container) { // Edge case for when the index is needed
        const index = try self.indexOfPush(.container, undefined);
        try self.convertContainer(expr.container, index);
        return index.index;
    }

    const index = try self.indexPush(undefined);

    const value: ir.Expr = value: switch (expr) {
        .word_type => .word_type,
        .product => |product| .{ .product = try self.convertExprList(product) },
        .sum => |sum| .{ .sum = try self.convertExprList(sum) },
        .decl => |decl| .{ .decl = try self.convertDecl(decl) },
        .container => unreachable,
        .pointer_type => |ptr| .{ .pointer_type = try ptr.map(self, convertExprPtr) },
        .word => |word| .{ .word = word },
        .type => .type,
        .ident => |ident| .{ .pointer = ident.swap(try self.lookupNameExpected(ident)) },
        .dereference => |deref| .{ .dereference = try deref.map(self, convertExprPtr) },
        .parentheses => |parens| continue :value parens.value.*,
        .member_access => |access| .{ .member_access = .{
            .container = try access.container.map(self, convertExprPtr),
            .member = access.member,
        } },
    };

    self.indexGet(index).* = value;
    return index;
}

pub fn lookupName(self: *@This(), name: Ranged(Token)) ?IndexOf(.def) {
    var current = self.current;

    while (current) |scope| {
        const container = self.indexOfGet(.container, scope);

        if (container.defs.getPtr(name.range.substr(self.src))) |def| {
            return def.value;
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
            .declared = self.indexOfGet(.def, index).name.range,
            .redeclared = name.range,
        } });
    }
}

pub fn lookupNameCurrentContainerForDefine(self: *@This(), name: Ranged(Token), defs: *std.StringArrayHashMap(Ranged(IndexOf(.def)))) Err!void {
    const value = name.range.substr(self.src);

    // This used to be merged into one with a `defs.getOrPut`, which has better
    // performance, but unfortunately has different semantics (would require
    // removing on fail, etc.).
    if (defs.get(value)) |def| {
        return self.fail(.{ .duplicate_member_name = .{
            .declared = self.indexOfGet(.def, def.value).name.range,
            .redeclared = name.range,
        } });
    }

    try self.lookupNameForDefine(name);
}

pub fn indexGet(self: *@This(), index: Index) *ir.Expr {
    return &self.exprs.items[index.index];
}

pub fn indexOfGet(self: *@This(), comptime variant: std.meta.Tag(ir.Expr), index: IndexOf(variant)) *std.meta.TagPayload(ir.Expr, index.variant) {
    return &@field(self.indexGet(index.index), @tagName(index.variant));
}

pub fn indexPush(self: *@This(), payload: ir.Expr) error{OutOfMemory}!Index {
    const index = self.exprs.items.len;
    try self.exprs.append(payload);
    return .{ .index = index };
}

pub fn indexOfPush(self: *@This(), comptime variant: std.meta.Tag(ir.Expr), payload: std.meta.TagPayload(ir.Expr, variant)) error{OutOfMemory}!IndexOf(variant) {
    const index = try self.indexPush(@unionInit(ir.Expr, @tagName(variant), payload));

    return .{ .index = index };
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
        try self.printExpr(def.value_ptr.value.index, writer);
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

    try self.printRange(def.name.range, writer);

    if (def.type) |type_specifier| {
        try writer.writeAll(": ");
        try self.printExpr(type_specifier.value, writer);
    }

    try writer.writeAll(" = ");
    try self.printExpr(def.value.value, writer);
    try writer.writeAll(";");
}

pub fn printDecl(self: *const @This(), decl: ir.Decl, writer: anytype) anyerror!void {
    try self.printRange(decl.name.range, writer);
    try writer.writeAll(": ");
    try self.printExpr(decl.type.value, writer);
}

pub fn printExpr(self: *const @This(), index: Index, writer: anytype) anyerror!void {
    const expr = self.exprs.items[index.index];

    switch (expr) {
        .word => |word| try writer.print("{}", .{word}),
        .word_type => try writer.writeAll("word"),
        .type => try writer.writeAll("type"),
        .product => |decls| {
            try writer.writeAll("(");
            for (0.., decls.items) |i, decl| {
                if (i != 0) try writer.writeAll(" ** ");
                try self.printExpr(decl.value, writer);
            }
            try writer.writeAll(")");
        },
        .sum => |decls| {
            try writer.writeAll("(");
            for (0.., decls.items) |i, decl| {
                if (i != 0) try writer.writeAll(" ++ ");
                try self.printExpr(decl.value, writer);
            }
            try writer.writeAll(")");
        },
        .def => |def| try self.printDef(def, writer),
        .decl => |decl| try self.printDecl(decl, writer),
        .pointer_type => |ptr| {
            try writer.writeByte('*');
            try self.printExpr(ptr.value, writer);
        },
        .container => |container| try self.printContainer(container, writer),
        .pointer => |ptr| {
            try self.printRange(ptr.range, writer);
            try writer.print("<{}>", .{ptr.value.index.index});
        },
        .dereference => |deref| {
            try self.printExpr(deref.value, writer);
            try writer.writeAll(".*");
        },
        .parentheses => |parens| {
            try writer.writeByte('(');
            try self.printExpr(parens.value, writer);
            try writer.writeByte(')');
        },
        .member_access => |member| {
            try self.printExpr(member.container.value, writer);
            try writer.writeByte('.');
            try self.printRange(member.member.range, writer);
        },
    }
}

pub fn printRange(self: *const @This(), range: tokenizer.Range, writer: anytype) anyerror!void {
    try writer.writeAll(range.substr(self.src));
}

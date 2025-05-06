const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const ast = @import("Parser.zig").ast;
const failure = @import("failure.zig");
const Err = failure.ErrorSet;

/// Represents an index of a type. The type annotation currently doesn't do
/// anything; it just makes code more legible.
pub fn Index(T: type) type {
    return struct {
        index: usize,
        comptime {
            _ = T;
        }
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
        parent: ?Index(Container),

        defs: std.StringArrayHashMap(Ranged(ContainerDef)),
    };

    pub const ContainerDef = struct {
        access: ast.Access,
        def: Index(Def),
    };

    pub const Def = struct {
        mutability: ast.Mutability,
        name: Ranged(Token),
        type: ?Ranged(Index(Expr)),
        value: Ranged(Index(Expr)),

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
        type: Ranged(Index(Expr)),

        /// Whether or not this definition is in the process of being evaluated.
        /// When a definition needs to be evaluated while it's already being
        /// evaluated, this means its value depends on itself, and thus a
        /// dependency loop exists.
        evaluating: bool = false,
    };

    pub const Expr = union(enum) {
        word: u32,
        word_type,
        decl: Decl,
        product: std.ArrayList(Ranged(Index(Expr))),
        sum: std.ArrayList(Ranged(Index(Expr))),
        pointer_type: Ranged(Index(Expr)),
        type,
        container: Index(Container),
        pointer: Ranged(Index(ir.Def)),
        dereference: Ranged(Index(Expr)),
        parentheses: Ranged(Index(Expr)),
        member_access: struct {
            container: Ranged(Index(Expr)),
            member: Ranged(Token),
        },
    };
};

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

containers: std.ArrayList(ir.Container),
defs: std.ArrayList(ir.Def),
exprs: std.ArrayList(ir.Expr),

// TODO: Will need to refactor when functions are added
current: ?Index(ir.Container) = null,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .containers = std.ArrayList(ir.Container).init(allocator),
        .defs = std.ArrayList(ir.Def).init(allocator),
        .exprs = std.ArrayList(ir.Expr).init(allocator),
    };
}

pub fn convertContainer(self: *@This(), container: ast.Container) Err!Index(ir.Container) {
    var defs = std.StringArrayHashMap(Ranged(ir.ContainerDef)).init(self.allocator);

    // Add references with no values
    for (container.defs.items) |def| {
        const name = def.value.def.name;
        const key = name.range.substr(self.src);

        const def_index = try self.pushDef(undefined);

        try self.lookupNameCurrentContainerForDefine(name, &defs);
        try defs.putNoClobber(key, def.swap(ir.ContainerDef{
            .access = def.value.access,
            .def = def_index,
        }));
    }

    const index = try self.pushContainer(.{
        .defs = defs,
        .parent = self.current,
    });

    self.current = index;

    // Add the values of the references
    for (container.defs.items) |def| {
        const key = def.value.def.name.range.substr(self.src);
        const stored_def = self.indexContainer(index).defs.get(key) orelse unreachable; // We just added it, so it must exist

        const converted = try self.convertDef(def.value.def);
        self.indexDef(stored_def.value.def).* = converted;
    }

    // We set current above, so it must be valid
    self.current = self.indexContainer(self.current.?).parent;

    return index;
}

pub fn convertContainerDef(self: *@This(), def: ast.ContainerDef) Err!ir.ContainerDef {
    return .{
        .access = def.access,
        .def = try self.convertDef(def.def),
    };
}

pub fn convertDef(self: *@This(), def: ast.Def) Err!ir.Def {
    return .{
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

fn convertExprList(self: *@This(), exprs: std.ArrayList(Ranged(ast.Expr))) Err!std.ArrayList(Ranged(Index(ir.Expr))) {
    var out_exprs = std.ArrayList(Ranged(Index(ir.Expr))).init(self.allocator);

    for (exprs.items) |item| {
        try out_exprs.append(try item.map(self, convertExpr));
    }

    return out_exprs;
}

fn convertExprPtr(self: *@This(), expr: *ast.Expr) Err!Index(ir.Expr) {
    return self.convertExpr(expr.*);
}

pub fn convertExpr(self: *@This(), expr: ast.Expr) Err!Index(ir.Expr) {
    return self.pushExpr(try self.convertExprRaw(expr));
}

fn convertExprRaw(self: *@This(), expr: ast.Expr) Err!ir.Expr {
    return switch (expr) {
        .word_type => .word_type,
        .product => |product| .{ .product = try self.convertExprList(product) },
        .sum => |sum| .{ .sum = try self.convertExprList(sum) },
        .decl => |decl| .{ .decl = try self.convertDecl(decl) },
        .pointer_type => |ptr| .{ .pointer_type = try ptr.map(self, convertExprPtr) },
        .word => |word| .{ .word = word },
        .type => .type,
        .container => |container| .{ .container = try self.convertContainer(container) },
        .ident => |ident| .{ .pointer = ident.swap(try self.lookupNameExpected(ident)) },
        .dereference => |deref| .{ .dereference = try deref.map(self, convertExprPtr) },
        .parentheses => |parens| try self.convertExprRaw(parens.value.*),
        .member_access => |access| .{ .member_access = .{
            .container = try access.container.map(self, convertExprPtr),
            .member = access.member,
        } },
    };
}

pub fn lookupName(self: *@This(), name: Ranged(Token)) ?Index(ir.Def) {
    var current = self.current;

    while (current) |scope| {
        const container = self.indexContainer(scope);

        if (container.defs.getPtr(name.range.substr(self.src))) |def| {
            return def.value.def;
        }

        current = container.parent;
    } else return null;
}

pub fn lookupNameExpected(self: *@This(), name: Ranged(Token)) Err!Index(ir.Def) {
    const index = self.lookupName(name);

    return index orelse self.fail(.{ .unknown_identifier = name.range });
}

pub fn lookupNameForDefine(self: *@This(), name: Ranged(Token)) Err!void {
    const maybe_index = self.lookupName(name);

    if (maybe_index) |index| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = self.indexDef(index).name.range,
            .redeclared = name.range,
        } });
    }
}

pub fn lookupNameCurrentContainerForDefine(self: *@This(), name: Ranged(Token), defs: *std.StringArrayHashMap(Ranged(ir.ContainerDef))) Err!void {
    const value = name.range.substr(self.src);

    // This used to be merged into one with a `defs.getOrPut`, which has better
    // performance, but unfortunately has different semantics (would require
    // removing on fail, etc.).
    if (defs.get(value)) |def| {
        const index = def.value.def;

        return self.fail(.{ .duplicate_member_name = .{
            .declared = self.indexDef(index).name.range,
            .redeclared = name.range,
        } });
    }

    try self.lookupNameForDefine(name);
}

pub fn indexContainer(self: *@This(), index: Index(ir.Container)) *ir.Container {
    return &self.containers.items[index.index];
}

pub fn indexDef(self: *@This(), index: Index(ir.Def)) *ir.Def {
    return &self.defs.items[index.index];
}

pub fn indexExpr(self: *@This(), index: Index(ir.Expr)) *ir.Expr {
    return &self.exprs.items[index.index];
}

pub fn pushContainer(self: *@This(), container: ir.Container) error{OutOfMemory}!Index(ir.Container) {
    const index = self.containers.items.len;
    try self.containers.append(container);
    return .{ .index = index };
}

pub fn pushDef(self: *@This(), def: ir.Def) error{OutOfMemory}!Index(ir.Def) {
    const index = self.defs.items.len;
    try self.defs.append(def);
    return .{ .index = index };
}

pub fn pushExpr(self: *@This(), expr: ir.Expr) error{OutOfMemory}!Index(ir.Expr) {
    const index = self.exprs.items.len;
    try self.exprs.append(expr);
    return .{ .index = index };
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{CodeError} {
    self.error_context = @"error";
    return error.CodeError;
}

pub fn printContainer(self: *const @This(), index: Index(ir.Container), writer: anytype) anyerror!void {
    const container = self.containers.items[index.index];

    try writer.writeAll("container { ");

    var iter = container.defs.iterator();
    while (iter.next()) |def| {
        try self.printContainerDef(def.value_ptr.value, writer);
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

pub fn printContainerDef(self: *const @This(), def: ir.ContainerDef, writer: anytype) anyerror!void {
    if (def.access == .public) try writer.writeAll("pub ");

    try self.printDef(def.def, writer);
}

pub fn printDef(self: *const @This(), index: Index(ir.Def), writer: anytype) anyerror!void {
    const def = self.defs.items[index.index];

    try writer.writeAll(switch (def.mutability) {
        .constant => "const",
        .variable => "var",
    });
    try writer.writeByte(' ');

    try self.printRange(def.name.range, writer);
    try writer.print("<{}>", .{index.index});

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

pub fn printExpr(self: *const @This(), index: Index(ir.Expr), writer: anytype) anyerror!void {
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
        .decl => |decl| try self.printDecl(decl, writer),
        .pointer_type => |ptr| {
            try writer.writeByte('*');
            try self.printExpr(ptr.value, writer);
        },
        .container => |container| try self.printContainer(container, writer),
        .pointer => |ptr| {
            try self.printRange(ptr.range, writer);
            try writer.print("<{}>", .{ptr.value.index});
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

const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const ast = @import("Parser.zig").ast;
const failure = @import("failure.zig");

/// Represents an index of a type. The type annotation currently doesn't do
/// anything; it just makes code more legible.
pub fn Index(T: type) type {
    _ = T;
    return usize;
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

    pub const Interface = struct {
        decls: std.StringArrayHashMap(Ranged(Decl)),
    };

    pub const ContainerDef = struct {
        access: ast.Access,
        def: Index(Def),
    };

    pub const Def = struct {
        mutability: ast.Mutability,
        name: Ranged(Token),
        type: ?Ranged(Expr),
        value: Ranged(Expr),

        /// Whether or not this definition is in the process of being evaluated.
        /// When a definition needs to be evaluated while it's already being
        /// evaluated, this means its value depends on itself, and thus a
        /// dependency loop exists.
        evaluating: bool = false,
    };

    pub const Decl = struct {
        name: Ranged(Token),
        type: Ranged(Expr),

        /// Whether or not this definition is in the process of being evaluated.
        /// When a definition needs to be evaluated while it's already being
        /// evaluated, this means its value depends on itself, and thus a
        /// dependency loop exists.
        evaluating: bool = false,
    };

    pub const Type = union(enum) {
        word,
        pointer: Ranged(*Expr),
        interface: Interface,
        type,
    };

    pub const Expr = union(enum) {
        word: u32,
        type: Type,
        container: Index(Container),
        pointer: Ranged(Index(ir.Def)),
        dereference: Ranged(*Expr),
        parentheses: Ranged(*Expr),
        member_access: struct {
            container: Ranged(*Expr),
            member: Ranged(Token),
        },
    };
};

/// The error set of errors that can occur while converting IR.
pub const Error = error{
    IrError,
    OutOfMemory,
};

src: [:0]const u8,
allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.IrError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?failure.Error = null,

containers: std.ArrayList(ir.Container),
defs: std.ArrayList(ir.Def),

// TODO: Will need to refactor when functions are added
current: ?Index(ir.Container) = null,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .containers = std.ArrayList(ir.Container).init(allocator),
        .defs = std.ArrayList(ir.Def).init(allocator),
    };
}

pub fn convertContainer(self: *@This(), container: ast.Container) Error!Index(ir.Container) {
    var defs = std.StringArrayHashMap(Ranged(ir.ContainerDef)).init(self.allocator);

    // Add references with no values
    for (container.defs.items) |def| {
        const name = def.value.def.name;
        const key = name.range.substr(self.src);

        const def_index = self.defs.items.len;

        try self.lookupNameCurrentContainerForDefine(name, &defs);
        try defs.putNoClobber(key, def.swap(ir.ContainerDef{
            .access = def.value.access,
            .def = def_index,
        }));

        try self.defs.append(undefined);
    }

    const index = self.containers.items.len;
    try self.containers.append(.{
        .defs = defs,
        .parent = self.current,
    });

    self.current = index;

    // Add the values of the references
    for (container.defs.items) |def| {
        const key = def.value.def.name.range.substr(self.src);
        const stored_def = self.containers.items[index].defs.get(key) orelse unreachable; // We just added it, so it must exist

        const converted = try self.convertDef(def.value.def);
        self.defs.items[stored_def.value.def] = converted;
    }

    // We set current above, so it must be valid
    self.current = self.containers.items[self.current.?].parent;

    return index;
}

pub fn convertInterface(self: *@This(), interface: ast.Interface) Error!ir.Interface {
    var decls = std.StringArrayHashMap(Ranged(ir.Decl)).init(self.allocator);

    for (interface.decls.items) |decl| {
        const name = decl.value.name.range.substr(self.src);

        const result = try decls.getOrPut(name);

        if (result.found_existing) return self.fail(.{ .duplicate_member_name = .{
            .declared = result.value_ptr.value.name.range,
            .redeclared = decl.value.name.range,
        } });

        result.value_ptr.* = try decl.map(self, convertDecl);
    }

    return .{
        .decls = decls,
    };
}

pub fn convertContainerDef(self: *@This(), def: ast.ContainerDef) Error!ir.ContainerDef {
    return .{
        .access = def.access,
        .def = try self.convertDef(def.def),
    };
}

pub fn convertDef(self: *@This(), def: ast.Def) Error!ir.Def {
    return .{
        .mutability = def.mutability,
        .name = def.name,
        .type = if (def.type) |@"type"| try @"type".map(self, convertExpr) else null,
        .value = try def.value.map(self, convertExpr),
    };
}

pub fn convertDecl(self: *@This(), decl: ast.Decl) Error!ir.Decl {
    return .{
        .name = decl.name,
        .type = try decl.type.map(self, convertExpr),
    };
}

pub fn convertExprPtr(self: *@This(), expr: *ast.Expr) Error!*ir.Expr {
    const ptr = try self.allocator.create(ir.Expr);
    ptr.* = try self.convertExpr(expr.*);
    return ptr;
}

pub fn convertExpr(self: *@This(), expr: ast.Expr) Error!ir.Expr {
    return switch (expr) {
        .word => |word| .{ .word = word },
        .type => |@"type"| .{ .type = try self.convertType(@"type") },
        .container => |container| .{ .container = try self.convertContainer(container) },
        .ident => |ident| .{ .pointer = ident.swap(try self.lookupNameExpected(ident)) },
        .dereference => |deref| .{ .dereference = try deref.map(self, convertExprPtr) },
        .parentheses => |parens| try self.convertExpr(parens.value.*),
        .member_access => |access| .{ .member_access = .{
            .container = try access.container.map(self, convertExprPtr),
            .member = access.member,
        } },
    };
}

pub fn convertType(self: *@This(), @"type": ast.Type) Error!ir.Type {
    return switch (@"type") {
        .word => .word,
        .interface => |interface| .{ .interface = try self.convertInterface(interface) },
        .pointer => |ptr| .{ .pointer = try ptr.map(self, convertExprPtr) },
        .type => .type,
    };
}

pub fn lookupName(self: *@This(), name: Ranged(Token)) ?Index(ir.Def) {
    var current = self.current;

    while (current) |scope| {
        const container = self.containers.items[scope];

        if (container.defs.getPtr(name.range.substr(self.src))) |def| {
            return def.value.def;
        }

        current = container.parent;
    } else return null;
}

pub fn lookupNameExpected(self: *@This(), name: Ranged(Token)) Error!Index(ir.Def) {
    const index = self.lookupName(name);

    return index orelse self.fail(.{ .unknown_identifier = name.range });
}

pub fn lookupNameForDefine(self: *@This(), name: Ranged(Token)) Error!void {
    const maybe_index = self.lookupName(name);

    if (maybe_index) |index| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = self.defs.items[index].name.range,
            .redeclared = name.range,
        } });
    }
}

pub fn lookupNameCurrentContainerForDefine(self: *@This(), name: Ranged(Token), defs: *std.StringArrayHashMap(Ranged(ir.ContainerDef))) Error!void {
    const value = name.range.substr(self.src);

    // This used to be merged into one with a `defs.getOrPut`, which has better
    // performance, but unfortunately has different semantics (would require
    // removing on fail, etc.).
    if (defs.get(value)) |def| {
        const index = def.value.def;

        return self.fail(.{ .duplicate_member_name = .{
            .declared = self.defs.items[index].name.range,
            .redeclared = name.range,
        } });
    }

    try self.lookupNameForDefine(name);
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{IrError} {
    self.error_context = @"error";
    return error.IrError;
}

pub fn printContainer(self: *const @This(), index: Index(ir.Container), writer: anytype) anyerror!void {
    const container = self.containers.items[index];

    try writer.writeAll("container { ");

    var iter = container.defs.iterator();
    while (iter.next()) |def| {
        try self.printContainerDef(def.value_ptr.value, writer);
        try writer.writeByte(' ');
    }

    try writer.writeByte('}');
}

pub fn printInterface(self: *const @This(), interface: ir.Interface, writer: anytype) anyerror!void {
    try writer.writeAll("interface { ");

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
    const def = self.defs.items[index];

    try writer.writeAll(switch (def.mutability) {
        .constant => "const",
        .variable => "var",
    });
    try writer.writeByte(' ');

    try self.printRange(def.name.range, writer);
    try writer.print("<{}>", .{index});

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
    try writer.writeAll(";");
}

pub fn printExpr(self: *const @This(), expr: ir.Expr, writer: anytype) anyerror!void {
    switch (expr) {
        .word => |word| try writer.print("{}", .{word}),
        .type => |@"type"| try self.printType(@"type", writer),
        .container => |container| try self.printContainer(container, writer),
        .pointer => |ptr| {
            try self.printRange(ptr.range, writer);
            try writer.print("<{}>", .{ptr.value});
        },
        .dereference => |deref| {
            try self.printExpr(deref.value.*, writer);
            try writer.writeAll(".*");
        },
        .parentheses => |parens| {
            try writer.writeByte('(');
            try self.printExpr(parens.value.*, writer);
            try writer.writeByte(')');
        },
        .member_access => |member| {
            try self.printExpr(member.container.value.*, writer);
            try writer.writeByte('.');
            try self.printRange(member.member.range, writer);
        },
    }
}

pub fn printType(self: *const @This(), @"type": ir.Type, writer: anytype) anyerror!void {
    switch (@"type") {
        .word => try writer.writeAll("word"),
        .type => try writer.writeAll("type"),
        .interface => |interface| try self.printInterface(interface, writer),
        .pointer => |ptr| {
            try writer.writeByte('*');
            try self.printExpr(ptr.value.*, writer);
        },
    }
}

pub fn printRange(self: *const @This(), range: tokenizer.Range, writer: anytype) anyerror!void {
    try writer.writeAll(range.substr(self.src));
}

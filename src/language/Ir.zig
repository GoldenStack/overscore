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

        decls: std.StringArrayHashMap(Ranged(ContainerDecl)),
    };

    pub const ContainerDecl = struct {
        access: ast.Access,
        decl: Index(Decl),
    };

    pub const Decl = struct {
        mutability: ast.Mutability,
        name: Ranged(Token),
        type: ?Ranged(Expr),
        value: Ranged(Expr),

        /// Whether or not this declaration is in the process of being
        /// evaluated. When a declaration needs to be evaluated while it's
        /// already being evaluated, this means its value depends on itself, and
        /// thus a dependency loop exists.
        evaluating: bool = false,
    };

    pub const Type = union(enum) {
        word,
        container: Index(Container),
        pointer: Ranged(*Expr),
        type,
    };

    pub const Expr = union(enum) {
        word: u32,
        type: Type,
        pointer: Ranged(Index(ir.Decl)),
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
decls: std.ArrayList(ir.Decl),

// TODO: Will need to refactor when functions are added
current: ?Index(ir.Container) = null,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .containers = std.ArrayList(ir.Container).init(allocator),
        .decls = std.ArrayList(ir.Decl).init(allocator),
    };
}

pub fn convertContainer(self: *@This(), container: ast.Container) Error!Index(ir.Container) {
    var decls = std.StringArrayHashMap(Ranged(ir.ContainerDecl)).init(self.allocator);

    // Add references with no values
    for (container.decls.items) |decl| {
        const name = decl.value.decl.name;
        const key = name.range.substr(self.src);

        const decl_index = self.decls.items.len;

        try self.lookupNameDeclareContainer(name, &decls);
        try decls.putNoClobber(key, decl.swap(ir.ContainerDecl{
            .access = decl.value.access,
            .decl = decl_index,
        }));

        try self.decls.append(undefined);
    }

    const index = self.containers.items.len;
    try self.containers.append(.{
        .decls = decls,
        .parent = self.current,
    });

    self.current = index;

    // Add the values of the references
    for (container.decls.items) |decl| {
        const key = decl.value.decl.name.range.substr(self.src);
        const stored_decl = self.containers.items[index].decls.get(key) orelse unreachable; // We just added it, so it must exist

        const converted = try self.convertDecl(decl.value.decl);
        self.decls.items[stored_decl.value.decl] = converted;
    }

    // We set current above, so it must be valid
    self.current = self.containers.items[self.current.?].parent;

    return index;
}

pub fn convertContainerDecl(self: *@This(), decl: ast.ContainerDecl) Error!ir.ContainerDecl {
    return .{
        .access = decl.access,
        .decl = try self.convertDecl(decl.decl),
    };
}

pub fn convertDecl(self: *@This(), decl: ast.Decl) Error!ir.Decl {
    return .{
        .mutability = decl.mutability,
        .name = decl.name,
        .type = if (decl.type) |@"type"| try @"type".map(self, convertExpr) else null,
        .value = try decl.value.map(self, convertExpr),
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
        .container => |container| .{ .container = try self.convertContainer(container) },
        .pointer => |ptr| .{ .pointer = try ptr.map(self, convertExprPtr) },
        .type => .type,
    };
}

pub fn lookupName(self: *@This(), name: Ranged(Token)) ?Index(ir.Decl) {
    var current = self.current;

    while (current) |scope| {
        const container = self.containers.items[scope];

        if (container.decls.getPtr(name.range.substr(self.src))) |decl| {
            return decl.value.decl;
        }

        current = container.parent;
    } else return null;
}

pub fn lookupNameExpected(self: *@This(), name: Ranged(Token)) Error!Index(ir.Decl) {
    const index = self.lookupName(name);

    return index orelse self.fail(.{ .unknown_identifier = name.range });
}

pub fn lookupNameDeclare(self: *@This(), name: Ranged(Token)) Error!void {
    const maybe_index = self.lookupName(name);

    if (maybe_index) |index| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = self.decls.items[index].name.range,
            .redeclared = name.range,
        } });
    }
}

pub fn lookupNameDeclareContainer(self: *@This(), name: Ranged(Token), decls: *std.StringArrayHashMap(Ranged(ir.ContainerDecl))) Error!void {
    const value = name.range.substr(self.src);

    // This used to be merged into one with a `decls.getOrPut`, which has better
    // performance, but unfortunately has different semantics (would require
    // removing on fail, etc.).
    if (decls.get(value)) |decl| {
        const index = decl.value.decl;

        return self.fail(.{ .duplicate_member_name = .{
            .declared = self.decls.items[index].name.range,
            .redeclared = name.range,
        } });
    }

    try self.lookupNameDeclare(name);
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{IrError} {
    self.error_context = @"error";
    return error.IrError;
}

pub fn printContainer(self: *const @This(), index: Index(ir.Container), writer: anytype) anyerror!void {
    const container = self.containers.items[index];

    try writer.writeAll("{ ");

    var iter = container.decls.iterator();
    while (iter.next()) |decl| {
        try self.printContainerDecl(decl.value_ptr.value, writer);
        try writer.writeByte(' ');
    }

    try writer.writeByte('}');
}

pub fn printContainerDecl(self: *const @This(), decl: ir.ContainerDecl, writer: anytype) anyerror!void {
    if (decl.access == .public) try writer.writeAll("pub ");

    try self.printDecl(decl.decl, writer);
}

pub fn printDecl(self: *const @This(), index: Index(ir.Decl), writer: anytype) anyerror!void {
    const decl = self.decls.items[index];

    try writer.writeAll(switch (decl.mutability) {
        .constant => "const",
        .variable => "var",
    });
    try writer.writeByte(' ');

    try self.printRange(decl.name.range, writer);
    try writer.print("<{}>", .{index});

    if (decl.type) |type_specifier| {
        try writer.writeAll(": ");
        try self.printExpr(type_specifier.value, writer);
    }

    try writer.writeAll(" = ");
    try self.printExpr(decl.value.value, writer);
    try writer.writeAll(";");
}

pub fn printExpr(self: *const @This(), expr: ir.Expr, writer: anytype) anyerror!void {
    switch (expr) {
        .word => |word| try writer.print("{}", .{word}),
        .type => |@"type"| try self.printType(@"type", writer),
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
        .container => |container| try self.printContainer(container, writer),
        .pointer => |ptr| {
            try writer.writeByte('*');
            try self.printExpr(ptr.value.*, writer);
        },
    }
}

pub fn printRange(self: *const @This(), range: tokenizer.Range, writer: anytype) anyerror!void {
    try writer.writeAll(range.substr(self.src));
}

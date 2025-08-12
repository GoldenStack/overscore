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

    pub const Literal = struct {
        total_range: Range,
        range: Range,
    };

    pub const Def = struct {
        access: ast.Access,
        mutability: ast.Mutability,
        name: Literal,
        type: ?Index,
        value: Index,

        /// Whether or not type checking has started for this definition.
        /// Generally it's not a good idea to do the same thing for evaluation
        /// (trusting that something has been evaluated is error-prone) but this
        /// works for type checking.
        type_checked: bool = false,
    };

    pub const Decl = struct {
        name: Literal,
        type: Index,
    };

    /// Represents a declaration that may or may not have its type computed yet.
    /// This is primarily for cases of product types calculated from fields, and
    /// prevents dependency loop issues.
    pub const LazyDecl = union(enum) {
        decl: IndexOf(.decl),
        def: IndexOf(.def),
    };

    pub const MemberAccess = struct {
        container: Index,
        member: Range,
    };

    pub const Expr = union(Tag) {
        pub const Tag = enum { word, word_type, literal, def, decl, product, sum, pointer_type, type, container, pointer, dereference, member_access, indirect_member_access, coerce };
        word: u32,
        word_type,
        literal: Literal,
        def: Def,
        decl: Decl,
        product: std.StringArrayHashMap(LazyDecl),
        sum: std.StringArrayHashMap(LazyDecl),
        pointer_type: Index,
        type,
        container: Container,
        pointer: IndexOf(.def),
        dereference: Index,
        member_access: MemberAccess,
        indirect_member_access: MemberAccess,
        coerce: struct {
            expr: Index,
            type: Index,
        },
    };
};

pub const Value = struct {
    range: Range,

    expr: ir.Expr,

    /// The type of the expression. This must always align with the expression
    /// value and thus by typed manually.
    ///
    /// Must be an evaluated type expression (word_type, decl, product, sum, pointer_type, type, container).
    type: ?Index,

    /// Whether or not this expression is in the process of being evaluated.
    /// When an expression needs to be evaluated while it's already being
    /// evaluated, this means its value depends on itself, and thus a dependency
    /// loop exists.
    evaluating: bool = false,
};

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

values: std.MultiArrayList(Value),

// TODO: Will need to refactor when functions are added
current: ?IndexOf(.container) = null,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .values = std.MultiArrayList(Value){},
    };
}

pub fn at(self: *@This(), comptime field: std.meta.FieldEnum(Value), index: Index) *std.meta.FieldType(Value, field) {
    return &self.values.items(field)[index.index];
}

pub fn atOf(self: *@This(), comptime variant: std.meta.Tag(ir.Expr), index: IndexOf(variant)) *std.meta.TagPayload(ir.Expr, index.variant) {
    return &@field(self.at(.expr, index.index), @tagName(index.variant));
}

pub fn push(self: *@This(), expr: ir.Expr, range: Range) error{OutOfMemory}!Index {
    const index = self.values.len;
    try self.values.append(self.allocator, .{
        .expr = expr,
        .range = range,
        .type = null,
    });
    return .{ .index = index };
}

pub fn convertExpr(self: *@This(), expr: Ranged(ast.Expr)) Err!Index {
    const index = try self.push(undefined, expr.range);

    const value: ir.Expr = value: switch (expr.value) {
        .word_type => .word_type,
        .type => .type,

        .word => |word| .{ .word = word },

        .literal => |literal| .{ .literal = try self.convertLiteral(expr.replace(literal)) },
        .def => |def| .{ .def = try self.convertDef(def) },
        .decl => |decl| .{ .decl = try self.convertDecl(decl) },
        .product => |product| .{ .product = try self.convertDefList(product, expr.range) },
        .sum => |sum| .{ .sum = try self.convertDefList(sum, expr.range) },

        .pointer_type => |ptr| .{ .pointer_type = try self.convertExprPtr(ptr) },
        .ident => |ident| .{ .pointer = try self.lookupNameExpected(ident.range) },
        .dereference => |deref| .{ .dereference = try self.convertExprPtr(deref) },
        .parentheses => |parens| continue :value parens.value.*,

        .member_access => |access| switch (access.indirection) {
            .direct => .{ .member_access = .{
                .container = try self.convertExprPtr(access.container),
                .member = access.member.range,
            } },
            .indirect => .{ .indirect_member_access = .{
                .container = try self.convertExprPtr(access.container),
                .member = access.member.range,
            } },
        },

        .container => |container| {
            try self.convertContainer(container, expr.range, index);
            return index; // Prevents duplicate writes
        },
    };

    self.at(.expr, index).* = value;
    return index;
}

fn convertDefList(self: *@This(), exprs: std.ArrayList(Ranged(ast.Expr)), range: Range) Err!std.StringArrayHashMap(ir.LazyDecl) {
    var out_exprs = std.StringArrayHashMap(ir.LazyDecl).init(self.allocator);

    for (exprs.items) |item| {
        const expr = try self.convertExpr(item);

        if (self.at(.expr, expr).* != .decl) return self.fail(.{ .can_only_multiply_or_add_decls = .{
            .invalid_field = item.range,
            .typedef = range,
        } });

        const decl_expr: IndexOf(.decl) = .{ .index = expr };

        const literal = self.atOf(.decl, decl_expr).name;

        const name = literal.range.substr(self.src);

        if (out_exprs.get(name)) |existing_expr| return self.fail(.{ .duplicate_member = .{
            .declared = self.lazyDeclName(existing_expr),
            .redeclared = literal.range,
        } });

        try out_exprs.put(name, .{ .decl = decl_expr });
    }

    return out_exprs;
}

fn lazyDeclName(self: *@This(), lazy: ir.LazyDecl) Range {
    return switch (lazy) {
        .decl => |decl| self.atOf(.decl, decl).name,
        .def => |def| self.atOf(.def, def).name,
    }.range;
}

fn convertExprPtr(self: *@This(), expr: Ranged(*ast.Expr)) Err!Index {
    return self.convertExpr(expr.replace(expr.value.*));
}

pub fn convertContainer(self: *@This(), container: std.ArrayList(Ranged(ast.Expr)), range: Range, index: Index) Err!void {
    var defs = std.StringArrayHashMap(IndexOf(.def)).init(self.allocator);

    // Add references with no values
    for (container.items) |def_range| {
        const def = switch (def_range.value) {
            .def => |def| def,
            else => return self.fail(.{ .can_only_cons_definitions = .{
                .invalid_field = def_range.range,
                .typedef = range,
            } }),
        };

        const name = try self.convertLiteral(def.name);
        const key = name.range.substr(self.src);

        const raw_index = try self.push(.{ .def = .{
            .access = def.access,
            .mutability = def.mutability,
            .name = name,
            .type = if (def.type == null) null else undefined,
            .value = undefined,
        } }, def_range.range);

        const def_index: IndexOf(.def) = .{ .index = raw_index };

        try self.lookupNameCurrentContainerForDefine(name.range, &defs);
        try defs.putNoClobber(key, def_index);
    }

    self.at(.expr, index).* = .{ .container = .{
        .defs = defs,
        .parent = self.current,
    } };

    self.current = .{ .index = index };

    // Add the values of the references
    for (container.items) |maybe_def| {
        const def = maybe_def.value.def; // We already guaranteed it's a definition

        const key = def.name.value.name.range.substr(self.src);
        const stored_def = self.atOf(.container, self.current.?).defs.get(key) orelse unreachable; // We just added it, so it must exist

        const converted = try self.convertDef(def);
        self.atOf(.def, stored_def).* = converted;
    }

    // We set current above, so it must be valid
    self.current = self.atOf(.container, self.current.?).parent;
}

pub fn convertLiteral(self: *@This(), literal: Ranged(ast.Literal)) Err!ir.Literal {
    _ = self;
    return .{
        .total_range = literal.range,
        .range = literal.value.name.range,
    };
}

pub fn convertDef(self: *@This(), def: ast.Def) Err!ir.Def {
    return .{
        .access = def.access,
        .mutability = def.mutability,
        .name = try self.convertLiteral(def.name),
        .type = if (def.type) |@"type"| try self.convertExprPtr(@"type") else null,
        .value = try self.convertExprPtr(def.value),
    };
}

pub fn convertDecl(self: *@This(), decl: ast.Decl) Err!ir.Decl {
    return .{
        .name = try self.convertLiteral(decl.name),
        .type = try self.convertExprPtr(decl.type),
    };
}

pub fn lookupName(self: *@This(), name: Range) ?IndexOf(.def) {
    var current = self.current;

    while (current) |scope| {
        const container = self.atOf(.container, scope);

        if (container.defs.get(name.substr(self.src))) |def| {
            return def;
        }

        current = container.parent;
    } else return null;
}

pub fn lookupNameExpected(self: *@This(), name: Range) Err!IndexOf(.def) {
    const index = self.lookupName(name);

    return index orelse self.fail(.{ .unknown_identifier = name });
}

pub fn lookupNameForDefine(self: *@This(), name: Range) Err!void {
    const maybe_index = self.lookupName(name);

    if (maybe_index) |index| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = self.atOf(.def, index).name.range,
            .redeclared = name,
        } });
    }
}

pub fn lookupNameCurrentContainerForDefine(self: *@This(), name: Range, defs: *std.StringArrayHashMap(IndexOf(.def))) Err!void {
    const value = name.substr(self.src);

    // This used to be merged into one with a `defs.getOrPut`, which has better
    // performance, but unfortunately has different semantics (would require
    // removing on fail, etc.).
    if (defs.get(value)) |def| {
        return self.fail(.{ .duplicate_member = .{
            .declared = self.atOf(.def, def).name.range,
            .redeclared = name,
        } });
    }

    try self.lookupNameForDefine(name);
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": failure.Error) error{CodeError} {
    self.error_context = @"error";
    return error.CodeError;
}

pub fn printContainer(self: *@This(), container: ir.Container, writer: anytype) anyerror!void {
    try writer.writeAll("container { ");

    var iter = container.defs.iterator();
    while (iter.next()) |def| {
        try self.printExpr(def.value_ptr.index, writer);
        try writer.writeByte(' ');
    }

    try writer.writeByte('}');
}

pub fn printLiteral(self: *@This(), literal: ir.Literal, writer: anytype) anyerror!void {
    try writer.writeByte('.');
    try self.printRange(literal.range, writer);
}

pub fn printDef(self: *@This(), def: ir.Def, writer: anytype) anyerror!void {
    if (def.access == .public) try writer.writeAll("pub ");

    try writer.writeAll(switch (def.mutability) {
        .constant => "const",
        .variable => "var",
    });
    try writer.writeByte(' ');

    try self.printLiteral(def.name, writer);

    if (def.type) |type_specifier| {
        try writer.writeAll(": ");
        try self.printExpr(type_specifier, writer);
    }

    try writer.writeAll(" = ");
    try self.printExpr(def.value, writer);
    try writer.writeAll(";");
}

pub fn printDecl(self: *@This(), decl: ir.Decl, writer: anytype) anyerror!void {
    try self.printLiteral(decl.name, writer);
    try writer.writeAll(": ");
    try self.printExpr(decl.type, writer);
}

pub fn printExpr(self: *@This(), index: Index, writer: anytype) anyerror!void {
    switch (self.at(.expr, index).*) {
        .word => |word| try writer.print("{}", .{word}),
        .word_type => try writer.writeAll("word"),
        .type => try writer.writeAll("type"),
        .product => |decls| {
            try writer.writeAll("(");
            for (0.., decls.values()) |i, lazy| {
                if (i != 0) try writer.writeAll(" and ");
                try self.printLazyDecl(lazy, writer);
            }
            try writer.writeAll(")");
        },
        .sum => |decls| {
            try writer.writeAll("(");
            for (0.., decls.values()) |i, lazy| {
                if (i != 0) try writer.writeAll(" or ");
                try self.printLazyDecl(lazy, writer);
            }
            try writer.writeAll(")");
        },
        .literal => |literal| try self.printLiteral(literal, writer),
        .def => |def| try self.printDef(def, writer),
        .decl => |decl| try self.printDecl(decl, writer),
        .pointer_type => |ptr| {
            try writer.writeByte('*');
            try self.printExpr(ptr, writer);
        },
        .container => |container| try self.printContainer(container, writer),
        .pointer => |ptr| {
            try self.printRange(self.at(.range, index).*, writer);
            try writer.print("<{}>", .{ptr.index.index});
        },
        .dereference => |deref| {
            try self.printExpr(deref, writer);
            try writer.writeAll(".*");
        },
        .member_access => |member| {
            try self.printExpr(member.container, writer);
            try writer.writeByte('.');
            try self.printRange(member.member, writer);
        },
        .indirect_member_access => |member| {
            try self.printExpr(member.container, writer);
            try writer.writeAll("->");
            try self.printRange(member.member, writer);
        },
        .coerce => |coerce| {
            try self.printExpr(coerce.expr, writer);
            try writer.writeAll(" as ");
            try self.printExpr(coerce.type, writer);
        },
    }
}

fn printLazyDecl(self: *@This(), lazy: ir.LazyDecl, writer: anytype) anyerror!void {
    return switch (lazy) {
        .decl => |decl| try self.printExpr(decl.index, writer),
        .def => |def| {
            try writer.writeAll("lazy ");
            try self.printExpr(def.index, writer);
        },
    };
}

fn printRange(self: *@This(), range: tokenizer.Range, writer: anytype) anyerror!void {
    try writer.writeAll(range.substr(self.src));
}

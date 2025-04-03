const std = @import("std");
const Parser = @import("Parser.zig");
const tokenizer = @import("tokenizer.zig");
const Error = @import("failure.zig").Error;
const Ranged = tokenizer.Ranged;
const Token = tokenizer.Token;

/// Possible fully-evaluated type expressions.
pub const Type = union(enum) {
    /// The empty type. This has no inhabitants, and is thus equivalent to the
    /// concept of `noreturn`. This is equivalent to `sum {}`.
    empty,

    /// The unit type. This has exactly one inhabitant, so is equivalent to
    /// `null`, `void`, etc. This is equivalent to `product {}`.
    unit,

    /// A function that can be called. This is the only way of expressing any
    /// sort of "lazy" evaluation, because functions are not (and usually can
    /// not) be eagerly evaluated.
    ///
    /// A function signature works like this: `fn(i32, i32) i32`.
    function: struct {
        parameters: std.ArrayList(Ranged(Expr)),
        @"return": Ranged(*Expr),
    },

    /// A container. See docs for `Container` for more specifics.
    container: Container,

    /// A wrapper around a type that makes it distinct from other types with
    /// identical structural equality..
    ///
    /// Since all types are, by default, equal by structure (i.e. `product { u32
    /// } == struct { u32 }`), a distinct type simply disrupts structural
    /// equality rules, so, for example, `distinct struct { u32 } != distinct
    /// struct { u32 }`.
    ///
    /// This does not necessarily affect coercion (i.e. `product { 5 }` being
    /// coercible to the type `distinct product { u32 }`).
    distinct: Ranged(*Expr),

    /// An integer type. For now, integers are always unsigned 32-bit integers,
    /// but in the future they will ideally be ranged, similar to the proposal
    /// for Zig.
    integer,

    /// A type.
    ///
    /// When the value of an expression is a type, the type of the expression is
    /// `type`. This does lead to unsoundness in the type system due to
    /// Russell's paradox, likely the only point of unsoundness, but I don't
    /// really care.
    type,
};

/// Containers can take four main types:
///
/// - The product of multiple types, equivalent to a tuple. For example,
///   `const Pos = product { u32, u32 }`
///
/// - The product of multiple types, including a tag for each type. This is
///   equivalent to a struct. For example, `const Pos = product { x: u32, y: u32
///   }`
///
/// - The sum of multiple types. This is equivalent to a union, as there is, in
///   memory, always overlap between multiple types as they're always
///   represented as bits. For example, `const Ip = sum { u32, [4]u8 }`.
///
/// - The sum of multiple types, including a tag for each type. This is
///   equivalent to a tagged union. For example, `const Ip = sum { v4: u32, v6:
///   u128 }`
pub const Container = struct {
    variant: Parser.ContainerVariant,
    fields: Fields,
    decls: std.ArrayList(Ranged(ContainerDecl)),
};

pub const Fields = union(Parser.TaggedStatus) {
    untagged: std.ArrayList(Ranged(Expr)),
    tagged: std.ArrayList(Ranged(NamedExpr)),
};

pub const NamedExpr = struct {
    name: Ranged(Token),
    value: Ranged(Expr),
};

pub const ContainerDecl = struct {
    access: Parser.Access,
    decl: Ranged(Decl),
};

pub const Decl = struct {
    mutability: Parser.Mutability,
    name: Ranged(Token),
    value: Ranged(Expr),
};

pub const Expr = union(enum) {
    function: struct {
        parameters: std.ArrayList(Ranged(NamedExpr)),
        @"return": Ranged(*Expr),
        body: Ranged(Block),
    },
    call: struct {
        function: Ranged(*Expr),
        arguments: std.ArrayList(Ranged(Expr)),
    },
    container: Ranged(Container),
    ident: usize,
    block: Ranged(Block),
    number: u32,
    type: Type,
    property: struct {
        container: Ranged(*Expr),
        property: Ranged(Token),
    },
    @"if": struct {
        condition: Ranged(*Expr),
        then: Ranged(*Expr),
        @"else": ?Ranged(*Expr),
    },
    do_while: struct {
        do: Ranged(*Expr),
        @"while": Ranged(*Expr),
    },
    while_do: struct {
        do: Ranged(*Expr),
        @"while": Ranged(*Expr),
    },
    @"return": Ranged(*Expr),
};

pub const Block = struct {
    stmts: std.ArrayList(Ranged(Stmt)),
};

pub const Stmt = union(enum) {
    decl: Ranged(Decl),
    expr: Ranged(Expr),
};

/// The error set of errors that can occur while compiling.
pub const CompilerError = error{
    CompilerError,
    OutOfMemory,
};

src: [:0]const u8,
allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.CompilerError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?Error = null,

namespace: std.ArrayList(*Ranged(Decl)),
names: TokenHashMap(usize), // Maps to index in namespace

pub fn init(src: [:0]const u8, allocator: std.mem.Allocator) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .namespace = std.ArrayList(*Ranged(Decl)).init(allocator),
        .names = TokenHashMap(usize).initContext(allocator, .{
            .src = src,
        }),
    };
}

/// Currently does not preserve order; waiting on
/// [#23400](https://github.com/ziglang/zig/issues/23400) and/or
/// [#23362](https://github.com/ziglang/zig/issues/23362) to become an
/// `ArrayHashMap` once again.
fn TokenHashMap(comptime V: type) type {
    const Context = struct {
        src: [:0]const u8,

        pub fn hash(self: @This(), token: Ranged(Token)) u64 {
            return std.hash_map.hashString(token.range.substr(self.src));
        }
        pub fn eql(self: @This(), a: Ranged(Token), b: Ranged(Token)) bool {
            return std.mem.eql(u8, a.range.substr(self.src), b.range.substr(self.src));
        }
    };

    return std.HashMap(Ranged(Token), V, Context, std.hash_map.default_max_load_percentage);
}

fn fail(self: *@This(), @"error": Error) error{CompilerError} {
    self.error_context = @"error";
    return error.CompilerError;
}

pub fn compile(self: *@This(), container: Ranged(Parser.Container)) CompilerError!Ranged(Container) {
    return container.map(self, semantics_container);
}

fn semantics_container(self: *@This(), container: Parser.Container) CompilerError!Container {
    var decls = std.ArrayList(Ranged(ContainerDecl)).init(self.allocator);
    for (container.decls.items) |decl| _ = try self.name_add(decl.value.decl.value.name);

    for (container.decls.items) |container_decl| {
        const decl = container_decl.value.decl;

        const value = container_decl.swap(ContainerDecl{
            .access = container_decl.value.access,
            .decl = decl.swap(Decl{
                .mutability = decl.value.mutability,
                .name = decl.value.name,
                .value = try decl.value.value.map(self, semantics_expr),
            }),
        });

        const index = self.names.get(decl.value.name).?;
        self.namespace.items[index].* = value.value.decl;

        try decls.append(value);
    }

    const fields = try self.homogenize_fields(container.fields, null, .untagged);

    for (container.decls.items) |decl| _ = self.names.remove(decl.value.decl.value.name);

    return .{
        .variant = container.variant,
        .fields = fields,
        .decls = decls,
    };
}

fn semantics_expr_ptr(self: *@This(), expr: Parser.Expr) CompilerError!*Expr {
    const ptr = try self.allocator.create(Expr);
    ptr.* = try self.semantics_expr(expr);
    return ptr;
}

fn semantics_expr(self: *@This(), expr: Parser.Expr) CompilerError!Expr {
    return switch (expr) {
        .function => |function| {
            return try if (function.body == null) self.semantics_function_type(expr) else self.semantics_function_declaration(expr);
        },
        .call => try self.semantics_call(expr),
        .container => |container| .{ .container = try container.map(self, semantics_container) },
        .ident => |ident| .{ .ident = self.names.get(ident) orelse return self.fail(.{
            .unknown_identifier = ident.range,
        }) },
        .block => |block| .{ .block = try block.map(self, semantics_block) },
        .number => |number| .{ .number = number },
        .parentheses => |parens| try self.semantics_expr(parens.value),
        .distinct => |distinct| .{ .type = .{ .distinct = try distinct.map(self, semantics_expr_ptr) } },
        .property => |property| .{ .property = .{
            .container = try property.container.map(self, semantics_expr_ptr),
            .property = property.property,
        } },
        .@"if" => |i| .{ .@"if" = .{
            .condition = try i.condition.map(self, semantics_expr_ptr),
            .then = try i.then.map(self, semantics_expr_ptr),
            .@"else" = if (i.@"else") |value| try value.map(self, semantics_expr_ptr) else null,
        } },
        .do_while => |do_while| .{ .do_while = .{
            .do = try do_while.do.map(self, semantics_expr_ptr),
            .@"while" = try do_while.@"while".map(self, semantics_expr_ptr),
        } },
        .while_do => |while_do| .{ .while_do = .{
            .@"while" = try while_do.@"while".map(self, semantics_expr_ptr),
            .do = try while_do.do.map(self, semantics_expr_ptr),
        } },
        .@"return" => |ret| .{ .@"return" = try ret.map(self, semantics_expr_ptr) },
    };
}

/// Expects that the provided expression is `.call`.
fn semantics_call(self: *@This(), expr: Parser.Expr) CompilerError!Expr {
    const call = expr.call;

    const ptr = try call.function.map(self, semantics_expr_ptr);

    var args = std.ArrayList(Ranged(Expr)).init(self.allocator);
    for (call.arguments.items) |arg| {
        try args.append(try arg.map(self, semantics_expr));
    }

    return .{ .call = .{
        .function = ptr,
        .arguments = args,
    } };
}

/// Assumes the expr is a function and that the function has a body
fn semantics_function_declaration(self: *@This(), expr: Parser.Expr) CompilerError!Expr {
    const function = expr.function;

    // 1. Dump names into namespace without evaluating
    for (function.parameters.items) |item| {
        if (item.value == .tagged) {
            // TODO: This induces name errors before correct tagged errors.
            _ = try self.name_add(item.value.tagged.name);
        }
    }

    // 2. Evaluate names and return value
    const params = try self.enforce_tagged_fields(function.parameters);
    const ret = try function.@"return".map(self, semantics_expr_ptr);

    // 3. Evaluate function body
    const body = try function.body.?.map(self, semantics_block);

    // 4. Clean up parameters in namespace
    for (params.items) |param| _ = self.names.remove(param.value.name);

    // 5. Return values
    return .{ .function = .{
        .parameters = params,
        .@"return" = ret,
        .body = body,
    } };
}

/// Assumes the expr is a function and that the function does not have a body
fn semantics_function_type(self: *@This(), expr: Parser.Expr) CompilerError!Expr {
    const function = expr.function;

    const fields = try self.enforce_untagged_fields(function.parameters);

    return .{ .type = .{ .function = .{
        .parameters = fields,
        .@"return" = try function.@"return".map(self, semantics_expr_ptr),
    } } };
}

fn semantics_block(self: *@This(), block: Parser.Block) CompilerError!Block {
    var stmts = std.ArrayList(Ranged(Stmt)).init(self.allocator);

    for (block.stmts.items) |stmt| {
        switch (stmt.value) {
            .decl => |decl| {
                _, const ptr = try self.name_add(decl.value.name);
                ptr.* = stmt.swap(Decl{
                    .mutability = decl.value.mutability,
                    .name = decl.value.name,
                    .value = try decl.value.value.map(self, semantics_expr),
                });

                try stmts.append(stmt.swap(Stmt{ .decl = ptr.* }));
            },
            .expr => |expr| try stmts.append(stmt.swap(Stmt{
                .expr = try expr.map(self, semantics_expr),
            })),
        }
    }

    for (block.stmts.items) |stmt| if (stmt.value == .decl) {
        _ = self.names.remove(stmt.value.decl.value.name);
    };

    return .{ .stmts = stmts };
}

fn name_add(self: *@This(), name: Ranged(Token)) CompilerError!struct { usize, *Ranged(Decl) } {
    if (self.names.getEntry(name)) |entry| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = entry.key_ptr.*.range,
            .redeclared = name.range,
        } });
    } else {
        const ptr = try self.allocator.create(Ranged(Decl));

        const index = self.namespace.items.len;
        try self.names.put(name, index);
        try self.namespace.append(ptr);

        return .{ index, ptr };
    }
}

fn homogenize_fields(self: *@This(), fields: std.ArrayList(Ranged(Parser.Field)), expected: ?Parser.TaggedStatus, default: Parser.TaggedStatus) CompilerError!Fields {
    if (fields.items.len == 0) {
        return switch (expected orelse default) {
            .untagged => .{ .untagged = std.ArrayList(Ranged(Expr)).init(self.allocator) },
            .tagged => .{ .tagged = std.ArrayList(Ranged(NamedExpr)).init(self.allocator) },
        };
    }

    return switch (expected orelse @as(Parser.TaggedStatus, fields.items[0].value)) {
        .untagged => .{ .untagged = try self.enforce_untagged_fields(fields) },
        .tagged => .{ .tagged = try self.enforce_tagged_fields(fields) },
    };
}

/// Expects at least one item in `fields` and that the first item is untagged.
fn enforce_untagged_fields(self: *@This(), fields: std.ArrayList(Ranged(Parser.Field))) CompilerError!std.ArrayList(Ranged(Expr)) {
    var list = std.ArrayList(Ranged(Expr)).init(self.allocator);

    if (fields.items.len > 0 and fields.items[0].value != .untagged) {
        return self.fail(.{ .expected_untagged_fields = .{
            .counterexample = fields.items[0].range,
        } });
    }

    for (fields.items) |item| {
        switch (item.value) {
            .untagged => |untagged| try list.append(try untagged.map(self, semantics_expr)),
            .tagged => return self.fail(.{ .expected_homogenous_fields = .{
                .untagged_example = fields.items[0].value.untagged.range,
                .tagged_example = item.range,
            } }),
        }
    }

    return list;
}

/// Expects at least one item in `fields` and that the first item is tagged.
fn enforce_tagged_fields(self: *@This(), fields: std.ArrayList(Ranged(Parser.Field))) CompilerError!std.ArrayList(Ranged(NamedExpr)) {
    var list = std.ArrayList(Ranged(NamedExpr)).init(self.allocator);

    if (fields.items.len > 0 and fields.items[0].value != .tagged) {
        return self.fail(.{ .expected_tagged_fields = .{
            .counterexample = fields.items[0].range,
        } });
    }

    for (fields.items) |item| {
        switch (item.value) {
            .untagged => return self.fail(.{ .expected_homogenous_fields = .{
                .untagged_example = item.range,
                .tagged_example = fields.items[0].range,
            } }),
            .tagged => |tagged| {
                for (list.items) |field| {
                    if (std.mem.eql(u8, field.value.name.range.substr(self.src), tagged.name.range.substr(self.src))) {
                        return self.fail(.{ .duplicate_tag = .{
                            .declared = field.value.name.range,
                            .redeclared = tagged.name.range,
                        } });
                    }
                }

                try list.append(item.swap(NamedExpr{
                    .name = tagged.name,
                    .value = try tagged.value.map(self, semantics_expr),
                }));
            },
        }
    }

    return list;
}

pub fn print_container(src: []const u8, container: Container, writer: anytype) anyerror!void {
    try writer.print("{s} {{ ", .{@tagName(container.variant)});

    try print_fields(src, container.fields, writer);

    for (container.decls.items) |decl| {
        try print_container_decl(src, decl.value, writer);
        try writer.writeAll(" ");
    }

    try writer.writeAll("}");
}

fn print_container_decl(src: []const u8, decl: ContainerDecl, writer: anytype) anyerror!void {
    try writer.print("{s} ", .{@tagName(decl.access)});
    try print_decl(src, decl.decl.value, writer);
}

fn print_decl(src: []const u8, decl: Decl, writer: anytype) anyerror!void {
    try writer.print("{s} {s} = ", .{ @tagName(decl.mutability), decl.name.range.substr(src) });
    try print_expr(src, decl.value.value, writer);
    try writer.writeAll(";");
}

fn print_fields(src: []const u8, fields: Fields, writer: anytype) anyerror!void {
    switch (fields) {
        .untagged => |untagged| {
            for (untagged.items) |field| {
                try print_expr(src, field.value, writer);
                try writer.writeAll(", ");
            }
        },
        .tagged => |tagged| {
            for (tagged.items) |field| {
                try print_named_expr(src, field.value, writer);
                try writer.writeAll(", ");
            }
        },
    }
}

fn print_named_expr(src: []const u8, named: NamedExpr, writer: anytype) anyerror!void {
    try writer.print("{s}: ", .{named.name.range.substr(src)});
    try print_expr(src, named.value.value, writer);
}

fn print_expr(src: []const u8, expr: Expr, writer: anytype) anyerror!void {
    switch (expr) {
        .function => |function| {
            try writer.writeAll("fn (");
            for (function.parameters.items) |param| {
                try print_named_expr(src, param.value, writer);
                try writer.writeAll(", ");
            }
            try writer.writeAll(") ");
            try print_expr(src, function.@"return".value.*, writer);
            try writer.writeAll(" ");
            try print_block(src, function.body.value, writer);
        },
        .call => |call| {
            try print_expr(src, call.function.value.*, writer);
            try writer.writeAll("(");
            for (call.arguments.items) |argument| {
                try print_expr(src, argument.value, writer);
                try writer.writeAll(", ");
            }
            try writer.writeAll(")");
        },
        .container => |container| try print_container(src, container.value, writer),
        .ident => |index| try writer.print("[{}]", .{index}),
        .block => |block| try print_block(src, block.value, writer),
        .number => |number| try writer.print("{}", .{number}),
        .type => |@"type"| try print_type(src, @"type", writer),
        .property => |property| {
            try print_expr(src, property.container.value.*, writer);
            try writer.writeAll(".");
            try writer.writeAll(property.property.range.substr(src));
        },
        .@"if" => |i| {
            try writer.writeAll("if ");
            try print_expr(src, i.condition.value.*, writer);
            try writer.writeAll(" then ");
            try print_expr(src, i.then.value.*, writer);
            if (i.@"else") |value| {
                try writer.writeAll(" else ");
                try print_expr(src, value.value.*, writer);
            }
        },
        .do_while => |do_while| {
            try writer.writeAll("do ");
            try print_expr(src, do_while.do.value.*, writer);
            try writer.writeAll(" while ");
            try print_expr(src, do_while.@"while".value.*, writer);
        },
        .while_do => |while_do| {
            try writer.writeAll("while ");
            try print_expr(src, while_do.@"while".value.*, writer);
            try writer.writeAll(" do ");
            try print_expr(src, while_do.do.value.*, writer);
        },
        .@"return" => |ret| {
            try writer.writeAll("return ");
            try print_expr(src, ret.value.*, writer);
        },
    }
}

fn print_type(src: []const u8, @"type": Type, writer: anytype) anyerror!void {
    switch (@"type") {
        .empty => try writer.writeAll("empty"),
        .unit => try writer.writeAll("unit"),
        .function => |function| {
            try writer.writeAll("fn (");
            for (function.parameters.items) |param| {
                try print_expr(src, param.value, writer);
                try writer.writeAll(", ");
            }
            try writer.writeAll(") ");
            try print_expr(src, function.@"return".value.*, writer);
        },
        .container => |container| try print_container(src, container, writer),
        .distinct => |distinct| {
            try writer.writeAll("distinct ");
            try print_expr(src, distinct.value.*, writer);
        },
        .integer => try writer.writeAll("integer"),
        .type => try writer.writeAll("type"),
    }
}

fn print_block(src: []const u8, block: Block, writer: anytype) anyerror!void {
    try writer.writeAll("{ ");

    for (block.stmts.items) |stmt| {
        try print_stmt(src, stmt.value, writer);
        try writer.writeAll(" ");
    }

    try writer.writeAll("}");
}

fn print_stmt(src: []const u8, stmt: Stmt, writer: anytype) anyerror!void {
    switch (stmt) {
        .decl => |decl| try print_decl(src, decl.value, writer),
        .expr => |expr| {
            try print_expr(src, expr.value, writer);
            try writer.writeByte(';');
        },
    }
}

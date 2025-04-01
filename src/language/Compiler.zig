const std = @import("std");
const Parser = @import("Parser.zig");
const tokenizer = @import("tokenizer.zig");
const Error = @import("failure.zig").Error;
const Ranged = tokenizer.Ranged;

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
    /// A function signature works like this: `fn(x: i32, y: i32) i32`.
    function: struct {
        parameters: std.ArrayList(Ranged(Expr)),
        @"return": Ranged(*Expr),
    },

    /// A container. See docs for `Container` for more specifics.
    container: Container,

    /// A unique wrapper around a type.
    ///
    /// Since all types are, by default, equal by structure (i.e. `struct { u32
    /// } == struct { u32 }`), a unique type simply removes equality by
    /// identity, so, for example, `unique struct { u32 } != unique struct { u32
    /// }`.
    ///
    /// This does not necessarily affect coercion (i.e. `struct { 5 }` being
    /// coercible to the type `unique struct { u32 }`).
    unique: Ranged(*Expr),

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

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .empty => try writer.writeAll("empty"),
            .unit => try writer.writeAll("unit"),
            .function => |function| {
                try writer.writeAll("fn (");
                for (function.parameters.items) |param| try writer.print("{}, ", .{param});
                try writer.print(") {}", .{function.@"return"});
            },
            .container => |container| try writer.print("{}", .{container}),
            .unique => |unique| try writer.print("unique {}", .{unique}),
            .integer => try writer.writeAll("integer"),
            .type => try writer.writeAll("type"),
        }
    }
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

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {{ ", .{@tagName(self.variant)});

        try writer.print("{}", .{self.fields});

        for (self.decls.items) |decl| try writer.print("{} ", .{decl});

        try writer.writeAll("}");
    }
};

pub const Fields = union(Parser.TaggedStatus) {
    untagged: std.ArrayList(Ranged(Expr)),
    tagged: std.ArrayList(Ranged(NamedExpr)),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .untagged => |fields| {
                for (fields.items) |field| {
                    try writer.print("{}, ", .{field});
                }
            },
            .tagged => |fields| {
                for (fields.items) |field| {
                    try writer.print("{}, ", .{field});
                }
            },
        }
    }
};

pub const NamedExpr = struct {
    name: tokenizer.Token,
    value: Ranged(Expr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}: {}", .{ self.name.value, self.value });
    }
};

pub const ContainerDecl = struct {
    access: Parser.Access,
    decl: Ranged(Decl),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {}", .{ @tagName(self.access), self.decl });
    }
};

pub const Decl = struct {
    mutability: Parser.Mutability,
    name: tokenizer.Token,
    value: Ranged(Expr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {s} = {};", .{ @tagName(self.mutability), self.name.value, self.value });
    }
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
        property: tokenizer.Token,
    },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .function => |function| {
                try writer.writeAll("fn (");
                for (function.parameters.items) |param| try writer.print("{}, ", .{param});
                try writer.print(") {} {}", .{ function.@"return", function.body });
            },
            .call => |call| {
                try writer.print("{}(", .{call.function});
                for (call.arguments.items) |argument| try writer.print("{}, ", .{argument});
                try writer.writeAll(")");
            },
            .container => |container| try writer.print("{}", .{container}),
            .ident => |index| try writer.print("[{}]", .{index}),
            .block => |block| try writer.print("{}", .{block}),
            .number => |number| try writer.print("{}", .{number}),
            .type => |@"type"| try writer.print("{}", .{@"type"}),
            .property => |property| try writer.print("{}.{s}", .{ property.container, property.property.value }),
        }
    }
};

pub const Block = struct {
    stmts: std.ArrayList(Ranged(Stmt)),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("{ ");

        for (self.stmts.items) |stmt| try writer.print("{} ", .{stmt});

        try writer.writeAll("}");
    }
};

pub const Stmt = union(enum) {
    decl: Ranged(Decl),
    @"return": Ranged(Expr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .decl => |decl| try writer.print("{}", .{decl}),
            .@"return" => |ret| try writer.print("return {};", .{ret}),
        }
    }
};

/// The error set of errors that can occur while compiling.
pub const CompilerError = error{
    CompilerError,
    OutOfMemory,
};

allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.CompilerError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?Error = null,

namespace: std.ArrayList(*Ranged(Decl)),
names: TokenHashMap(usize), // Maps to index in namespace

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
        .namespace = std.ArrayList(*Ranged(Decl)).init(allocator),
        .names = TokenHashMap(usize).init(allocator),
    };
}

/// Currently does not preserve order; waiting on
/// [#23400](https://github.com/ziglang/zig/issues/23400) and/or
/// [#23362](https://github.com/ziglang/zig/issues/23362) to become an
/// `ArrayHashMap` once again.
fn TokenHashMap(comptime V: type) type {
    const Context = struct {
        pub fn hash(_: @This(), token: tokenizer.Token) u64 {
            return std.hash_map.hashString(token.value);
        }
        pub fn eql(_: @This(), a: tokenizer.Token, b: tokenizer.Token) bool {
            return std.mem.eql(u8, a.value, b.value);
        }
    };

    return std.HashMap(tokenizer.Token, V, Context, std.hash_map.default_max_load_percentage);
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
            .unknown_identifier = ident,
        }) },
        .block => |block| .{ .block = try block.map(self, semantics_block) },
        .number => |number| .{ .number = number },
        .parentheses => |parens| try self.semantics_expr(parens.*.value),
        .unique => |unique| .{ .type = .{ .unique = try unique.map(self, semantics_expr_ptr) } },
        .property => |property| .{ .property = .{
            .container = try property.container.map(self, semantics_expr_ptr),
            .property = property.property,
        } },
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
                _, const ptr = try self.name_add(decl.name);
                ptr.* = stmt.swap(Decl{
                    .mutability = decl.mutability,
                    .name = decl.name,
                    .value = try decl.value.map(self, semantics_expr),
                });

                try stmts.append(stmt.swap(Stmt{ .decl = ptr.* }));
            },
            .@"return" => |ret| try stmts.append(stmt.swap(Stmt{
                .@"return" = try ret.map(self, semantics_expr),
            })),
        }
    }

    for (block.stmts.items) |stmt| if (stmt.value == .decl) {
        _ = self.names.remove(stmt.value.decl.name);
    };

    return .{ .stmts = stmts };
}

fn name_add(self: *@This(), name: tokenizer.Token) CompilerError!struct { usize, *Ranged(Decl) } {
    if (self.names.getEntry(name)) |entry| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = entry.key_ptr.*,
            .redeclared = name,
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
                    if (std.mem.eql(u8, field.value.name.value, tagged.name.value)) {
                        return self.fail(.{ .duplicate_tag = .{
                            field.value.name,
                            tagged.name,
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

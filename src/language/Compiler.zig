const std = @import("std");
const Parser = @import("Parser.zig");
const tokenizer = @import("tokenizer.zig");

/// The tag for types.
pub const TypeTag = enum {
    empty,
    unit,
    function,
    container,
    unique,
    type,
};

/// Possible fully-evaluated type expressions.
pub const Type = union(TypeTag) {
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
        parameters: std.ArrayList(TypedExpr),
        @"return": *TypedExpr,
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
    unique: *TypedExpr,

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
            .unique => |unique| try writer.print("unique {}", .{unique.*}),
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
    decls: std.ArrayList(ContainerDecl),

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
    untagged: std.ArrayList(TypedExpr),
    tagged: std.ArrayList(NamedExpr),

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
    value: TypedExpr,
    
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
    decl: Decl,

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
    value: Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {s} = {};", .{ @tagName(self.mutability), self.name.value, self.value });
    }
};

pub const TypedExpr = struct {
    type: ?Type,
    value: Expr,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({} :: {?})", .{ self.value, self.type });
    }
};

pub const ExprTag = enum {
    function,
    call,
    container,
    ident,
    block,
    number,
    type,
    property,
};

pub const Expr = union(ExprTag) {
    function: Function,
    call: Call,
    container: Container,
    ident: usize,
    block: Block,
    number: u128,
    type: Type,
    property: Property,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .function => |function| try writer.print("{}", .{function}),
            .call => |call| try writer.print("{}", .{call}),
            .container => |container| try writer.print("{}", .{container}),
            .ident => |index| try writer.print("[{}]", .{index}),
            .block => |block| try writer.print("{}", .{block}),
            .number => |number| try writer.print("{}", .{number}),
            .type => |@"type"| try writer.print("{}", .{@"type"}),
            .property => |property| try writer.print("{}", .{property}),
        }
    }
};

pub const Property = struct {
    container: *TypedExpr,
    property: tokenizer.Token,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{}.{s}", .{ self.container.*, self.property.value });
    }
};

pub const Function = struct {
    parameters: std.ArrayList(NamedExpr),
    @"return": *TypedExpr,
    body: Block,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("fn (");

        for (self.parameters.items) |param| try writer.print("{}, ", .{param});

        try writer.print(") {} {}", .{ self.@"return", self.body });
    }
};

pub const Call = struct {
    function: *TypedExpr,
    arguments: std.ArrayList(TypedExpr),

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{}(", .{self.function});

        for (self.arguments.items) |argument| try writer.print("{}, ", .{argument});

        try writer.writeAll(")");
    }
};

pub const Block = struct {
    stmts: std.ArrayList(Stmt),

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

pub const StmtTag = enum {
    decl,
    @"return",
};

pub const Stmt = union(StmtTag) {
    decl: Decl,
    @"return": TypedExpr,

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

/// Every possible error that can occur when compiling.
pub const Error = enum {
    redeclared_identifier,
    unknown_identifier,
    expected_homogenous_fields,
    duplicate_tag,
    expected_different_tagged_status,
};

/// The context for errors that occur while compiling.
pub const ErrorContext = union(Error) {
    redeclared_identifier: struct {
        declared: tokenizer.Token,
        redeclared: tokenizer.Token,
    },
    unknown_identifier: tokenizer.Token,
    expected_homogenous_fields: struct {
        untagged_example: Parser.Expr,
        tagged_example: Parser.NamedExpr,
    },
    duplicate_tag: struct { tokenizer.Token, tokenizer.Token },
    expected_different_tagged_status: struct {
        expected: Parser.TaggedStatus,
        counterexample: Parser.Field,
    },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .redeclared_identifier => |red| {
                // TODO: Point to parent as well (struct/container/function)
                try writer.print("Redeclared identifier {s}, defined at both {} and {}", .{ red.declared.value, red.declared.start, red.redeclared.start });
            },
            .unknown_identifier => |ident| try writer.print("Unknown identifier {s}", .{ident.value}),
            .expected_homogenous_fields => |fields| {
                // TODO: Pretty error message API
                // TODO: Point to parent as well (struct/container/function)
                _ = fields;
                try writer.writeAll("Expected entirely tagged or untagged values, but found a mix of both");
            },
            .duplicate_tag => |dup| {
                // TODO: Point to parent as well (struct/container/function)
                try writer.print("Duplicate tag {s}, defined at both {} and {}", .{ dup[0].value, dup[0].start, dup[1].start });
            },
            .expected_different_tagged_status => |status| {
                switch (status.expected) {
                    .untagged => try writer.writeAll("Expected untagged field but found tagged one(s)"),
                    .tagged => try writer.writeAll("Expected tagged fields but found untagged one(s)"),
                }
            },
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
error_context: ?ErrorContext = null,

namespace: std.ArrayList(*Decl),
names: TokenHashMap(usize), // Maps to index in namespace

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
        .namespace = std.ArrayList(*Decl).init(allocator),
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

fn fail(self: *@This(), @"error": ErrorContext) error{CompilerError} {
    self.error_context = @"error";
    return error.CompilerError;
}

pub fn compile(self: *@This(), container: Parser.Container) CompilerError!Container {
    return try self.semantics_container(container);
}

fn semantics_container(self: *@This(), container: Parser.Container) CompilerError!Container {
    var decls = std.ArrayList(ContainerDecl).init(self.allocator);
    for (container.decls.items) |decl| _ = try self.name_add(decl.decl.name);

    for (container.decls.items) |decl| {
        const value = ContainerDecl{
            .access = decl.access,
            .decl = .{
                .mutability = decl.decl.mutability,
                .name = decl.decl.name,
                .value = try self.semantics_expr(decl.decl.value),
            },
        };

        const index = self.names.get(decl.decl.name).?;
        self.namespace.items[index].* = value.decl;

        try decls.append(value);
    }

    const fields = try self.homogenize_fields(container.fields, null, .untagged);

    for (container.decls.items) |decl| _ = self.names.remove(decl.decl.name);

    return .{
        .variant = container.variant,
        .fields = fields,
        .decls = decls,
    };
}

fn semantics_expr(self: *@This(), expr: Parser.Expr) CompilerError!Expr {
    return switch (expr) {
        .function => |function| try self.semantics_function(function),
        .call => |call| .{ .call = try self.semantics_call(call) },
        .container => |container| .{ .container = try self.semantics_container(container) },
        .ident => |ident| .{ .ident = self.names.get(ident) orelse return self.fail(.{
            .unknown_identifier = ident,
        }) },
        .block => |block| .{ .block = try self.semantics_block(block) },
        .number => |number| .{ .number = number },
        .parentheses => |parens| try self.semantics_expr(parens.*),
        .unique => |unique| .{ .type = .{ .unique = try self.box(TypedExpr, try self.enforce_has_type(unique.value.*, .type)) } },
        .property => |property| .{ .property = .{
            .container = try self.box(TypedExpr, try self.enforce_has_type(property.container.*, null)),
            .property = property.property,
        } },
    };
}

fn semantics_call(self: *@This(), call: Parser.Call) CompilerError!Call {
    const ptr = try self.box(TypedExpr, try self.enforce_has_type(call.function.*, null));

    var args = std.ArrayList(TypedExpr).init(self.allocator);
    for (call.arguments.items) |arg| {
        try args.append(try self.enforce_has_type(arg, null));
    }

    return .{
        .function = ptr,
        .arguments = args,
    };
}

fn semantics_function(self: *@This(), function: Parser.Function) CompilerError!Expr {
    if (function.body) |body| { // This is a function declaration
        // 1. Dump names into namespace without evaluating
        for (function.parameters.items) |item| {
            if (item == .tagged) {
                // TODO: This induces name errors before correct tagged errors.
                _ = try self.name_add(item.tagged.name);
            }
        }

        // 2. Evaluate names and return value
        const params = try self.enforce_tagged_fields(function.parameters);
        const ret = try self.box(TypedExpr, try self.enforce_has_type(function.@"return".*, .type));

        // 3. Evaluate function body
        const block = try self.semantics_block(body);

        // 4. Clean up parameters in namespace
        for (params.items) |param| _ = self.names.remove(param.name);

        // 5. Return values
        return .{ .function = .{
            .parameters = params,
            .@"return" = ret,
            .body = block,
        } };
    } else { // This is a function type
        const fields = try self.enforce_untagged_fields(function.parameters);

        return .{ .type = .{ .function = .{
            .parameters = fields,
            .@"return" = try self.box(TypedExpr, try self.enforce_has_type(function.@"return".*, .type)),
        } } };
    }
}

fn semantics_block(self: *@This(), block: Parser.Block) CompilerError!Block {
    var stmts = std.ArrayList(Stmt).init(self.allocator);

    for (block.stmts.items) |stmt| {
        switch (stmt) {
            .decl => |decl| {
                _, const ptr = try self.name_add(decl.name);
                ptr.* = .{
                    .mutability = decl.mutability,
                    .name = decl.name,
                    .value = try self.semantics_expr(decl.value),
                };

                try stmts.append(.{ .decl = ptr.* });
            },
            .@"return" => |ret| try stmts.append(.{ .@"return" = try self.enforce_has_type(ret, null) }),
        }        
    }

    for (block.stmts.items) |stmt| if (stmt == .decl) {
        _ = self.names.remove(stmt.decl.name);
    };

    return .{ .stmts = stmts };
}

fn name_add(self: *@This(), name: tokenizer.Token) CompilerError!struct { usize, *Decl } {
    if (self.names.getEntry(name)) |entry| {
        return self.fail(.{ .redeclared_identifier = .{
            .declared = entry.key_ptr.*,
            .redeclared = name,
        } });
    } else {
        const ptr = try self.box(Decl, undefined);

        const index = self.namespace.items.len;
        try self.names.put(name, index);
        try self.namespace.append(ptr);

        return .{ index, ptr };
    }
}

fn box(self: *@This(), comptime T: type, value: T) CompilerError!*T {
    const ptr = try self.allocator.create(T);
    ptr.* = value;
    return ptr;
}

fn homogenize_fields(self: *@This(), fields: std.ArrayList(Parser.Field), expected: ?Parser.TaggedStatus, default: Parser.TaggedStatus) CompilerError!Fields {
    if (fields.items.len == 0) {
        return switch (expected orelse default) {
            .untagged => .{ .untagged = std.ArrayList(TypedExpr).init(self.allocator) },
            .tagged => .{ .tagged = std.ArrayList(NamedExpr).init(self.allocator) },
        };
    }

    return switch (expected orelse @as(Parser.TaggedStatus, fields.items[0])) {
        .untagged => .{ .untagged = try self.enforce_untagged_fields(fields) },
        .tagged => .{ .tagged = try self.enforce_tagged_fields(fields) },
    };
}

/// Expects at least one item in `fields` and that the first item is untagged.
fn enforce_untagged_fields(self: *@This(), fields: std.ArrayList(Parser.Field)) CompilerError!std.ArrayList(TypedExpr) {
    var list = std.ArrayList(TypedExpr).init(self.allocator);

    if (fields.items.len > 0 and fields.items[0] != .untagged) {
        return self.fail(.{ .expected_different_tagged_status = .{
            .expected = .untagged,
            .counterexample = fields.items[0],
        } });
    }

    for (fields.items) |item| {
        switch (item) {
            .untagged => |untagged| try list.append(try self.enforce_has_type(untagged, .type)),
            .tagged => |tagged| return self.fail(.{ .expected_homogenous_fields = .{
                .untagged_example = fields.items[0].untagged,
                .tagged_example = tagged,
            } }),
        }
    }

    return list;
}

/// Expects at least one item in `fields` and that the first item is tagged.
fn enforce_tagged_fields(self: *@This(), fields: std.ArrayList(Parser.Field)) CompilerError!std.ArrayList(NamedExpr) {
    var list = std.ArrayList(NamedExpr).init(self.allocator);

    if (fields.items.len > 0 and fields.items[0] != .tagged) {
        return self.fail(.{ .expected_different_tagged_status = .{
            .expected = .tagged,
            .counterexample = fields.items[0],
        } });
    }

    for (fields.items) |item| {
        switch (item) {
            .untagged => |untagged| return self.fail(.{ .expected_homogenous_fields = .{
                .untagged_example = untagged,
                .tagged_example = fields.items[0].tagged,
            } }),
            .tagged => |tagged| {
                for (list.items) |field| {
                    if (std.mem.eql(u8, field.name.value, tagged.name.value)) {
                        return self.fail(.{ .duplicate_tag = .{
                            field.name,
                            tagged.name,
                        } });
                    }
                }

                try list.append(.{
                    .name = tagged.name,
                    .value = try self.enforce_has_type(tagged.value, .type),
                });
            },
        }
    }

    return list;
}

fn enforce_has_type(self: *@This(), expr: Parser.Expr, @"type": ?Type) CompilerError!TypedExpr {
    return .{
        .type = @"type",
        .value = try self.semantics_expr(expr),
    };
}

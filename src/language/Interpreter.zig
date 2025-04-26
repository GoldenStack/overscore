const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Ranged = tokenizer.Ranged;

const Ir = @import("Ir.zig");
const ir = Ir.ir;
const Index = Ir.Index;
const failure = @import("failure.zig");

/// The error set of errors that can occur while interpreting.
pub const Error = error{
    InterpreterError,
    OutOfMemory,
};

src: [:0]const u8,
allocator: std.mem.Allocator,

/// The context for whatever error may have occurred. If any functions on this
/// type return error.InterpreterError, this value is significant. Otherwise, it
/// may contain anything.
error_context: ?failure.Error = null,

context: Ir,

pub fn init(allocator: std.mem.Allocator, src: [:0]const u8, context: Ir) @This() {
    return .{
        .src = src,
        .allocator = allocator,
        .context = context,
    };
}

pub fn evalMain(self: *@This(), index: Index(ir.Container)) Error!Index(ir.Expr) {
    const container = self.context.indexContainer(index);
    const main = container.defs.get("main") orelse @panic("No main found!");

    const mainIndex = main.value.def;

    try self.evalDef(mainIndex);

    return self.context.indexDef(mainIndex).value.value;
}

fn evalContainer(self: *@This(), index: Index(ir.Container)) Error!void {
    const container = self.context.indexContainer(index);

    for (container.defs.values()) |def| try self.evalDef(def.value.def);
}

fn evalInterface(self: *@This(), interface: *ir.Interface) Error!void {
    for (interface.decls.values()) |*def| {
        try self.evalExpr(def.value.type.value);
    }
}

fn evalDef(self: *@This(), index: Index(ir.Def)) Error!void {
    var def = self.context.indexDef(index);

    def.evaluating = true;
    defer def.evaluating = false;

    if (def.type) |@"type"| {
        try self.evalExpr(@"type".value);
        try self.evalExpr(def.value.value);

        try self.expectTypeExpression(@"type");
        try self.expectType(def.value, self.context.indexExpr(@"type".value).type, @"type".range);
    } else {
        try self.evalExpr(def.value.value);
    }
}

fn expectTypeExpression(self: *@This(), index: Ranged(Index(ir.Expr))) Error!void {
    const expr = self.context.indexExpr(index.value);

    if (!try self.typeContainsValue(expr.*, .type)) return self.fail(.{ .expected_type_expression = .{
        .found_type = self.typeToString(try self.typeOf(index.value)),
        .has_wrong_type = index.range,
    } });
}

fn expectType(self: *@This(), index: Ranged(Index(ir.Expr)), @"type": ir.Type, cause: tokenizer.Range) Error!void {
    const expr = self.context.indexExpr(index.value);

    if (!try self.typeContainsValue(expr.*, @"type")) return self.fail(.{ .mismatched_type = .{
        .expected_type = self.typeToString(@"type"),
        .found_type = self.typeToString(try self.typeOf(index.value)),
        .expected_type_declared = cause,
        .has_wrong_type = index.range,
    } });
}

fn typeContainsValue(self: *@This(), expr: ir.Expr, @"type": ir.Type) Error!bool {
    return switch (@"type") {
        .word => expr == .word,
        .type => expr == .type,
        .interface => {
            if (expr != .container) return false;

            // TODO: This copies the interface and will thus recalculate often.
            var interface = @"type".interface;

            try self.evalInterface(&interface);

            // TODO: This makes container evaluation mandatory pretty much everywhere. Is this actually necessary?
            try self.evalContainer(expr.container);

            const container = self.context.indexContainer(expr.container);

            if (interface.decls.keys().len != container.defs.keys().len) return false;

            var iter = interface.decls.iterator();
            while (iter.next()) |decl| {
                if (container.defs.get(decl.key_ptr.*)) |def| {
                    const def_value = self.context.indexDef(def.value.def);

                    const value = self.context.indexExpr(def_value.value.value).*;
                    const expected_type = self.context.indexExpr(decl.value_ptr.value.type.value).type; // We assume it is a type because it has been evaluated previously

                    if (!try self.typeContainsValue(value, expected_type)) {
                        return false;
                    }
                } else return false;
            }

            return true;
        },
        .pointer => |ptr| {
            // An expression `e` has type `*T` if `e` is a pointer and `e.value` has type `T`.
            if (expr != .pointer) return false;

            // Pre-evaluated pointer types must be type expressions
            const value = self.context.indexDef(expr.pointer.value).value.value;
            const type2 = self.context.indexExpr(ptr.value).type;

            return self.typeContainsValue(self.context.indexExpr(value).*, type2);
        },
    };
}

/// Assumes that the provided expression has been fully evaluated. This means
/// the provided expression must be a *minimal* value, i.e. `.type` or `.word`.
///
/// Call `evalExpr` to make sure.
pub fn typeOf(self: *@This(), index: Index(ir.Expr)) Error!ir.Type {
    const expr = self.context.indexExpr(index);

    return switch (expr.*) {
        .type => .type,
        .word => .word,
        .container => |container_index| {
            const container = self.context.indexContainer(container_index);
            var decls = std.StringArrayHashMap(Ranged(ir.Decl)).init(self.allocator);

            var iter = container.defs.iterator();
            while (iter.next()) |def| {
                const def_value = self.context.indexDef(def.value_ptr.value.def);
                const def_type = def_type: {
                    if (def_value.type) |@"type"| {
                        break :def_type @"type";

                    } else {
                        // TODO: This range doesn't really make sense
                        const type_index = try self.context.pushExpr(.{ .type = try self.typeOf(def_value.value.value) }); 

                        break :def_type def_value.value.swap(type_index);
                    }
                };

                const custom_decl: ir.Decl = .{
                    .name = def_value.name,
                    .type = def_type,
                };

                try decls.putNoClobber(def.key_ptr.*, def.value_ptr.swap(custom_decl)); // TODO: Range doesn't make sense
            }

            return .{ .interface = .{ .decls = decls } };
        },
        .pointer => |ptr| {
            const ptr_value = self.context.indexDef(ptr.value).value.value;
            const ptr_type = try self.typeOf(ptr_value);
            
            const type_index = try self.context.pushExpr(.{ .type = ptr_type });

            return .{ .pointer = ptr.swap(type_index) };
        },
        else => |value| std.debug.panic("Expected fully evaluated expression, but found one of type {s}", .{@tagName(value)}),
    };
}

/// Evaluates an expression until it is a raw value that cannot be decomposed
/// any further.
fn evalExpr(self: *@This(), index: Index(ir.Expr)) Error!void {
    const expr = self.context.indexExpr(index);

    switch (expr.*) {
        .type => try self.evalType(index),
        .word => {}, // Already minimal
        .container => {}, // Already minimal
        .pointer => |ptr| {
            const def = self.context.indexDef(ptr.value);

            if (def.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = def.name.range,
                .depends = ptr.range,
            } });

            try self.evalDef(ptr.value);
        },
        .dereference => |deref| {
            try self.evalExpr(deref.value);
            const left = self.context.indexExpr(deref.value);

            if (left.* != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
                .expr = deref.range,
                .type = self.typeToString(try self.typeOf(deref.value)),
            } });

            const def_value = self.context.indexDef(left.pointer.value).value.value;

            // TODO: Shallow copy on dereference
            expr.* = self.context.indexExpr(def_value).*; // Clone it (suboptimal)
        },
        .parentheses => |parens| {
            expr.* = self.context.indexExpr(parens.value).*; // Clone it (suboptimal)
            try self.evalExpr(index);
        },
        .member_access => |access| {
            try self.evalExpr(access.container.value);

            try self.memberAccessGeneric(index, access.container, access.member);
        },
    }
}

fn evalType(self: *@This(), index: Index(ir.Expr)) Error!void {
    const expr = self.context.indexExpr(index);

    switch (expr.type) {
        .type => {}, // Already minimal
        .word => {}, // Already minimal
        .interface => {
            // TODO: Must fields be evaluated?
        },
        .pointer => |ptr| {
            try self.evalExpr(ptr.value);
            try self.expectTypeExpression(ptr);
        },
    }
}

/// Member access works in several different ways, primarily static and dynamic
/// field access. This function figures it out.
fn memberAccessGeneric(self: *@This(), expr: Index(ir.Expr), container: Ranged(Index(ir.Expr)), member: Ranged(Token)) Error!void {
    const container_expr = self.context.indexExpr(container.value);

    if (container_expr.* == .pointer) {
        const ptr_range = self.context.indexDef(container_expr.pointer.value);
        const ptr_value = self.context.indexExpr(ptr_range.value.value);

        if (ptr_value.* == .container) {
            // Eval the decl and then evaluate to a pointer to the decl
            const def = try self.staticMemberAccess(container.swap(ptr_range.value.value), member);
            try self.evalDef(def);
            
            self.context.indexExpr(expr).* = .{ .pointer = container.swap(def) };
            return;
        }
    }

    if (container_expr.* == .container) {
        const def = try self.staticMemberAccess(container, member);
        try self.evalDef(def);

        self.context.indexExpr(expr).* = self.context.indexExpr(self.context.indexDef(def).value.value).*; // Clone it (suboptimal)
        return;
    }

    return self.fail(.{ .unsupported_member_access = .{
        .type = self.typeToString(try self.typeOf(container.value)),
        .member = member.range,
    } });
}

/// Access definitions on a container. This assumes that the provided expression
/// is a container type.
fn staticMemberAccess(self: *@This(), container: Ranged(Index(ir.Expr)), member: Ranged(Token)) Error!Index(ir.Def) {
    const container_expr = self.context.indexExpr(container.value);
    const defs = self.context.indexContainer(container_expr.container).defs;

    const key = member.range.substr(self.src);

    const container_def = defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = container.range,
        .member = member.range,
    } });

    const def = self.context.indexDef(container_def.value.def);

    if (container_def.value.access == .private) return self.fail(.{ .private_member = .{
        .declaration = def.name.range,
        .member = member.range,
    } });

    return container_def.value.def;
}

fn typeToString(self: *@This(), @"type": ir.Type) []u8 {
    var out = std.ArrayListUnmanaged(u8){};

    // TODO: Make this more robust
    self.context.printType(@"type", out.writer(self.allocator)) catch |err| std.debug.panic("could not print type: {any}", .{err});

    return out.items;
}

/// Fails, storing the given error context and returning an error.
fn fail(self: *@This(), @"error": failure.Error) error{InterpreterError} {
    self.error_context = @"error";
    return error.InterpreterError;
}

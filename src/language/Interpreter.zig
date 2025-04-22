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

pub fn evalMain(self: *@This(), index: Index(ir.Container)) Error!ir.Expr {
    const container = self.context.containers.items[index];
    const main = container.defs.get("main") orelse @panic("No main found!");

    const mainIndex = main.value.def;

    try self.evalDef(mainIndex);

    return self.context.defs.items[mainIndex].value.value;
}

fn evalContainer(self: *@This(), index: Index(ir.Container)) Error!void {
    var container = &self.context.containers.items[index];

    for (container.defs.values()) |def| try self.evalDef(def.value.def);
}

fn evalInterface(self: *@This(), interface: *ir.Interface) Error!void {
    for (interface.decls.values()) |*def| {
        def.value.type.value = try self.evalExpr(def.value.type.value);
    }
}

fn evalDef(self: *@This(), index: Index(ir.Def)) Error!void {
    var def = &self.context.defs.items[index];

    def.evaluating = true;
    defer def.evaluating = false;

    if (def.type) |*@"type"| {
        // Evaluate the type
        @"type".value = try self.evalExpr(@"type".value);

        const expr = &def.value.value;
        expr.* = try self.evalExpr(expr.*);

        try self.expectTypeExpression(@"type".*);
        try self.expectType(def.value, @"type".value.type, @"type".range);
    } else {
        const expr = &def.value.value;
        expr.* = try self.evalExpr(expr.*);
    }
}

fn expectTypeExpression(self: *@This(), expr: Ranged(ir.Expr)) Error!void {
    if (!try self.typeContainsValue(expr.value, .type)) return self.fail(.{ .expected_type_expression = .{
        .found_type = self.typeToString(try self.typeOf(expr.value)),
        .has_wrong_type = expr.range,
    } });
}

fn expectType(self: *@This(), expr: Ranged(ir.Expr), @"type": ir.Type, cause: tokenizer.Range) Error!void {
    if (!try self.typeContainsValue(expr.value, @"type")) return self.fail(.{ .mismatched_type = .{
        .expected_type = self.typeToString(@"type"),
        .found_type = self.typeToString(try self.typeOf(expr.value)),
        .expected_type_declared = cause,
        .has_wrong_type = expr.range,
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

            const container = self.context.containers.items[expr.container];

            if (interface.decls.keys().len != container.defs.keys().len) return false;

            var iter = interface.decls.iterator();
            while (iter.next()) |decl| {
                if (container.defs.get(decl.key_ptr.*)) |def| {
                    const def_value = self.context.defs.items[def.value.def];

                    if (!try self.typeContainsValue(def_value.value.value, decl.value_ptr.value.type.value.type)) {
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
            const value = self.context.defs.items[expr.pointer.value].value.value;
            const type2 = ptr.value.type;

            return self.typeContainsValue(value, type2);
        },
    };
}

/// Assumes that the provided expression has been fully evaluated. This means
/// the provided expression must be a *minimal* value, i.e. `.type` or `.word`.
///
/// Call `evalExpr` to make sure.
pub fn typeOf(self: *@This(), expr: ir.Expr) Error!ir.Type {
    return switch (expr) {
        .type => .type,
        .word => .word,
        .container => |container_index| {
            const container = self.context.containers.items[container_index];
            var decls = std.StringArrayHashMap(Ranged(ir.Decl)).init(self.allocator);

            var iter = container.defs.iterator();
            while (iter.next()) |def| {
                const def_value = self.context.defs.items[def.value_ptr.value.def];
                const def_type = if (def_value.type) |@"type"| @"type"
                    else def_value.value.swap(ir.Expr{ .type = try self.typeOf(def_value.value.value) }); // TODO: Range doesn't make sense

                const custom_decl: ir.Decl = .{
                    .name = def_value.name,
                    .type = def_type,
                };

                try decls.putNoClobber(def.key_ptr.*, def.value_ptr.swap(custom_decl)); // TODO: Range doesn't make sense
            }

            return .{ .interface = .{ .decls = decls } };
        },
        .pointer => |ptr| {
            const ptr_value = self.context.defs.items[ptr.value].value.value;
            const ptr_type = try self.typeOf(ptr_value);

            const type_ptr = try self.allocator.create(ir.Expr);
            type_ptr.* = .{ .type = ptr_type };
            return .{ .pointer = ptr.swap(type_ptr) };
        },
        else => |value| std.debug.panic("Expected fully evaluated expression, but found one of type {s}", .{@tagName(value)}),
    };
}

fn evalExprPtr(self: *@This(), expr: *ir.Expr) Error!*ir.Expr {
    const ptr = try self.allocator.create(ir.Expr);
    ptr.* = try self.evalExpr(expr.*);
    return ptr;
}

/// Evaluates an expression until it is a raw value that cannot be decomposed
/// any further.
fn evalExpr(self: *@This(), expr: ir.Expr) Error!ir.Expr {
    return switch (expr) {
        .type => |@"type"| .{ .type = try self.evalType(@"type") },
        .word => |word| .{ .word = word },
        .container => |container| .{ .container = container },
        .pointer => |ptr| {
            const def = &self.context.defs.items[ptr.value];

            if (def.evaluating) return self.fail(.{ .dependency_loop = .{
                .declared = def.name.range,
                .depends = ptr.range,
            } });

            try self.evalDef(ptr.value);

            return expr;
        },
        .dereference => |deref| {
            const deref2 = try self.evalExpr(deref.value.*);
            if (deref2 != .pointer) return self.fail(.{ .dereferenced_non_pointer = .{
                .expr = deref.range,
                .@"type" = self.typeToString(try self.typeOf(deref2)),
            } });

            const index = deref2.pointer.value;
            const def = self.context.defs.items[index];

            // Can just return the direct value because evaluating an expression copies it
            // TODO: This doesn't work for container instances.
            return def.value.value;
        },
        .parentheses => |parens| try self.evalExpr(parens.value.*),
        .member_access => |access| {
            const container = access.container.swap(try self.evalExpr(access.container.value.*));
            return self.memberAccessGeneric(container, access.member);
        },
    };
}

fn evalType(self: *@This(), @"type": ir.Type) Error!ir.Type {
    return switch (@"type") {
        .type => .type,
        .word => .word,
        .interface => |interface| .{ .interface = interface }, // TODO: Do fields need to be evaluated?
        .pointer => |ptr| {
            const value = try ptr.map(self, evalExprPtr);
            try self.expectTypeExpression(value.swap(value.value.*));
            return .{ .pointer = value };
        },
    };
}

/// Member access works in several different ways, primarily static and dynamic
/// field access. This function figures it out.
fn memberAccessGeneric(self: *@This(), expr: Ranged(ir.Expr), member: Ranged(Token)) Error!ir.Expr {
    if (expr.value == .pointer) {
        const ptr_range = self.context.defs.items[expr.value.pointer.value].value;
        const ptr_value = ptr_range.value;

        if (ptr_value == .container) {
            const def = try self.staticMemberAccess(ptr_range, member);
            try self.evalDef(def);
            return .{ .pointer = expr.value.pointer.swap(def) };
        }
    }

    if (expr.value == .container) {
        const def = try self.staticMemberAccess(expr, member);
        try self.evalDef(def);
        return self.context.defs.items[def].value.value;
    }

    return self.fail(.{ .unsupported_member_access = .{
        .type = self.typeToString(try self.typeOf(expr.value)),
        .member = member.range,
    } });
}

/// Access definitions on a container. This assumes that the provided expression
/// is a container type.
fn staticMemberAccess(self: *@This(), container: Ranged(ir.Expr), member: Ranged(Token)) Error!Index(ir.Def) {
    const container_index = container.value.container;
    const defs = self.context.containers.items[container_index].defs;

    const key = member.range.substr(self.src);

    const container_def = defs.get(key) orelse return self.fail(.{ .unknown_member = .{
        .container = container.range,
        .member = member.range,
    } });

    const def = self.context.defs.items[container_def.value.def];

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

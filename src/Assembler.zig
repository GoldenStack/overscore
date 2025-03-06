const std = @import("std");
const Cpu = @import("Cpu.zig");

pub const LineBreak = '\n';

pub const Comment = "//";

pub const Whitespace = " \t";

pub const AddrTag = enum { literal, label };

pub const Addr = union(AddrTag) {
    const Literal = struct { base: u8, value: Cpu.Word };

    literal: Literal,
    label: []const u8,

    pub fn read(reader: anytype) !Addr {
        const token = try reader.readToken();

        const literal: ?Literal = switch (token[0]) {
            'x' => Literal{
                .base = 16,
                .value = try std.fmt.parseUnsigned(Cpu.Word, token[1..], 16),
            },
            'd' => Literal{
                .base = 10,
                .value = try std.fmt.parseUnsigned(Cpu.Word, token[1..], 10),
            },
            'b' => Literal{
                .base = 2,
                .value = try std.fmt.parseUnsigned(Cpu.Word, token[1..], 2),
            },
            else => otherwise: {
                const result = std.fmt.parseUnsigned(Cpu.Word, token, 16);

                break :otherwise if (result) |value| Literal{
                    .base = 16,
                    .value = value,
                } else |_| null;
            },
        };

        return if (literal) |lit| .{ .literal = lit } else .{
            .label = token,
        };
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Word)) !void {
        try switch (self.*) {
            .literal => |literal| writer.writeInt(Cpu.Word, literal.value, .little),
            .label => |label| writer.writeInt(Cpu.Word, labels.get(label) orelse return error.UnknownLabel, .little),
        };
    }
};

pub const UnaryOp = struct {
    left: Addr,
    right: Addr,

    pub fn read(reader: anytype) !UnaryOp {
        return .{
            .left = try Addr.read(reader),
            .right = try Addr.read(reader),
        };
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        try self.left.write(writer, labels);
        try self.right.write(writer, labels);
    }
};

pub const LineTag = enum {
    set,
    mov,
    not,
    @"and",
    add,
    irm,
    iwm,
    sys,

    raw,
    label,
    bytes,
    end,

    pub fn read(reader: anytype) !LineTag {
        const token = try reader.readToken();

        return std.meta.stringToEnum(LineTag, token) orelse error.UnknownLineType;
    }
};

pub const Line = union(LineTag) {
    set: UnaryOp,
    mov: UnaryOp,
    not: UnaryOp,
    @"and": UnaryOp,
    add: UnaryOp,
    irm: UnaryOp,
    iwm: UnaryOp,
    sys: UnaryOp,

    raw: Addr,
    label: []const u8,
    bytes: []const u8,
    end,

    pub fn read(reader: anytype) !Line {
        const tag = try LineTag.read(reader);

        return switch (tag) {
            .set => .{ .set = try UnaryOp.read(reader) },
            .mov => .{ .mov = try UnaryOp.read(reader) },
            .not => .{ .not = try UnaryOp.read(reader) },
            .@"and" => .{ .@"and" = try UnaryOp.read(reader) },
            .add => .{ .add = try UnaryOp.read(reader) },
            .irm => .{ .irm = try UnaryOp.read(reader) },
            .iwm => .{ .iwm = try UnaryOp.read(reader) },
            .sys => .{ .sys = try UnaryOp.read(reader) },

            .raw => .{ .raw = try Addr.read(reader) },
            .label => .{ .label = try reader.readToken() },
            .bytes => .{ .bytes = try reader.readRemaining() },
            .end => .end,
        };
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        switch (self.*) {
            .set => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.set));
                try line.write(writer, labels);
            },
            .mov => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.mov));
                try line.write(writer, labels);
            },
            .not => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.not));
                try line.write(writer, labels);
            },
            .@"and" => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.@"and"));
                try line.write(writer, labels);
            },
            .add => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.add));
                try line.write(writer, labels);
            },
            .irm => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.irm));
                try line.write(writer, labels);
            },
            .iwm => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.iwm));
                try line.write(writer, labels);
            },
            .sys => |line| {
                try writer.writeByte(@intFromEnum(Cpu.InstructionTag.sys));
                try line.write(writer, labels);
            },

            .raw => |line| try line.write(writer, labels),
            .label => {},
            .bytes => |line| try writer.writeAll(line),
            .end => try writer.writeByte(0),
        }
    }

    pub fn size(self: *const @This()) Cpu.Word {
        return switch (self.*) {
            .set, .mov, .not, .@"and", .add, .irm, .iwm, .sys => 9,
            .raw => 4,
            .label => 0,
            .bytes => |line| @truncate(line.len),
            .end => 1,
        };
    }
};

/// Assembles a string of Overscore assembly language into a binary file for
/// input into the CPU.
pub fn assemble(buffer: []const u8, allocator: std.mem.Allocator, writer: anytype) !void {
    var iter = AssemblyIterator.init(buffer);

    var lines = std.ArrayList(Line).init(allocator);
    var labels = std.StringHashMap(Cpu.Addr).init(allocator);
    defer lines.deinit();
    defer labels.deinit();

    try parseLines(&iter, &lines, &labels);

    for (lines.items) |line| try line.write(writer, &labels);
}

fn parseLines(reader: anytype, lines: *std.ArrayList(Line), labels: *std.StringHashMap(Cpu.Addr)) !void {
    var total_size: Cpu.Word = 0;

    while (reader.nextLine()) {
        const line = try Line.read(reader);

        if ((try reader.readRemaining()).len != 0) return error.ExpectedNewline;

        switch (line) {
            .label => |label| try labels.put(label, total_size),
            else => {},
        }
        total_size += line.size();

        try lines.append(line);
    }
}

const AssemblyIterator = struct {
    line_iterator: std.mem.TokenIterator(u8, .scalar),
    token_iterator: ?std.mem.TokenIterator(u8, .any),

    fn init(buffer: []const u8) @This() {
        return .{
            .line_iterator = std.mem.tokenizeScalar(u8, buffer, LineBreak),
            .token_iterator = null,
        };
    }

    fn removeComments(line: []const u8) []const u8 {
        return if (std.mem.indexOf(u8, line, Comment)) |comment|
            line[0..comment]
        else
            line;
    }

    fn nextUnprocessed(self: *@This()) ?[]const u8 {
        const line = self.line_iterator.next() orelse return null;

        const uncommented_line = removeComments(line);

        const trimmed_line = std.mem.trim(u8, uncommented_line, Whitespace);

        return trimmed_line;
    }

    pub fn nextLine(self: *@This()) bool {
        while (self.nextUnprocessed()) |line| {
            if (line.len == 0) continue;

            self.token_iterator = std.mem.tokenizeAny(u8, line, Whitespace);
            return true;
        } else return false;
    }

    pub fn readToken(self: *@This()) ![]const u8 {
        const token = if (self.token_iterator) |*tokens| tokens.next() else null;

        return token orelse error.ExpectedToken;
    }

    fn readRemaining(self: *@This()) ![]const u8 {
        const tokens = &self.token_iterator.?;
        const slice = tokens.rest();
        tokens.index = tokens.buffer.len;

        return slice;
    }
};

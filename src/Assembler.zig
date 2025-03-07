const std = @import("std");
const Cpu = @import("Cpu.zig");

/// The line break character.
pub const LineBreak = '\n';

/// The comment string, commenting text until the end of the line.
pub const Comment = "//";

/// The tag for addresses in assembly.
pub const AddrTag = enum { literal, label };

/// The type for an address literal.
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

/// A binary operation, with a left and right address.
pub const BinOp = struct {
    left: Addr,
    right: Addr,

    pub fn read(reader: anytype) !BinOp {
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

/// The tag for the line type.
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

/// A line in assembly. This can be an instruction or one of many Assembler
/// constructs.
pub const Line = union(LineTag) {
    set: BinOp,
    mov: BinOp,
    not: BinOp,
    @"and": BinOp,
    add: BinOp,
    irm: BinOp,
    iwm: BinOp,
    sys: BinOp,

    raw: Addr,
    label: []const u8,
    bytes: []const u8,
    end,

    pub fn read(reader: anytype) !Line {
        const tag = try LineTag.read(reader);

        return switch (tag) {
            .set => .{ .set = try BinOp.read(reader) },
            .mov => .{ .mov = try BinOp.read(reader) },
            .not => .{ .not = try BinOp.read(reader) },
            .@"and" => .{ .@"and" = try BinOp.read(reader) },
            .add => .{ .add = try BinOp.read(reader) },
            .irm => .{ .irm = try BinOp.read(reader) },
            .iwm => .{ .iwm = try BinOp.read(reader) },
            .sys => .{ .sys = try BinOp.read(reader) },

            .raw => .{ .raw = try Addr.read(reader) },
            .label => .{ .label = try reader.readToken() },
            .bytes => .{ .bytes = try reader.readRemaining() },
            .end => .end,
        };
    }

    fn write_instruction(comptime variant: Cpu.InstructionTag, value: BinOp, writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        const opcode = @intFromEnum(variant);
        try writer.writeByte(opcode);

        try value.write(writer, labels);
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        try switch (self.*) {
            .set => |line| write_instruction(.set, line, writer, labels),
            .mov => |line| write_instruction(.mov, line, writer, labels),
            .not => |line| write_instruction(.not, line, writer, labels),
            .@"and" => |line| write_instruction(.@"and", line, writer, labels),
            .add => |line| write_instruction(.add, line, writer, labels),
            .irm => |line| write_instruction(.irm, line, writer, labels),
            .iwm => |line| write_instruction(.iwm, line, writer, labels),
            .sys => |line| write_instruction(.sys, line, writer, labels),

            .raw => |line| line.write(writer, labels),
            .label => {},
            .bytes => |line| writer.writeAll(line),
            .end => writer.writeByte(0),
        };
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

/// Parses lines from the given reader, writing the lines to the lines parameter
/// and writing any parsed labels to the labels map.
fn parseLines(reader: anytype, lines: *std.ArrayList(Line), labels: *std.StringHashMap(Cpu.Addr)) !void {
    var total_size: Cpu.Word = 0;

    while (reader.nextLine()) {
        const line = try Line.read(reader);

        // Make sure exactly one instruction per line
        if ((try reader.readRemaining()).len != 0) return error.ExpectedNewline;

        // Write a label if possible
        switch (line) {
            .label => |label| try labels.put(label, total_size),
            else => {},
        }

        total_size += line.size();

        try lines.append(line);
    }
}

/// Iterates over assembly code, one line at a time.
/// 
/// This allows iterating over whitespace-separated tokens via `nextToken` until
/// a newline, at which point you have to call `nextLine` to continue to the
/// next line.
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

        const trimmed_line = std.mem.trim(u8, uncommented_line, std.ascii.whitespace);

        return trimmed_line;
    }

    /// Tries to move to the next line, returning whether or not it succeeded.
    pub fn nextLine(self: *@This()) bool {
        while (self.nextUnprocessed()) |line| {
            if (line.len == 0) continue;

            self.token_iterator = std.mem.tokenizeAny(u8, line, std.ascii.whitespace);
            return true;
        } else return false;
    }

    /// Reads the next token from this iterator.
    pub fn readToken(self: *@This()) ![]const u8 {
        const token = if (self.token_iterator) |*tokens| tokens.next() else null;

        return token orelse error.ExpectedToken;
    }

    /// Reads all of the remaining tokens (ignoring whitespace) from this
    /// iterator until a newline. You will have to call `nextLine` after this.
    pub fn readRemaining(self: *@This()) ![]const u8 {
        const tokens = &self.token_iterator.?;
        const slice = tokens.rest();
        tokens.index = tokens.buffer.len;

        return slice;
    }
};

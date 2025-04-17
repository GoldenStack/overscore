const std = @import("std");
const Cpu = @import("Cpu.zig");

/// The line break character.
pub const LineBreak = '\n';

/// The comment string, commenting text until the end of the line.
pub const Comment = "//";

/// The type for an address literal.
pub const Addr = union(enum) {
    literal: Cpu.Word,
    label: []const u8,

    pub fn read(reader: anytype) !Addr {
        const token = try reader.readToken();

        const literal: ?Cpu.Word = switch (token[0]) {
            'x' => try std.fmt.parseUnsigned(Cpu.Word, token[1..], 16),
            'd' => try std.fmt.parseUnsigned(Cpu.Word, token[1..], 10),
            'b' => try std.fmt.parseUnsigned(Cpu.Word, token[1..], 2),
            else => std.fmt.parseUnsigned(Cpu.Word, token, 16) catch null,
        };

        return if (literal) |lit| .{ .literal = lit } else .{
            .label = token,
        };
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Word)) !void {
        try switch (self.*) {
            .literal => |value| writer.writeInt(Cpu.Word, value, .little),
            .label => |label| writer.writeInt(Cpu.Word, labels.get(label) orelse return error.UnknownLabel, .little),
        };
    }
};

/// A unary operation, with only one address.
pub const UnaryOp = struct {
    left: Addr,

    pub fn read(reader: anytype) !UnaryOp {
        return .{
            .left = try Addr.read(reader),
        };
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        try self.left.write(writer, labels);
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
    unary,
    binary,

    raw,
    label,
    bytes,
    end,
};

/// A line in assembly. This can be an instruction or one of many Assembler
/// constructs.
pub const Line = union(LineTag) {
    unary: struct { Cpu.UnaryInstruction.Opcode, UnaryOp },
    binary: struct { Cpu.BinaryInstruction.Opcode, BinOp },

    raw: Addr,
    label: []const u8,
    bytes: []const u8,
    end,

    pub fn read(reader: anytype) !Line {
        const token = try reader.readToken();

        if (std.meta.stringToEnum(Cpu.UnaryInstruction.Opcode, token)) |op| {
            return .{ .unary = .{ op, try UnaryOp.read(reader) } };
        } else if (std.meta.stringToEnum(Cpu.BinaryInstruction.Opcode, token)) |op| {
            return .{ .binary = .{ op, try BinOp.read(reader) } };
        } else if (std.mem.eql(u8, "raw", token)) {
            return .{ .raw = try Addr.read(reader) };
        } else if (std.mem.eql(u8, "label", token)) {
            return .{ .label = try reader.readToken() };
        } else if (std.mem.eql(u8, "bytes", token)) {
            return .{ .bytes = try reader.readRemaining() };
        } else if (std.mem.eql(u8, "end", token)) {
            return .end;
        } else return error.UnknownLineType;
    }

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        try switch (self.*) {
            .unary => |unary| {
                const opcode = @intFromEnum(unary.@"0");
                try writer.writeByte(opcode);

                try unary.@"1".write(writer, labels);
            },
            .binary => |binary| {
                const opcode = @intFromEnum(binary.@"0") | 0b10000000;
                try writer.writeByte(opcode);

                try binary.@"1".write(writer, labels);
            },

            .raw => |line| line.write(writer, labels),
            .label => {},
            .bytes => |line| writer.writeAll(line),
            .end => writer.writeByte(0xff),
        };
    }

    pub fn size(self: *const @This()) Cpu.Word {
        return switch (self.*) {
            .unary => 5,
            .binary => 9,
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

        const trimmed_line = std.mem.trim(u8, uncommented_line, &std.ascii.whitespace);

        return trimmed_line;
    }

    /// Tries to move to the next line, returning whether or not it succeeded.
    pub fn nextLine(self: *@This()) bool {
        while (self.nextUnprocessed()) |line| {
            if (line.len == 0) continue;

            self.token_iterator = std.mem.tokenizeAny(u8, line, &std.ascii.whitespace);
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

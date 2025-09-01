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

    pub fn write(self: *const @This(), writer: anytype, labels: *const std.StringHashMap(Cpu.Addr)) !void {
        try self.left.write(writer, labels);
    }
};

/// A binary operation, with a left and right address.
pub const BinOp = struct {
    left: Addr,
    right: Addr,

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
pub fn assemble(iter: *AssemblyIterator, allocator: std.mem.Allocator, writer: anytype) !void {
    var lines = std.ArrayList(Line).init(allocator);
    var labels = std.StringHashMap(Cpu.Addr).init(allocator);
    defer lines.deinit();
    defer labels.deinit();

    try parseLines(iter, &lines, &labels);

    for (lines.items) |line| try line.write(writer, &labels);
}

/// Parses lines from the given reader, writing the lines to the lines parameter
/// and writing any parsed labels to the labels map.
fn parseLines(reader: *AssemblyIterator, lines: *std.ArrayList(Line), labels: *std.StringHashMap(Cpu.Addr)) !void {
    var total_size: Cpu.Word = 0;

    while (try reader.next()) |line| {
        // Write a label if possible
        switch (line) {
            .label => |label| try labels.put(label, total_size),
            else => {},
        }

        total_size += line.size();

        try lines.append(line);
    }
}

/// Iterates over assembly code, one line at a time. Only parses lines with
/// meaning and returns the parsed line.
pub const AssemblyIterator = struct {
    /// Iterates over input lines. Mutate this however much you like, and it
    /// will have the side effects you expect.
    line_iterator: std.mem.TokenIterator(u8, .scalar),

    /// The location of the error that occurred.
    maybe_error: ?[]const u8 = null,

    /// Creates a new AssemblyIterator starting at the beginning of the buffer.
    pub fn init(buffer: []const u8) @This() {
        return .{
            .line_iterator = std.mem.tokenizeScalar(u8, buffer, LineBreak),
        };
    }

    /// Removes comments from a line.
    fn removeComments(line: []const u8) []const u8 {
        return if (std.mem.indexOf(u8, line, Comment)) |comment|
            line[0..comment]
        else
            line;
    }

    /// Returns the character slice of the next line, or `null` if EOF.
    fn nextRawLine(self: *@This()) ?[]const u8 {
        // Get the next line
        while (self.line_iterator.next()) |line| {
            // Remove comments
            const uncommented_line = removeComments(line);

            // Remove whitespace on both ends
            const trimmed = std.mem.trim(u8, uncommented_line, &std.ascii.whitespace);

            // Return if it has any text
            if (trimmed.len != 0) return trimmed;
        } else return null;
    }

    /// Parses the next line from this reader.
    pub fn next(self: *@This()) !?Line {
        const raw_line = self.nextRawLine() orelse return null;
        var tokens = std.mem.tokenizeAny(u8, raw_line, &std.ascii.whitespace);

        const token = tokens.next() orelse return self.fail(error.EOF, raw_line);

        const line: Line = if (std.meta.stringToEnum(Cpu.UnaryInstruction.Opcode, token)) |op|
            .{ .unary = .{ op, try self.readUnary(&tokens) } }
        else if (std.meta.stringToEnum(Cpu.BinaryInstruction.Opcode, token)) |op|
            .{ .binary = .{ op, try self.readBinary(&tokens) } }
        else if (std.mem.eql(u8, "raw", token))
            .{ .raw = try self.readAddr(&tokens) }
        else if (std.mem.eql(u8, "label", token))
            .{ .label = tokens.next() orelse return self.fail(error.ExpectedLabel, raw_line) }
        else if (std.mem.eql(u8, "bytes", token))
            .{ .bytes = tokens.rest() }
        else if (std.mem.eql(u8, "end", token))
            .end
        else
            return self.fail(error.UnknownLineType, raw_line);

        // Make sure exactly one instruction per line
        if (tokens.peek() != null) return self.fail(error.ExpectedNewline, raw_line);

        return line;
    }

    fn readUnary(self: *@This(), tokens: *std.mem.TokenIterator(u8, .any)) !UnaryOp {
        return .{
            .left = try readAddr(self, tokens),
        };
    }

    fn readBinary(self: *@This(), tokens: *std.mem.TokenIterator(u8, .any)) !BinOp {
        return .{
            .left = try readAddr(self, tokens),
            .right = try readAddr(self, tokens),
        };
    }

    fn readAddr(self: *@This(), tokens: *std.mem.TokenIterator(u8, .any)) !Addr {
        const token = tokens.next() orelse return self.fail(error.ExpectedAddress, tokens.buffer);

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

    fn fail(self: *@This(), comptime err: anytype, line: []const u8) @TypeOf(err) {
        self.maybe_error = line;
        return err;
    }
};

const std = @import("std");
const Cpu = @import("Cpu.zig");

pub const LineBreak = '\n';

pub const Comment = "//";

pub const Whitespace = " \t";

allocator: std.mem.Allocator,
buffer: []const u8,

iter: AssemblyIterator,
memory: std.ArrayList(u8),
labels: std.StringHashMap(Cpu.Addr),

pub fn init(allocator: std.mem.Allocator, buffer: []const u8) @This() {
    return .{
        .allocator = allocator,
        .buffer = buffer,

        .iter = AssemblyIterator.init(buffer),
        .memory = std.ArrayList(u8).init(allocator),
        .labels = std.StringHashMap(Cpu.Addr).init(allocator),
    };
}

pub fn deinit(self: *@This()) void {
    self.memory.deinit();
    self.labels.deinit();
}

/// Assembles a string of Overscore assembly language into a binary file for
/// input into the CPU.
pub fn assemble(self: *@This()) !std.ArrayList(u8) {
    var start: ?[]const u8 = null;

    try self.memory.appendNTimes(0, Cpu.UnitsPerWord); // Space for start

    while (self.iter.next_line()) {
        if (self.iter.next_token()) |token| {
            if (std.mem.eql(u8, "start", token)) {
                if (start == null) start = try self.read_token()
                else return error.MultipleStartDefinitions;
            } else if (std.mem.eql(u8, "block", token)) {
                try self.labels.put(try self.read_token(), @truncate(self.memory.items.len));
            } else if (std.meta.stringToEnum(Cpu.InstructionTag, token)) |tag| {
                const instruction = try self.read_instruction(tag);

                try instruction.write(self.memory.writer());
            } else if (std.mem.eql(u8, "end", token)) {
                try self.memory.append(0);
            } else if (std.mem.eql(u8, "raw", token)) {
                const tokens = &self.iter.token_iterator.?;
                try self.memory.appendSlice(tokens.rest());
                tokens.index = tokens.buffer.len;
            } else {
                return error.ExpectedBlockOrInstruction;
            }
        } else return error.ExpectedToken;

        if (self.iter.peek_token() != null) return error.UnexpectedToken;
    }

    if (start) |name| {
        if (self.labels.get(name)) |addr| {
            std.mem.writeInt(Cpu.Addr, self.memory.items[0..Cpu.UnitsPerWord], addr, .little);
        } else return error.InvalidStartSection;
    } else return error.ExpectedStartDefinition;

    return self.memory;
}

fn read_token(self: *@This()) ![]const u8 {
    const token = self.iter.next_token() orelse return error.ExpectedToken;

    if (token.len == 0) return error.ExpectedToken else return token;
}

fn read_literal(self: *@This()) !Cpu.Word {
    const token = try self.read_token();

    if (self.labels.get(token)) |value| {
        return value;
    }

    return switch (token[0]) {
        'x' => std.fmt.parseUnsigned(Cpu.Word, token[1..], 16),
        'b' => std.fmt.parseUnsigned(Cpu.Word, token[1..], 2),
        'd' => std.fmt.parseUnsigned(Cpu.Word, token[1..], 10),
        else => std.fmt.parseUnsigned(Cpu.Word, token, 16),
    };
}

fn read_unaryop(self: *@This()) !Cpu.UnaryOp {
    return .{
        .read = try self.read_literal(),
        .write = try self.read_literal(),
    };
}

fn read_binop(self: *@This()) !Cpu.BinOp {
    return .{
        .read1 = try self.read_literal(),
        .read2 = try self.read_literal(),
        .write = try self.read_literal(),
    };
}

fn read_instruction(self: *@This(), tag: Cpu.InstructionTag) !Cpu.Instruction {
    return switch (tag) {
        .set => .{ .set = try self.read_unaryop() },
        .mov => .{ .mov = try self.read_unaryop() },
        .not => .{ .not = try self.read_unaryop() },
        .@"and" => .{ .@"and" = try self.read_binop() },
        .add => .{ .add = try self.read_binop() },
        .irm => .{ .irm = try self.read_unaryop() },
        .iwm => .{ .iwm = try self.read_unaryop() },
        .sys => .{ .sys = try self.read_unaryop() },
    };
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

    fn remove_comments(line: []const u8) []const u8 {
        return if (std.mem.indexOf(u8, line, Comment)) |comment|
            line[0..comment]
        else
            line;
    }

    fn next_unprocessed(self: *@This()) ?[]const u8 {
        const line = self.line_iterator.next() orelse return null;

        const uncommented_line = remove_comments(line);

        const trimmed_line = std.mem.trim(u8, uncommented_line, Whitespace);

        return trimmed_line;
    }

    pub fn next_line(self: *@This()) bool {
        while (self.next_unprocessed()) |line| {
            if (line.len == 0) continue;

            self.token_iterator = std.mem.tokenizeAny(u8, line, Whitespace);
            return true;
        } else return false;
    }

    pub fn next_token(self: *@This()) ?[]const u8 {
        if (self.token_iterator) |*tokens| return tokens.next() else return null;
    }

    pub fn peek_token(self: *@This()) ?[]const u8 {
        if (self.token_iterator) |*tokens| return tokens.peek() else return null;
    }
};

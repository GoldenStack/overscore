const std = @import("std");
const Cpu = @import("Cpu.zig");

pub const LineBreak = '\n';

pub const Comment = "//";

pub const Whitespace = " \t";

/// Assembles a string of Overscore assembly language into a binary file for
/// input into the CPU.
pub fn assemble(allocator: std.mem.Allocator, buffer: []const u8) !std.ArrayList(u8) {
    var iter = AssemblyIterator.init(buffer);

    var start: ?[]const u8 = null;

    var memory = std.ArrayList(u8).init(allocator);
    try memory.appendNTimes(0, Cpu.UnitsPerWord); // Space for start

    var sections = std.StringHashMap(Cpu.Addr).init(allocator);
    defer sections.deinit();

    while (iter.next_line()) {
        if (iter.next_token()) |token| {
            if (std.mem.eql(u8, "start", token)) {
                if (start == null) start = try extract_start_label(&iter)
                else return error.MultipleStartDefinitions;
            } else if (std.mem.eql(u8, "block", token)) {
                try sections.put(try extract_block_label(&iter), @truncate(memory.items.len));
            } else if (std.meta.stringToEnum(Cpu.InstructionTag, token)) |tag| {
                const instruction = try extract_instruction(&iter, tag);

                try instruction.write(memory.writer());
            } else if (std.mem.eql(u8, "end", token)) {
                try memory.append(0);
            } else {
                return error.ExpectedBlockOrInstruction;
            }
        } else return error.ExpectedToken;

        if (!empty(&iter)) return error.UnexpectedToken;
    }

    if (start) |name| {
        if (sections.get(name)) |addr| {
            std.mem.writeInt(Cpu.Addr, memory.items[0..Cpu.UnitsPerWord], addr, .little);
        } else return error.InvalidStartSection;
    } else return error.ExpectedStartDefinition;

    return memory;
}

fn empty(line: *AssemblyIterator) bool {
    return line.peek_token() == null;
}

/// Reads a line that follows the format "start Block", returning the value of
/// the string "Block".
fn extract_start_label(line: *AssemblyIterator) ![]const u8 {
    const label = line.next_token() orelse return error.ExpectedToken;

    if (empty(line)) return label else return error.UnexpectedToken;
}

fn extract_block_label(line: *AssemblyIterator) ![]const u8 {
    const label = line.next_token() orelse return error.ExpectedToken;

    if (empty(line)) return label else return error.UnexpectedToken;
}

fn parse_literal(token: []const u8) !Cpu.Word {
    if (token.len == 0) return error.ExpectedToken; // Should not be possible, but handle anyway

    return switch (token[0]) {
        'x' => std.fmt.parseUnsigned(Cpu.Word, token[1..], 16),
        'b' => std.fmt.parseUnsigned(Cpu.Word, token[1..], 2),
        'd' => std.fmt.parseUnsigned(Cpu.Word, token[1..], 10),
        else => std.fmt.parseUnsigned(Cpu.Word, token, 16),
    };
}

fn extract_unary_operation(line: *AssemblyIterator) !Cpu.UnaryOp {
    const read = try parse_literal(line.next_token() orelse return error.ExpectedAddress);
    const write = try parse_literal(line.next_token() orelse return error.ExpectedAddress);

    return .{
        .read = read,
        .write = write,
    };
}

fn extract_binary_operation(line: *AssemblyIterator) !Cpu.BinOp {
    const read1 = try parse_literal(line.next_token() orelse return error.ExpectedAddress);
    const read2 = try parse_literal(line.next_token() orelse return error.ExpectedAddress);
    const write = try parse_literal(line.next_token() orelse return error.ExpectedAddress);

    return .{
        .read1 = read1,
        .read2 = read2,
        .write = write,
    };
}

fn extract_instruction(line: *AssemblyIterator, tag: Cpu.InstructionTag) !Cpu.Instruction {
    return switch (tag) {
        .set => .{ .set = try extract_unary_operation(line) },
        .mov => .{ .mov = try extract_unary_operation(line) },
        .not => .{ .not = try extract_unary_operation(line) },
        .@"and" => .{ .@"and" = try extract_binary_operation(line) },
        .add => .{ .add = try extract_binary_operation(line) },
        .irm => .{ .irm = try extract_unary_operation(line) },
        .iwm => .{ .iwm = try extract_unary_operation(line) },
        .sys => .{ .sys = try extract_unary_operation(line) },
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
        if (self.token_iterator) |*tokens| return tokens.next()
        else return null;
    }

    pub fn peek_token(self: *@This()) ?[]const u8 {
        if (self.token_iterator) |*tokens| return tokens.peek()
        else return null;
    }
};

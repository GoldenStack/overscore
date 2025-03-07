const std = @import("std");

/// The minimum addressable size of this CPU, in bits.
///
/// A dependency on standard library functions requires this to be divisible by
/// 8.
pub const UnitSize = 8;

/// The number of units (minimum addressable size) per word (the natural data
/// size for the CPU).
pub const UnitsPerWord = 4;

/// The standard size (number of bits) of data that is moved around the CPU.
/// This is always a multiple of the unit size.
pub const WordSize = UnitsPerWord * UnitSize;

/// The minimum addressable size of this CPU, as an unsigned integer.
pub const Unit = std.meta.Int(.unsigned, UnitSize);

/// The standard size for CPU operations, as an unsigned integer.
pub const Word = std.meta.Int(.unsigned, WordSize);

/// The address type for this CPU. This is equivalent to the word size.
/// Due to the structure of the CPU and various usages (such as in Set
/// immediate) this is essentially locked as being equivalent to Word.
pub const Addr = Word;

/// The size of the CPU's memory.
pub const Memory = 4096;

/// The tag for the instruction type.
pub const InstructionTag = enum(Unit) {
    set = 1,
    mov,
    not,
    @"and",
    add,
    irm,
    iwm,
    sys,

    pub fn read(reader: anytype) !InstructionTag {
        const opcode = try reader.readByte();

        return std.meta.intToEnum(InstructionTag, opcode) catch error.UnknownOpcode;
    }
};

/// A unary operation that reads from one address and writes to another one.
pub const UnaryOp = struct {
    read: Addr,
    write: Addr,

    pub fn read_from(reader: anytype) !UnaryOp {
        return .{
            .read = try reader.readInt(Addr, .little),
            .write = try reader.readInt(Addr, .little),
        };
    }
};

/// A CPU instruction.
pub const Instruction = union(InstructionTag) {
    set: UnaryOp,
    mov: UnaryOp,
    not: UnaryOp,
    @"and": UnaryOp,
    add: UnaryOp,
    irm: UnaryOp,
    iwm: UnaryOp,
    sys: UnaryOp,

    pub fn read(reader: anytype) !Instruction {
        return switch (try InstructionTag.read(reader)) {
            .set => .{ .set = try UnaryOp.read_from(reader) },
            .mov => .{ .mov = try UnaryOp.read_from(reader) },
            .not => .{ .not = try UnaryOp.read_from(reader) },
            .@"and" => .{ .@"and" = try UnaryOp.read_from(reader) },
            .add => .{ .add = try UnaryOp.read_from(reader) },
            .irm => .{ .irm = try UnaryOp.read_from(reader) },
            .iwm => .{ .iwm = try UnaryOp.read_from(reader) },
            .sys => .{ .sys = try UnaryOp.read_from(reader) },
        };
    }
};

memory: [Memory]Unit,
sys: *const fn (Word) Word,

/// Creates a new CPU. This initializes all registers to zero.
pub fn init(sys: *const fn (Word) Word) @This() {
    return .{
        .memory = [_]Unit{0} ** Memory,
        .sys = sys,
    };
}

fn word_ptr(self: *@This(), addr: Addr) *[UnitsPerWord]Unit {
    if (addr > Memory - UnitsPerWord) {
        @panic("Tried to read address outside of working memory");
    }

    return self.memory[addr..][0..UnitsPerWord];
}

fn word_read(self: *@This(), addr: Addr) Word {
    return std.mem.readInt(Word, self.word_ptr(addr), .little);
}

fn word_write(self: *@This(), addr: Addr, word: Word) void {
    std.mem.writeInt(Word, self.word_ptr(addr), word, .little);
}

pub fn prepare_instruction(self: *@This()) !?Instruction {
    const addr = self.word_read(0);

    // Verify and read the first byte
    if (addr >= Memory) return error.InstructionPointerOutsideMemory;

    // Handle opcode of 0
    if (self.memory[addr] == 0) return null;

    // Create a buffer stream of memory
    var stream = std.io.fixedBufferStream(self.memory[0..]);
    stream.pos = addr;

    // Read an instruction
    const instruction = try Instruction.read(&stream.reader());

    // Update the instruction pointer
    self.word_write(0, @truncate(stream.pos));

    return instruction;
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) void {
    switch (instruction) {
        .set => |instr| self.word_write(instr.write, instr.read),

        .mov => |instr| self.word_write(instr.write, self.word_read(instr.read)),

        .not => |instr| self.word_write(instr.write, ~self.word_read(instr.read)),

        .@"and" => |instr| self.word_write(instr.write, self.word_read(instr.write) & self.word_read(instr.read)),

        .add => |instr| self.word_write(instr.write, self.word_read(instr.write) +% self.word_read(instr.read)),

        .irm => |instr| self.word_write(instr.write, self.word_read(self.word_read(instr.read))),

        .iwm => |instr| self.word_write(self.word_read(instr.write), instr.read),

        .sys => |instr| self.word_write(instr.write, self.sys(self.word_read(instr.read))),
    }
}

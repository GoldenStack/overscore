const std = @import("std");

/// The word size of this CPU. This denotes the standard size (number of bits)
/// of data that is moved around the CPU.
pub const Word = u32;

/// The address type for this CPU. This is equivalent to the word size.
pub const Addr = Word;

/// The size of the CPU's memory.
pub const Memory = 512;

/// The tag for the instruction type.
pub const InstructionTag = enum {
    set,
    mov,
    not,
    @"and",
    add,
    mvr,
    mvw,
};

/// A binary operation that reads from two addresses and writes to a third one.
pub const BinOp = struct {
    read1: Addr,
    read2: Addr,
    write: Addr,
};

/// A unary operation that reads from one address and writes to another one.
pub const UnaryOp = struct {
    read: Addr,
    write: Addr,
};

/// A CPU instruction.
pub const Instruction = union(InstructionTag) {
    set: struct {
        addr: Addr,
        word: Word
    },
    mov: UnaryOp,
    not: UnaryOp,
    @"and": BinOp,
    add: BinOp,
    mvr: UnaryOp,
    mvw: UnaryOp,
};

memory: [Memory]u8,

/// Creates a new CPU. This initializes all registers to zero.
pub fn init() @This() {
    return .{
        .memory = [_]u8{0} ** Memory,
    };
}

fn check_addr(comptime T: type, addr: Addr) void {
    if (addr > Memory - @divExact(@typeInfo(T).int.bits, 8)) @panic("Tried to read address outside of working memory");
}

fn word_read(self: *const @This(), addr: Addr) Word {
    check_addr(Word, addr);

    const buffer = self.memory[addr..][0..@divExact(@typeInfo(Word).int.bits, 8)];

    return std.mem.readInt(Word, buffer, .little);
}

fn word_write(self: *@This(), addr: Addr, word: Word) void {
    check_addr(Word, addr);

    const buffer = self.memory[addr..][0..@divExact(@typeInfo(Word).int.bits, 8)];

    std.mem.writeInt(Word, buffer, word, .little);
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) void {
    switch (instruction) {

        .set => |instr| self.word_write(instr.addr, instr.word),

        .mov => |instr| self.word_write(instr.write, self.word_read(instr.read)),

        .not => |instr| self.word_write(instr.write, ~self.word_read(instr.read)),

        .@"and" => |instr| self.word_write(
            instr.write,
            self.word_read(instr.read1) & self.word_read(instr.read2)
        ),

        .add => |instr| self.word_write(
            instr.write,
            self.word_read(instr.read1) +% self.word_read(instr.read2)
        ),

        .mvr => |instr| self.word_write(instr.write, self.word_read(self.word_read(instr.read))),

        .mvw => |instr| self.word_write(self.word_read(instr.write), instr.read)
    }
}

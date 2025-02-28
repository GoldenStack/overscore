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
    irm,
    iwm,
    sys,
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
    irm: UnaryOp,
    iwm: UnaryOp,
    sys: UnaryOp,
};

memory: [Memory]Unit,

/// Creates a new CPU. This initializes all registers to zero.
pub fn init() @This() {
    return .{
        .memory = [_]Unit{0} ** Memory,
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

        .irm => |instr| self.word_write(instr.write, self.word_read(self.word_read(instr.read))),

        .iwm => |instr| self.word_write(self.word_read(instr.write), instr.read),

        // TODO: Implement system (hardware/os/etc) IO
        .sys => |instr| self.word_write(instr.write, 0),
    }
}

const std = @import("std");

const This = @This();

/// The word size of this CPU. This denotes the standard size (number of bits)
/// of data that is moved around the CPU.
pub const Word = u32;

/// The address type for this CPU. This is equivalent to the word size.
pub const Addr = Word;

pub const Memory = 50;

/// The tag for the instruction type.
pub const InstructionTag = enum {
    set,
    mov,
    not,
    @"and",
    @"or",
};

/// A binary operation that reads from two addresses and writes to a third one.
pub const BinOp = struct {
    read1: Addr,
    read2: Addr,
    write: Addr,

    fn apply(comptime operation: fn(Word, Word) Word, cpu: *This, addrs: BinOp) void {
        const read1 = cpu.word_read(addrs.read1);
        const read2 = cpu.word_read(addrs.read2);

        cpu.word_write(addrs.write, operation(read1, read2));
    }

    fn @"and"(x: Word, y: Word) Word {
        return x & y;
    }

    fn @"or"(x: Word, y: Word) Word {
        return x | y;
    }

};

/// A unary operation that reads from one address and writes to another one.
pub const UnaryOp = struct {
    read: Addr,
    write: Addr,

    fn apply(comptime operation: fn(Word) Word, cpu: *This, addrs: UnaryOp) void {
        const read = cpu.word_read(addrs.read);

        cpu.word_write(addrs.write, operation(read));
    }

    fn mov(x: Word) Word {
        return x;
    }

    fn @"not"(x: Word) Word {
        return ~x;
    }

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
    @"or": BinOp,
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

    const buffer = self.memory[addr..][0..4];

    return std.mem.readInt(Word, buffer, .little);
}

fn word_write(self: *@This(), addr: Addr, word: Word) void {
    check_addr(Word, addr);

    const buffer = self.memory[addr..][0..4];

    std.mem.writeInt(Word, buffer, word, .little);
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) void {
    switch (instruction) {
        .set => |instr| self.word_write(instr.addr, instr.word),
        .mov => |instr| UnaryOp.apply(UnaryOp.mov, self, instr),
        .not => |instr| UnaryOp.apply(UnaryOp.@"not", self, instr),
        .@"and" => |instr| BinOp.apply(BinOp.@"and", self, instr),
        .@"or" => |instr| BinOp.apply(BinOp.@"or", self, instr),
    }
}

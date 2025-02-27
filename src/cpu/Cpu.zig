const std = @import("std");

/// The word size of this CPU. This denotes the standard size (number of bits)
/// of data that is moved around the CPU.
pub const Word = u32;

/// A register address (the register type).
pub const Register = u5;

/// The number of registers that can be addressed.
pub const Registers: Word = 32;

/// The tag for the instruction type.
pub const InstructionTag = enum {
    set,
    mov,
    not,
    @"and",
    @"or",
};

/// A binary operation that reads from the left and the right register,
/// and writes the output of the operation into the left register.
pub const BinOp = struct {
    left: Register,
    right: Register,
};

/// A CPU instruction.
pub const Instruction = union(InstructionTag) {
    set: struct {
        reg: Register,
        word: Word
    },
    mov: BinOp,
    not: BinOp,
    @"and": BinOp,
    @"or": BinOp,
};

registers: [Registers]Word,

/// Creates a new CPU. This initializes all registers to zero.
pub fn init() @This() {
    return .{
        .registers = [_]Word{0} ** Registers,
    };
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) void {
    switch (instruction) {
        .set => |instr| self.registers[instr.reg] = instr.word,
        .mov => |instr| self.registers[instr.left] = instr.right,
        .not => |instr| self.registers[instr.left] = ~self.registers[instr.right],
        .@"and" => |instr| self.registers[instr.left] &= self.registers[instr.right],
        .@"or" => |instr| self.registers[instr.left] |= self.registers[instr.right],
    }
}
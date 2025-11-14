const std = @import("std");
const flat = @import("instruction.zig").flat;

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

/// The minimum addressable size of this CPU, as an unsigned integer type.
pub const Unit = std.meta.Int(.unsigned, UnitSize);

/// The standard size for CPU operations, as an unsigned integer type.
pub const Word = std.meta.Int(.unsigned, WordSize);

/// The address type for this CPU. This is equivalent to the word size.
/// Due to the structure of the CPU and various usages (such as in Set
/// immediate) this is essentially locked as being equivalent to Word.
pub const Addr = Word;

/// The size of the CPU's memory.
pub const Memory = 4096;

/// The type for errors that can occur while the CPU is following an
/// instruction.
pub const Error = error{
    /// Indicates that it was not possible to read a word because the address
    /// was either fully or partially outside of memory.
    AddressOutOfBounds,

    /// Indicates that an instruction could not be read because there was no
    /// memory left to read from.
    InstructionOutOfBounds,

    /// Indicates that an instruction could not be parsed because the first unit
    /// (the opcode) was unrecognized.
    UnknownOpcode,
};

memory: [Memory]Unit,
sys: *const fn (Word) Word,

/// Creates a new CPU from the given system instruction handler. This
/// initializes all memory to zero.
pub fn init(sys: *const fn (Word) Word) @This() {
    return .{
        .memory = [_]Unit{0} ** Memory,
        .sys = sys,
    };
}

fn wordSliceAt(self: *@This(), addr: Addr) Error!*[UnitsPerWord]Unit {
    if (addr > Memory - UnitsPerWord) return error.AddressOutOfBounds;

    return self.memory[addr..][0..UnitsPerWord];
}

inline fn getWordAt(self: *@This(), addr: Addr) Error!Word {
    return std.mem.readInt(Word, try self.wordSliceAt(addr), .little);
}

inline fn setWordAt(self: *@This(), addr: Addr, word: Word) Error!void {
    std.mem.writeInt(Word, try self.wordSliceAt(addr), word, .little);
}

/// Reads an instruction from the CPU, advancing the instruction pointer as
/// necessary.
pub fn prepareInstruction(self: *@This()) Error!?flat.Instruction {
    const addr = try self.getWordAt(0);

    // Cannot read instructions outside of memory
    if (addr >= Memory) return error.AddressOutOfBounds;

    // Handle opcode of 255 (invalid)
    if (self.memory[addr] == 0xff) return null;

    var index: usize = addr;
    const instruction = try flat.Instruction.read(&self.memory, &index);
    try self.setWordAt(0, @truncate(index));

    return instruction;
}

fn followUnaryInstruction(self: *@This(), unary: flat.UnaryInstruction) Error!void {
    const op1 = unary.op1;

    const slice = try self.wordSliceAt(op1);

    switch (unary.opcode) {
        .not1 => slice.* = @bitCast(~@as(Word, @bitCast(slice.*))), // Endianness is irrelevant
        .sys1 => std.mem.writeInt(Word, slice, self.sys(std.mem.readInt(Word, slice, .little)), .little),
        else => unreachable,
    }
}

fn followBinaryInstruction(self: *@This(), binary: flat.BinaryInstruction) Error!void {
    const op1 = binary.op1;
    const op2 = binary.op2;

    switch (binary.opcode) {
        .mov10 => try self.setWordAt(op1, op2),
        .mov11 => try self.setWordAt(op1, try self.getWordAt(op2)),
        .mov12 => try self.setWordAt(op1, try self.getWordAt(try self.getWordAt(op2))),
        .mov20 => try self.setWordAt(try self.getWordAt(op1), op2),
        .mov21 => try self.setWordAt(try self.getWordAt(op1), try self.getWordAt(op2)),
        .mov22 => try self.setWordAt(try self.getWordAt(op1), try self.getWordAt(try self.getWordAt(op2))),

        .and10 => try self.setWordAt(op1, try self.getWordAt(op1) & op2),
        .and11 => try self.setWordAt(op1, try self.getWordAt(op1) & try self.getWordAt(op2)),
        .or10 => try self.setWordAt(op1, try self.getWordAt(op1) | op2),
        .or11 => try self.setWordAt(op1, try self.getWordAt(op1) | try self.getWordAt(op2)),
        .add10 => try self.setWordAt(op1, try self.getWordAt(op1) +% op2),
        .add11 => try self.setWordAt(op1, try self.getWordAt(op1) +% try self.getWordAt(op2)),
        .sub10 => try self.setWordAt(op1, try self.getWordAt(op1) -% op2),
        .sub11 => try self.setWordAt(op1, try self.getWordAt(op1) -% try self.getWordAt(op2)),
        .mul10 => try self.setWordAt(op1, try self.getWordAt(op1) *% op2),
        .mul11 => try self.setWordAt(op1, try self.getWordAt(op1) *% try self.getWordAt(op2)),

        .jz10 => if (try self.getWordAt(op1) == 0) try self.setWordAt(0, op2),
        .jz11 => if (try self.getWordAt(op1) == 0) try self.setWordAt(0, try self.getWordAt(op2)),
        .jnz10 => if (try self.getWordAt(op1) != 0) try self.setWordAt(0, op2),
        .jnz11 => if (try self.getWordAt(op1) != 0) try self.setWordAt(0, try self.getWordAt(op2)),

        else => unreachable,
    }
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: flat.Instruction) Error!void {
    try switch (instruction) {
        .unary => |unary| self.followUnaryInstruction(unary),
        .binary => |binary| self.followBinaryInstruction(binary),
    };
}

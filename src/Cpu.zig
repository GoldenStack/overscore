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

/// A binary operation that takes two arguments. Typically, the left argument is
/// written to.
pub const BinOp = struct {
    left: Addr,
    right: Addr,

    pub fn read(reader: anytype) Error!BinOp {
        return .{
            .left = reader.readInt(Addr, .little) catch return error.InstructionOutOfBounds,
            .right = reader.readInt(Addr, .little) catch return error.InstructionOutOfBounds,
        };
    }
};

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

    pub fn read(reader: anytype) Error!InstructionTag {
        const opcode = reader.readByte() catch return error.InstructionOutOfBounds;

        return std.meta.intToEnum(InstructionTag, opcode) catch error.UnknownOpcode;
    }
};

/// A CPU instruction.
pub const Instruction = union(InstructionTag) {
    set: BinOp,
    mov: BinOp,
    not: BinOp,
    @"and": BinOp,
    add: BinOp,
    irm: BinOp,
    iwm: BinOp,
    sys: BinOp,

    pub fn read(reader: anytype) Error!Instruction {
        return switch (try InstructionTag.read(reader)) {
            .set => .{ .set = try BinOp.read(reader) },
            .mov => .{ .mov = try BinOp.read(reader) },
            .not => .{ .not = try BinOp.read(reader) },
            .@"and" => .{ .@"and" = try BinOp.read(reader) },
            .add => .{ .add = try BinOp.read(reader) },
            .irm => .{ .irm = try BinOp.read(reader) },
            .iwm => .{ .iwm = try BinOp.read(reader) },
            .sys => .{ .sys = try BinOp.read(reader) },
        };
    }
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

fn getWordAt(self: *@This(), addr: Addr) Error!Word {
    return std.mem.readInt(Word, try self.wordSliceAt(addr), .little);
}

fn setWordAt(self: *@This(), addr: Addr, word: Word) Error!void {
    std.mem.writeInt(Word, try self.wordSliceAt(addr), word, .little);
}

/// Reads an instruction from the CPU, advancing the instruction pointer as
/// necessary.
pub fn prepareInstruction(self: *@This()) Error!?Instruction {
    const addr = try self.getWordAt(0);

    // Cannot read instructions outside of memory
    if (addr >= Memory) return error.AddressOutOfBounds;

    // Handle opcode of 0
    if (self.memory[addr] == 0) return null;

    // Create a buffer stream of memory
    var stream = std.io.fixedBufferStream(self.memory[0..]);
    stream.pos = addr;

    // Read an instruction
    const instruction = try Instruction.read(&stream.reader());

    // Update the instruction pointer
    try self.setWordAt(0, @truncate(stream.pos));

    return instruction;
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) Error!void {
    try switch (instruction) {
        .set => |op| self.setWordAt(op.left, op.right),

        .mov => |op| self.setWordAt(op.left, try self.getWordAt(op.right)),

        .not => |op| self.setWordAt(op.left, ~try self.getWordAt(op.right)),

        .@"and" => |op| self.setWordAt(op.left, try self.getWordAt(op.left) & try self.getWordAt(op.right)),

        .add => |op| self.setWordAt(op.left, try self.getWordAt(op.left) +% try self.getWordAt(op.right)),

        .irm => |op| self.setWordAt(op.left, try self.getWordAt(try self.getWordAt(op.right))),

        .iwm => |op| self.setWordAt(try self.getWordAt(op.left), op.right),

        .sys => |op| self.setWordAt(op.left, self.sys(try self.getWordAt(op.right))),
    };
}

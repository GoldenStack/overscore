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

pub const Error = error{
    AddressOutOfBounds, // Called when reading a word from an address not fully in memory
    InstructionOutOfBounds, // Called when reading an instruction fails because memory ends before it is fully read
    UnknownOpcode, // Called when trying to read an instruction but the opcode is unrecognized
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

/// Creates a new CPU. This initializes all registers to zero.
pub fn init(sys: *const fn (Word) Word) @This() {
    return .{
        .memory = [_]Unit{0} ** Memory,
        .sys = sys,
    };
}

fn word_ptr(self: *@This(), addr: Addr) Error!*[UnitsPerWord]Unit {
    if (addr > Memory - UnitsPerWord) return error.AddressOutOfBounds;

    return self.memory[addr..][0..UnitsPerWord];
}

fn word_read(self: *@This(), addr: Addr) Error!Word {
    return std.mem.readInt(Word, try self.word_ptr(addr), .little);
}

fn word_write(self: *@This(), addr: Addr, word: Word) Error!void {
    std.mem.writeInt(Word, try self.word_ptr(addr), word, .little);
}

/// Reads an instruction from the CPU, advancing the instruction pointer as
/// necessary.
pub fn prepare_instruction(self: *@This()) Error!?Instruction {
    const addr = try self.word_read(0);

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
    try self.word_write(0, @truncate(stream.pos));

    return instruction;
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) Error!void {
    try switch (instruction) {
        .set => |op| self.word_write(op.right, op.left),

        .mov => |op| self.word_write(op.right, try self.word_read(op.left)),

        .not => |op| self.word_write(op.right, ~try self.word_read(op.left)),

        .@"and" => |op| self.word_write(op.right, try self.word_read(op.right) & try self.word_read(op.left)),

        .add => |op| self.word_write(op.right, try self.word_read(op.right) +% try self.word_read(op.left)),

        .irm => |op| self.word_write(op.right, try self.word_read(try self.word_read(op.left))),

        .iwm => |op| self.word_write(try self.word_read(op.right), op.left),

        .sys => |op| self.word_write(op.right, self.sys(try self.word_read(op.left))),
    };
}

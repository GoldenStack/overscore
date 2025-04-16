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
};

/// The tag for the instruction type.
pub const InstructionTag = enum(Unit) {
    set = 1,
    mov,
    nand,
    add,
    irm,
    iwm,
    sys,
    not,
    @"and",
    @"or",
    ori,
    addi,
    sub,
    subi,
    mul,
    muli,
    iwmi,
    nrm,
    jz,
    jnz,
};

/// A CPU instruction.
pub const Instruction = union(InstructionTag) {
    set: BinOp,
    mov: BinOp,
    nand: BinOp,
    add: BinOp,
    irm: BinOp,
    iwm: BinOp,
    sys: BinOp,
    not: BinOp,
    @"and": BinOp,
    @"or": BinOp,
    ori: BinOp,
    addi: BinOp,
    sub: BinOp,
    subi: BinOp,
    mul: BinOp,
    muli: BinOp,
    iwmi: BinOp,
    nrm: BinOp,
    jz: BinOp,
    jnz: BinOp,
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

fn readBinOp(self: *@This(), index: *usize) Error!BinOp {
    const left = try self.getWordAt(@truncate(index.*));
    const right = try self.getWordAt(@truncate(index.* + 4));
    index.* += 8;
    return .{ .left = left, .right = right };
}

fn readInstructionTag(self: *@This(), index: *usize) Error!InstructionTag {
    const opcode = self.memory[index.*];
    index.* += 1;

    if (opcode == 0 or opcode > @intFromEnum(InstructionTag.jnz)) return error.UnknownOpcode;
    return @enumFromInt(opcode);
}

fn readInstruction(self: *@This(), index: *usize) Error!Instruction {
    const tag = try self.readInstructionTag(index);

    return switch (tag) {
        .set => .{ .set = try self.readBinOp(index) },
        .mov => .{ .mov = try self.readBinOp(index) },
        .nand => .{ .nand = try self.readBinOp(index) },
        .add => .{ .add = try self.readBinOp(index) },
        .irm => .{ .irm = try self.readBinOp(index) },
        .iwm => .{ .iwm = try self.readBinOp(index) },
        .sys => .{ .sys = try self.readBinOp(index) },
        .not => .{ .not = try self.readBinOp(index) },
        .@"and" => .{ .@"and" = try self.readBinOp(index) },
        .@"or" => .{ .@"or" = try self.readBinOp(index) },
        .ori => .{ .ori = try self.readBinOp(index) },
        .addi => .{ .addi = try self.readBinOp(index) },
        .sub => .{ .sub = try self.readBinOp(index) },
        .subi => .{ .subi = try self.readBinOp(index) },
        .mul => .{ .mul = try self.readBinOp(index) },
        .muli => .{ .muli = try self.readBinOp(index) },
        .iwmi => .{ .iwmi = try self.readBinOp(index) },
        .nrm => .{ .nrm = try self.readBinOp(index) },
        .jz => .{ .jz = try self.readBinOp(index) },
        .jnz => .{ .jnz = try self.readBinOp(index) },
    };
}

/// Reads an instruction from the CPU, advancing the instruction pointer as
/// necessary.
pub fn prepareInstruction(self: *@This()) Error!?Instruction {
    const addr = try self.getWordAt(0);

    // Cannot read instructions outside of memory
    if (addr >= Memory) return error.AddressOutOfBounds;

    // Handle opcode of 0
    if (self.memory[addr] == 0) return null;

    var index: usize = addr;
    const instruction = try self.readInstruction(&index);
    try self.setWordAt(0, @truncate(index));

    return instruction;
}

/// Follows the provided CPU instruction.
pub fn follow(self: *@This(), instruction: Instruction) Error!void {
    try switch (instruction) {
        .set => |op| self.setWordAt(op.left, op.right),

        .mov => |op| self.setWordAt(op.left, try self.getWordAt(op.right)),

        .nand => |op| self.setWordAt(op.left, ~(try self.getWordAt(op.left) & try self.getWordAt(op.right))),

        .add => |op| self.setWordAt(op.left, try self.getWordAt(op.left) +% try self.getWordAt(op.right)),

        .irm => |op| self.setWordAt(op.left, try self.getWordAt(try self.getWordAt(op.right))),

        .iwm => |op| self.setWordAt(try self.getWordAt(op.left), try self.getWordAt(op.right)),

        .sys => |op| self.setWordAt(op.left, self.sys(try self.getWordAt(op.right))),

        .not => |op| self.setWordAt(op.left, ~try self.getWordAt(op.right)),

        .@"and" => |op| self.setWordAt(op.left, try self.getWordAt(op.left) & try self.getWordAt(op.right)),

        .@"or" => |op| self.setWordAt(op.left, try self.getWordAt(op.left) | try self.getWordAt(op.right)),

        .ori => |op| self.setWordAt(op.left, try self.getWordAt(op.left) | op.right),

        .addi => |op| self.setWordAt(op.left, try self.getWordAt(op.left) +% op.right),

        .sub => |op| self.setWordAt(op.left, try self.getWordAt(op.left) -% try self.getWordAt(op.right)),

        .subi => |op| self.setWordAt(op.left, try self.getWordAt(op.left) -% op.right),

        .mul => |op| self.setWordAt(op.left, try self.getWordAt(op.left) *% try self.getWordAt(op.right)),

        .muli => |op| self.setWordAt(op.left, try self.getWordAt(op.left) *% op.right),

        .iwmi => |op| self.setWordAt(try self.getWordAt(op.left), op.right),

        .nrm => |op| self.setWordAt(op.left, if (try self.getWordAt(op.right) == 0) 0 else 1),

        .jz => |op| if (try self.getWordAt(op.left) == 0) try self.setWordAt(0, op.right),

        .jnz => |op| if (try self.getWordAt(op.left) != 0) try self.setWordAt(0, op.right),
    };
}

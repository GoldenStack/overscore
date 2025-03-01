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

    pub fn write(self: @This(), writer: anytype) !void {
        const opcode: Unit = switch (self) {
            .set => 0b0001,
            .mov => 0b0010,
            .not => 0b0011,
            .@"and" => 0b0100,
            .add => 0b0101,
            .irm => 0b0110,
            .iwm => 0b0111,
            .sys => 0b1000,
        };

        try writer.writeByte(opcode);
    }
};

/// A binary operation that reads from two addresses and writes to a third one.
pub const BinOp = struct {
    read1: Addr,
    read2: Addr,
    write: Addr,

    pub fn read_from(reader: anytype) !BinOp {
        return .{
            .read1 = try reader.readInt(Addr, .little),
            .read2 = try reader.readInt(Addr, .little),
            .write = try reader.readInt(Addr, .little),
        };
    }

    pub fn write_to(self: @This(), writer: anytype) !void {
        try writer.writeInt(Addr, self.read1, .little);
        try writer.writeInt(Addr, self.read2, .little);
        try writer.writeInt(Addr, self.write, .little);
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

    pub fn write_to(self: @This(), writer: anytype) !void {
        try writer.writeInt(Addr, self.read, .little);
        try writer.writeInt(Addr, self.write, .little);
    }
};

/// A CPU instruction.
pub const Instruction = union(InstructionTag) {
    set: UnaryOp,
    mov: UnaryOp,
    not: UnaryOp,
    @"and": BinOp,
    add: BinOp,
    irm: UnaryOp,
    iwm: UnaryOp,
    sys: UnaryOp,

    pub fn write(instruction: @This(), writer: anytype) !void {
        try InstructionTag.write(instruction, writer);

        switch (instruction) {
            .set => |instr| try instr.write_to(writer),
            .mov => |instr| try instr.write_to(writer),
            .not => |instr| try instr.write_to(writer),
            .@"and" => |instr| try instr.write_to(writer),
            .add => |instr| try instr.write_to(writer),
            .irm => |instr| try instr.write_to(writer),
            .iwm => |instr| try instr.write_to(writer),
            .sys => |instr| try instr.write_to(writer),
        }
    }

    pub fn read(reader: anytype) !Instruction {
        const opcode = try reader.readByte();

        return switch (opcode) {
            0b0001 => .{ .set = try UnaryOp.read_from(reader) },
            0b0010 => .{ .mov = try UnaryOp.read_from(reader) },
            0b0011 => .{ .not = try UnaryOp.read_from(reader) },
            0b0100 => .{ .@"and" = try BinOp.read_from(reader) },
            0b0101 => .{ .add = try BinOp.read_from(reader) },
            0b0110 => .{ .irm = try UnaryOp.read_from(reader) },
            0b0111 => .{ .iwm = try UnaryOp.read_from(reader) },
            0b1000 => .{ .sys = try UnaryOp.read_from(reader) },
            else => return error.InvalidOpcode,
        };
    }

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

fn opcode_size(opcode: Unit) ?Word {
    return switch (opcode) {
        0b0001, 0b0010, 0b0011 => 9,
        0b0100, 0b0101 => 13,
        0b0110, 0b0111, 0b1001 => 9,
        else => null,
    };
}

pub fn instruction_read(self: *@This()) !?Instruction {
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

pub fn loop(self: *@This()) !void {
    while (try self.instruction_read()) |instruction| self.follow(instruction);
}
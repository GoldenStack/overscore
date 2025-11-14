//! Instruction-handling functions and structs. This includes instruction
//! reading and writing, and the two instruction struct formats.

const std = @import("std");
const Cpu = @import("Cpu.zig");
const Unit = Cpu.Unit;
const Addr = Cpu.Addr;
const Word = Cpu.Word;

/// Flat instructions, stored in the format that the CPU can understand.
pub const flat = struct {

    /// A flat CPU instruction.
    pub const Instruction = union(enum) {
        unary: UnaryInstruction,
        binary: BinaryInstruction,

        /// Reads an instruction from a memory array and an index.
        pub fn read(memory: *const [Cpu.Memory]Unit, index: *usize) !Instruction {
            if (memory[index.*] & 0b10000000 == 0) {
                return .{ .unary = try UnaryInstruction.read(memory, index) };
            } else {
                return .{ .binary = try BinaryInstruction.read(memory, index) };
            }
        }
    };

    /// Flattened opcodes for each instruction, stored in the way the CPU can
    /// understand.
    pub const Opcode = enum(Unit) {
        not1 = 0,
        sys1,

        mov10 = 128,
        mov11,
        mov12,
        mov20,
        mov21,
        mov22,

        and10,
        and11,
        or10,
        or11,
        add10,
        add11,
        sub10,
        sub11,
        mul10,
        mul11,

        jz10,
        jz11,
        jnz10,
        jnz11,
    };

    /// The data for a unary instruction: an opcode and an address.
    pub const UnaryInstruction = struct {
        opcode: Opcode,
        op1: Addr,

        /// Reads a unary instruction from a memory array and an index.
        pub fn read(memory: *const [Cpu.Memory]Unit, index: *usize) !UnaryInstruction {
            if (index.* > memory.len - 5) return error.InstructionOutOfBounds;

            const opcode = memory[index.*];
            if (opcode > @intFromEnum(Opcode.sys1)) return error.UnknownOpcode;

            const op1 = std.mem.readInt(Word, memory[index.*+1..][0..Cpu.UnitsPerWord], .little);

            index.* += 5;

            return .{
                .opcode = @enumFromInt(opcode),
                .op1 = op1,
            };
        }
    };

    /// The data for a binary instruction: an opcode and two addresses.
    pub const BinaryInstruction = struct {
        opcode: Opcode,
        op1: Addr,
        op2: Addr,

        /// Reads a binary instruction from a memory array and an index.
        pub fn read(memory: *const [Cpu.Memory]Unit, index: *usize) !BinaryInstruction {
            if (index.* > memory.len - 9) return error.InstructionOutOfBounds;

            const opcode = memory[index.*];
            if (opcode > @intFromEnum(Opcode.jnz11)) return error.UnknownOpcode;

            const op1 = std.mem.readInt(Word, memory[index.*+1..][0..Cpu.UnitsPerWord], .little);
            const op2 = std.mem.readInt(Word, memory[index.*+5..][0..Cpu.UnitsPerWord], .little);

            index.* += 9;

            return .{
                .opcode = @enumFromInt(opcode),
                .op1 = op1,
                .op2 = op2,
            };
        }
    };
};


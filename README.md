# Overscore

Overscore is a custom CPU emulator, instruction set architecture, machine code,
assembler, and (in the future) high-level (C, Zig, etc.) language.

## CPU

32-bit CPU with 8 bit minimum addressable size. This is extensively configurable
and modifying the values in-code should not break any "hardware".

All address space/memory is in one rwx (read-write-execute) block, including
registers and the instruction counter (address 0). This allows jumps to be
implemented by adding to the instruction counter at address 0.

### Instruction Set

| Name  | Opcode | Description        | Raw size (words) |
|-------|--------|--------------------|------------------|
| `set` | 000001 | Set immediate      | 2                |
| `mov` | 000010 | Move               | 2                |
| `not` | 000011 | Not                | 2                |
| `and` | 000100 | And                | 3                |
| `add` | 000101 | Add                | 3                |
| `mvr` | 000111 | Reading move       | 2                |
| `mvw` | 001000 | Writing move       | 2                |
| `sys` | 001001 | System instruction | 2                |

Invalid opcodes always error. Addressing invalid memory always errors.

Instruction size (in bytes) equals `1 + 4 * words`. Again, this is extensively
configurable; a more general equation is `(UnitSize + WordSize * words) / 8`.

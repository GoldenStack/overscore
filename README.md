# Overscore

Overscore is a custom CPU emulator, instruction set architecture, machine code,
assembler, and (in the future) high-level (C, Zig, etc.) language.

## CPU

32-bit CPU with 8 bit minimum addressable size. This is extensively configurable
and modifying the values in-code should not break any "hardware".

All address space/memory is in one rwx (read-write-execute) block, including
registers and the instruction counter (address 0). This allows jumps to be
implemented with arithmetic on the instruction counter at address 0.

### Instruction Set

| Name  | Opcode | Description           | Raw size (words) | C-like equivalent|
|-------|--------|-----------------------|------------------|------------------|
| `set` | 000001 | Set immediate         | 2                | `a = <constant>` |
| `mov` | 000010 | Move                  | 2                | `a = b`          |
| `not` | 000011 | Not                   | 2                | `a = ~b`         |
| `and` | 000100 | And                   | 3                | `a = b & c`      |
| `add` | 000101 | Add                   | 3                | `a = b + c`      |
| `irm` | 000111 | Indirect reading move | 2                | `a = *b`         |
| `iwm` | 001000 | Indirect writing move | 2                | `*a = b`         |
| `sys` | 001001 | System instruction    | 2                | `a = sys(b)`*    |
> _*where sys is a standard IO function_

> _Note: C-like equivalents address with variables for simplicity; a more
> accurate representation for e.g. `mov` might be `mem[a] = mem[b]`._

Invalid opcodes always error. Addressing invalid memory always errors.

Instruction size (in bytes) equals `1 + 4 * words`. Again, this is extensively
configurable; a more general equation is `(UnitSize + WordSize * words) / 8`.

# Overscore CPU

The CPU is a simple 32-bit CPU with 8 bit minimum addressable size and a small
instruction set with many variants per instruction, following concepts from
[ForwardCom](https://www.forwardcom.info/). The architecture size and minimum
addressable size are extensively configurable, and modifying constants in the
emulator should not break any of the hardware.

All address space/memory is in one rwx (read-write-execute) block, including the
instruction counter at address 0. This allows jumps to be implemented with
arithmetic on the instruction counter at address 0. There are no registers;
everything refers to memory directly.

When reading instructions, the CPU silently exits when the first byte read is
`0xFF`. Otherwise, reading from invalid memory or having an invalid opcode makes
the CPU exit, as there is currently no error/interrupt handling.

## Instruction Set

The instruction set consists of 10 instructions, each with several variants. It
is heavily inspired by [x86](https://www.felixcloutier.com/x86/),
[RISCV](https://github.com/riscv/riscv-isa-manual/), and most importantly
[ForwardCom](https://www.forwardcom.info/).

Eight (8) variants (`mov10`, `mov11`, `mov12`, `mov21`, `not1`, `and11`,
`add11`, and `sys1`) are denoted denoted as "core" variants, with every other
instruction and instruction variant defined in terms of them. This dichotomy
exists to demonstrate that the CPU is exceedingly simple at its core, but
instructions and variants past the core 8 are implemented to make sure that it
s still reasonably to program in (a semi-documented Fibonacci implementation
using just core instructions was 600 lines long). The core instructions could
be simplified further (e.g. with `nand11` replacing `not1` and `and11`, and with
self-modifying code replacing several of the `mov` variants), but the point is
not to achieve absolutely minimality in hopes of the instruction set not being a
[Turing tarpit](https://en.wikipedia.org/wiki/Turing_tarpit) (it could always
[be simpler](https://en.wikipedia.org/wiki/One-instruction_set_computer)).

Having more instructions past the core eight helps decrease the size of
programs (something which is already of concern due to the register-free
structure), increases optimizability with more specialized instructions, and
helps the instruction set *feel* more powerful whilst not necessarily being more
powerful in reality.

Instructions are 5 or 9 bytes. The first bit indicates which size it has; `0`
indicates a length of 5, and `1` indicates a length of 9. The rest of the first
byte is the opcode of the instruction. Opcodes are specific to the length of the
instruction; for example, a first byte of `00000001` denotes the `not1`
instruction, while `10000001` corresponds to `mov11`. This means there is room
for 128 instructions of each size, for a total of 256.

Instruction names are generally suffixed with the levels of indirection for
their argument(s). For example, `not1` means there is one level of indirection
used, corresponding to `*a = ~*a`. A hypothetical `not2` would be represented as
`**a = ~**a`. All instructions with two levels of indirection are variants of
the `mov` instruction; for example, `mov12` corresponds to `*a = **b`, as the
`1` means one level of indirection on `a` and the `2` means two levels of
indirection on `b`.

Instructions with a length of 5 have one operand, and are called one-operand (or
unary) instructions, while instructions with a length of 9 are called
two-operand (or binary) instructions.

## One-operand (unary) instructions

```
┌───────────────────────────────────┐
|  INSTRUCTION (40 BITS / 5 BYTES)  | 
├──────────┬────────────┬───────────┤
│ SIZE (1) │ OPCODE (7) | OP_A (32) |
└──────────┴────────────┴───────────┘

```

| Name              | Description | Variant | Opcode | C-like equivalent |
|-------------------|-------------|---------|--------|-------------------|
| [`not`](#not)     | Not         | not1    | 0      | `*a = ~*a`        |
| [`sys`](#syscall) | Syscall     | sys1    | 1      | `*a = sys(*a)`*   |
> _*where sys is a standard IO function_

## Two-operand (binary) instructions

```
┌───────────────────────────────────────────────┐
|        INSTRUCTION (72 BITS / 9 BYTES)        |
├──────────┬────────────┬───────────┬───────────┤
│ SIZE (1) │ OPCODE (7) | OP_A (32) | OP_B (32) |
└──────────┴────────────┴───────────┴───────────┘

```

| Name               | Description     | Variant | Opcode | C-like equivalent      |
|--------------------|-----------------|---------|--------|------------------------|
| [`mov`](#move)     | Move            | mov10   | 0      | `*a = b`               |
|                    |                 | mov11   | 1      | `*a = *b`              |
|                    |                 | mov12   | 2      | `*a = **b`             |
|                    |                 | mov20   | 3      | `**a = b`              |
|                    |                 | mov21   | 4      | `**a = *b`             |
|                    |                 | mov22   | 5      | `**a = **b`            |
| [`and`](#and)      | And             | and10   | 6      | `*a &= b`              |
|                    |                 | and11   | 7      | `*a &= *b`             |
| [`or`](#or)        | Or              | or10    | 8      | `*a \|= b`             |
|                    |                 | or11    | 9      | `*a \|= *b`            |
| [`add`](#add)      | Add             | add10   | 10     | `*a += b`              |
|                    |                 | add11   | 11     | `*a += *b`             |
| [`sub`](#subtract) | Sub             | sub10   | 12     | `*a -= b`              |
|                    |                 | sub11   | 13     | `*a -= *b`             |
| [`mul`](#multiply) | Multiply        | mul10   | 14     | `*a *= b`              |
|                    |                 | mul11   | 15     | `*a *= *b`             |
| [`jz`](#jz)        | Jump if zero    | jz10    | 16     | `if (*a == 0) *0 = b`  |
|                    |                 | jz11    | 17     | `if (*a == 0) *0 = *b` |
| [`jnz`](#jnz)      | Jump if nonzero | jnz10   | 18     | `if (*a != 0) *0 = b`  |
|                    |                 | jnz11   | 19     | `if (*a != 0) *0 = *b` |

TODO: Descriptions of each instruction.

TODO: Define each instruction in terms of the core instructions.

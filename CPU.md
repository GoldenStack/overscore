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

For binary instructions, the operand that can be thought of as the "output" or
"write" operand is always the left operand. For unary instructions, the only
operand is always the write operand, since there are no no-op instructions.

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

| Name          | Description | Variant | Opcode | C-like equivalent |
|---------------|-------------|---------|--------|-------------------|
| [`not`](#not) | Not         | not1    | 0      | `*a = ~*a`        |
| [`sys`](#sys) | Syscall     | sys1    | 1      | `*a = sys(*a)`*   |
> _*where sys is a standard IO function_

## Two-operand (binary) instructions

```
┌───────────────────────────────────────────────┐
|        INSTRUCTION (72 BITS / 9 BYTES)        |
├──────────┬────────────┬───────────┬───────────┤
│ SIZE (1) │ OPCODE (7) | OP_A (32) | OP_B (32) |
└──────────┴────────────┴───────────┴───────────┘

```

| Name          | Description     | Variant | Opcode | C-like equivalent      |
|---------------|-----------------|---------|--------|------------------------|
| [`mov`](#mov) | Move            | mov10   | 0      | `*a = b`               |
|               |                 | mov11   | 1      | `*a = *b`              |
|               |                 | mov12   | 2      | `*a = **b`             |
|               |                 | mov20   | 3      | `**a = b`              |
|               |                 | mov21   | 4      | `**a = *b`             |
|               |                 | mov22   | 5      | `**a = **b`            |
| [`and`](#and) | And             | and10   | 6      | `*a &= b`              |
|               |                 | and11   | 7      | `*a &= *b`             |
| [`or`](#or)   | Or              | or10    | 8      | `*a \|= b`             |
|               |                 | or11    | 9      | `*a \|= *b`            |
| [`add`](#add) | Add             | add10   | 10     | `*a += b`              |
|               |                 | add11   | 11     | `*a += *b`             |
| [`sub`](#sub) | Sub             | sub10   | 12     | `*a -= b`              |
|               |                 | sub11   | 13     | `*a -= *b`             |
| [`mul`](#mul) | Multiply        | mul10   | 14     | `*a *= b`              |
|               |                 | mul11   | 15     | `*a *= *b`             |
| [`jz`](#jz)   | Jump if zero    | jz10    | 16     | `if (*a == 0) *0 = b`  |
|               |                 | jz11    | 17     | `if (*a == 0) *0 = *b` |
| [`jnz`](#jnz) | Jump if nonzero | jnz10   | 18     | `if (*a != 0) *0 = b`  |
|               |                 | jnz11   | 19     | `if (*a != 0) *0 = *b` |

## Descriptions

### Not

Bitwise unary NOT on the bit sat the target address, with one level of
indirection.

### Sys

Performs a syscall, sending the value of the given address to the system and
replacing the value with the result of the syscall. Note that this is at most a
pseudo-syscall as it doesn't actually resemble a typical hardware syscall that
closely.

### Mov

Moves a value into an address, supporting one or two levels of indirection on
the output operand, and zero, one, or two levels of indirection on the other
operand.

### And

Bitwise binary AND on the bits of the two operands, with one level of
indirection on the output and zero or one levels of indirection on the other
operand.

### Not

Bitwise binary NOT on the bits of the two operands, with one level of
indirection on the output and zero or one levels of indirection on the other
operand.

### Add

Unsigned wrapping addition of the two operands, writing into the left operand.
Supports one level of indirection on the left operand, and zero or one levels of
indirection on the right operand.

### Sub

Unsigned wrapping subtraction of the two operands, writing into the left
operand. Supports one level of indirection on the left operand, and zero or one
levels of indirection on the right operand. Strictly equivalent to
`a - b = a + 1 + ~b`.

### Mul

Unsigned wrapping multiplication of the two operands, writing into the left
operand. Supports one level of indirection on the left operand, and zero or one
levels of indirection on the right operand.

### Jz

Conditional jump to the right operand if the left operand is zero. Supports one
level of indirection on the left operand, and zero or one levels of indirection
on the right operand.

### Jnz

Conditional jump to the right operand if the left operand is any number other
than zero. Supports one level of indirection on the left operand, and zero or
one levels of indirection on the right operand.

## "Micro-instructions"

If the 8 core instructions (`mov10`, `mov11`, `mov12`, `mov21`, `not1`, `and11`,
`add11`, and `sys1`) are considered the "micro-instructions" of this CPU,
usage of the term being heavily debatable as the whole instruction set is not
actually interpreted as these micro-instructions, then all the other variants
must be able to be defined in terms of these.

Many of these instructions simply require a temporary variable in able to be
emulated with existing instructions.

### `mov20`

```asm
// emulate `mov [[a]] b` with:
mov [Tmp1] b
mov [[a]] [Tmp1]
```

### `mov22`

```asm
// emulate `mov [[a]] [[b]]` with:
mov [Tmp1] [[b]]
mov [[a]] [Tmp1]
```

### `and10`

```asm
// emulate `and [a] b` with:
mov [Tmp1] b
and [a] [Tmp1]
```

### `or10`

```asm
// emulate `or [a] b` with:
mov [Tmp1] b
or [a] [Tmp1]
```

### `or11`

```asm
// emulate `or [a] [b]` with:
not [a]
not [b]
and [a] [b]
```
Note: this definition mutates `b`, which is not done in the non-emulated version
of this instruction. This is fine as it simply serves as a proof of concept.

### `add10`

```asm
// emulate `add [a] b` with:
mov [Tmp1] b
add [a] [Tmp1]
```
### `sub10`

```asm
// emulate `sub [a] b` with:
mov [Tmp1] b
sub [a] [Tmp1]
```

### `sub11`

```asm
// emulate `sub [a] [b]` with:
add [a] 1
not [b]
add [a] [b]
```
Note: this definition mutates `b`, which is not done in the non-emulated version
of this instruction. This is fine as it simply serves as a proof of concept.

### `mul10`

```asm
// emulate `mul [a] b` with:
mov [Tmp1] b
mul [a] [b]
```

### `mul11`

TODO
```asm
// emulate `mul [a] [b]` with:
```

### `jz10`

```asm
// emulate `jz [a] b` with:
mov [Tmp1] b
jz [a] [b]
```

### `jz11`

TODO
```asm
// emulate `jz [a] [b]` with:
not [a]
jnz [a] [b]
```
Note: this definition mutates `b`, which is not done in the non-emulated version
of this instruction. This is fine as it simply serves as a proof of concept.

### `jnz10`

```asm
// emulate `jnz [a] b` with:
mov [Tmp1] b
jnz [a] [b]
```

### `jnz11`

This is an incredibly hard expression to emulate, given no conditional branching
exists at all. Several hours were spent working this out, and it turns out that
the solution is having a jump table with the length of the minimum addressable
size.

You see, the core issue with branching is not "how to tell if something is
zero", it is "what does it mean to tell if something is zero"--this is because
in order to conditionally jump to A or B, you need a number that is one of two
known values, ideally 0 or 1, which can be manipulated into the number of
bytes you want to jump by, so that it can be added to the instruction pointer.

Thus, what is required for a "conditional jump" is simply a way to "normalize" a
number into one of two outcomes, again, ideally 0 or 1, so that math can be
performed on the result to determine how much to jump by. This is unfortunately
only possible with a (in this case) 256^n element long jump table, where if
`n=1` this table will need to be used 4 times, if `n=2` then it will need to be
used twice, and if `n=4` it will only need to be used once. Clearly, the only
reasonable case is `n=1`.

The strategy for "normalizing" a number is simply to mask off an individual
byte, multiply that number by `9` by adding it to itself several times, and then
add that number to the instruction pointer. An example:

```c
// This function uses static addresses in memory as stack space, which is
// possible as this function is not technically a "real" function and can never
// be called twice at the same time.

// Returns 0 if the input is 0, and returns 1 if the input is any number other
// than zero. Only works on a byte; providing larger numbers is UB.
label Normalize-Byte:
    mov [Tmp1] [Check.Value] // Copy the value once (1x)
    add [Tmp1] [Tmp1]        // Double it (2x)
    add [Tmp1] [Tmp1]        // Double it (4x)
    add [Tmp1] [Tmp1]        // Double it (8x)
    add [Tmp1] [Check.Value] // Add x     (9x)

    add [#0] [Tmp1] // Skip ahead!

    mov [#0] [Check.Zero]
    mov [#0] [Check.NonZero]
    // ... 253 copies of `mov [#0] [Check.NonZero]`
    mov [#0] [Check.NonZero]

label Normalize-Byte.Zero:
    // Return 0
    mov [Normalize-Byte.Return] #0
    mov [#0] Normalize-Byte.ReturnAddr

label Normalize-Byte.NonZero:
    // Return 1
    mov [Normalize-Byte.Return] #1
    mov [#0] Normalize-Byte.ReturnAddr

// "Stack" space
label Normalize-Byte.Value:
    word #0 // The parameter

label Normalize-Byte.Return:
    raw #0 // The return value

label Normalize-Byte.ReturnAddr:
    raw #0 // The return address
```

This example provides a way to normalize bytes. As explained, it can be easily
extended to work for entire words, by masking out each byte in the word. I
promise I have not been able to find a better way to work this out; everything
else has a circular dependency on something that implicitly branches (e.g.
multiplication) or just otherwise does not work.




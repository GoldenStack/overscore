# Overscore

Overscore is a from-scratch CPU emulator, instruction set, assembler, and (in
the future) high-level (C, Zig, etc) language. It's an entirely separate
computer ecosystem, starting from the ground up.

# CPU

The CPU is a simple 32-bit CPU with 8 bit minimum addressable size and a small
instruction set. The architecture size and minimum addressable size are
extensively configurable, and modifying constants in the emulator should not
break any of the hardware.

All address space/memory is in one rwx (read-write-execute) block, including the
instruction counter at address 0. This allows jumps to be implemented with
arithmetic on the instruction counter at address 0. There are no registers;
everything refers to memory directly.

## Instruction Set

The instruction set consists of a core set of 7 micro-instructions, along with
13 macro-instructions that can be reconstructed out of micro-instructions. It is
heavily inspired by [x86](https://www.felixcloutier.com/x86/),
[RISCV](https://github.com/riscv/riscv-isa-manual/), and most importantly
[ForwardCom](https://www.forwardcom.info/).

The dichotomy of micro-instructions and macro-instructions, especially with the
requirement that macro-instructions must be constructible from
micro-instructions, exists to demonstrate that the CPU is exceedingly simple at
its core, but also to make sure that it's still reasonable to program in (a
semi-documented Fibonacci implementation with just the micro-instructions was
600 lines long).

This also allows decreasing the size of programs, increasing optimizability with
more specialized instructions, and making the instruction set *feel* more
powerful whilst not necessarily being more powerful in reality.

Note that there is no difference in implementation between micro-instructions
and macro-instructions.

Instructions start with a one-byte opcode, written here in decimal but encoded
as unsigned one-byte integers.

## Micro-instructions

| Name                            | Opcode | Description           | Total size (bytes) | C-like equivalent |
|---------------------------------|--------|-----------------------|--------------------|-------------------|
| [`set`](#set-immediate)         | 1      | Set immediate         | 9                  | `*a = b`          |
| [`mov`](#move)                  | 2      | Move                  | 9                  | `*a = *b`         |
| [`nand`](#nand)                 | 3      | Nand                  | 9                  | `*a = ~(*a & *b)` |
| [`add`](#add)                   | 4      | Add                   | 9                  | `*a = *a + *b`    |
| [`irm`](#indirect-reading-move) | 5      | Indirect reading move | 9                  | `*a = **b`        |
| [`iwm`](#indirect-writing-move) | 6      | Indirect writing move | 9                  | `**a = *b`        |
| [`sys`](#system-instruction)    | 7      | System instruction    | 9                  | `*a = sys(*b)`*   |
> _*where sys is a standard IO function_

## Macro-instructions
| Name                                       | Opcode | Description                     | Total size (bytes) | C-like equivalent |
|--------------------------------------------|--------|---------------------------------|--------------------|-------------------|
| [`not`](#not)                              | 8      | Not                             | 9                  | `*a = ~*b`        |
| [`and`](#and)                              | 9      | And                             | 9                  | `*a = *a & *b`    |
| [`or`](#or)                                | 10     | Or                              | 9                  | `*a = *a \| *b`   |
| [`ori`](#or-immediate)                     | 11     | Or immediate                    | 9                  | `*a = *a \| b`    |
| [`addi`](#add-immediate)                   | 12     | Add immediate                   | 9                  | `*a = *a + b`     |
| [`sub`](#subtract)                         | 13     | Subtract                        | 9                  | `*a = *a - *b`    |
| [`subi`](#subtract-immediate)              | 14     | Subtract immediate              | 9                  | `*a = *a - b`     |
| [`mul`](#multiply)                         | 15     | Multiply                        | 9                  | `*a = *a * *b`    |
| [`muli`](#multiply-immediate)              | 16     | Multiply immediate              | 9                  | `*a = *a * b`     |
| [`iwmi`](#indirect-writing-move-immediate) | 17     | Indirect writing move immediate | 9                  | `**a = b`         |
| [`nrm`](#normalize)                        | 18     | Normalize                       | 9                  | `*a = *b != 0`    |
| [`jz`](#jump-if-zero)                      | 19     | Jump if zero                    | 9                  | `if (*a) *0 = b`  |
| [`jnz`](#jump-if-nonzero)                  | 20     | Jump if nonzero                 | 9                  | `if (!*a) *0 = b` |

TODO: Write mappings from macro-instruction -> micro-instruction

The instruction set is intentionally relatively limited to maintain overall
simplicity. It [could be simpler](https://en.wikipedia.org/wiki/One-instruction_set_computer), 
but this has been intentionally avoided as to prevent the CPU from becoming a
[Turing tarpit](https://en.wikipedia.org/wiki/Turing_tarpit).

For example, it's possible to remove some of the indirect move instructions by
modifying relevant addresses in the code itself, but this requires
self-modifying code and increases the complexity greatly.

A null byte (opcode zero) exits. Otherwise, invalid opcodes always error.
Addressing invalid memory always errors (errors are defined as emulator exits).

Instruction size (in bytes) is calculated as `1 + 4 * words`. Again, this is 
extensively configurable; a more general equation is
`(UnitSize + WordSize * words) / 8`.

All instructions currently accept two arguments and are thus all the same size.

TODO: Description for new instructions

### Set immediate
| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `set` | 9                  | 0001 | `write` (1 word) | `read` (1 word) |

Sets the value at `write` to the immediate (i.e., specified inline) `read`.
This treats `read` as a value and not a register.

### Move

| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `mov` | 9                  | 0010 | `write` (1 word) | `read` (1 word) |

Sets the value of `write` to the value of `read`.

### Not

| Name  | Total size (bytes) | Data | ...              |                  |
|-------|--------------------|------|------------------|------------------|
| `not` | 9                  | 0011 | `write` (1 word) | `read` (1 word) |

Sets the value of `write` to the binary complement of the value of `read`.

### And

| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `and` | 9                  | 0100 | `write` (1 word) | `read` (1 word) |

Sets the value of `write` to the binary AND of the values of `read` and `write`.

### Add

| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `and` | 9                  | 0101 | `write` (1 word) | `read` (1 word) |

Sets the value of `write` to the wrapping unsigned sum of the values of `read`
and `write`.

### Indirect reading move

| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `irm` | 9                  | 0110 | `write` (1 word) | `read` (1 word) |

Sets the value of `write` to the value of the value of `read`. This is
equivalent to `mov` except instead of the value of `read` being interpreted as a
word, the value of `read` is treated as an address and the value of this address
is treated as a word. This is similar to having dereferenced the right argument.

### Indirect writing move

| Name  | Total size (bytes) | Data | ...              |                  |
|-------|--------------------|------|------------------|------------------|
| `iwm` | 9                  | 0111 | `write` (1 word) | `read` (1 word) |

Sets the value of the value of `write` to the value of `read`. This is
equivalent to `mov` except instead of the value of `write` being set, the value
of `write` is interpreted as an address and the value of this address is written
to instead. This is similar to having dereferenced the left argument.

### System instruction

| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `sys` | 9                  | 1000 | `write` (1 word) | `read` (1 word) |

Performs some arbitrary syscall. This is essentially a hardware/operating system
defined call, and is the only standard way of communicating with the
environment. Typically it might not be optimal to bottleneck all communication
through a single limited operation, but this is not the typical CPU.

# Assembly

The assembler is a simple text-based way to write machine code, with a few
higher-level features.

Lines are processed and written to the output binary in the order they were
written in the file.

Comments are written with `//`. Semicolons are not supported as comments and
will cause an assembler error.

Here's an example program. The next few sections will help with understanding
it.
```
raw Main

label Main
    set 100 AA
    not 104 100
    and 100 104
    end
```

## Line Syntax

### Statements

| Name             | Syntax          | Output size |
|------------------|-----------------|-------------|
| [Raw](#raw)      | `raw <WORD>`    | 4 bytes     |
| [Label](#labels) | `label <NAME>`  | 0 bytes     |
| [Bytes](#bytes)  | `bytes <BYTES>` | variable    |
| [End](#end)      | `end`           | 1 byte      |

### Instructions

| Name  | Syntax              | Output size |
|-------|---------------------|-------------|
| `set` | `set <ADDR> <WORD>` | 9 bytes     |
| `mov` | `mov <ADDR> <ADDR>` | 9 bytes     |
| `not` | `not <ADDR> <ADDR>` | 9 bytes     |
| `and` | `and <ADDR> <ADDR>` | 9 bytes     |
| `add` | `add <ADDR> <ADDR>` | 9 bytes     |
| `irm` | `irm <ADDR> <ADDR>` | 9 bytes     |
| `iwm` | `iwm <ADDR> <ADDR>` | 9 bytes     |
| `sys` | `sys <ADDR> <ADDR>` | 9 bytes     |

## Instructions

Instructions are referred to using their shortened name. The destination address
is placed on the left. For example:
```
    set 100 AA  // Set address 100 (in hexadecimal) to AA
    not 104 100 // Flip all of the bits of 100, writing to 104 
    and 100 104 // Binary AND on 100 and 104, writing to 100
```

This works the same for every instruction, so it should make sense from here.

Instructions always have a size equivalent to their size as indicated in the
[instruction set](#instruction-set).

## Literals

Number literals can be formatted in hexadecimal, binary, or decimal.

By default, they're parsed as hexadecimal.

When the documentations refers to an address or a word, it's referring to a
literal.

```
    set 0 AA        // Set address 0 to AA (hexadecimal)
    set 0 d170      // Set address 0 to 170 (decimal)
    set 0 b10101010 // Set address 0 to 10101010 (binary)
    set 0 xAA       // Set address 0 to AA (hexadecimal)
```

## Labels

The line `label Main` (putting the name of the label where `Main` is) allows
referring to the address of the next instruction after the label by the name of
the label. This is useful for jumping to or reading from certain areas, because
there's no way to guarantee where in memory the code is.

Writing a label (e.g. `Main`) instead of an address will have the label replaced
with the address when assembling.

Labels always have a size of zero, because they're an assembler construct.

## Raw

You can embed 4 bytes (the size of a CPU word) directly in the binary with the
`raw` line.

This is useful for embedding data that doesn't fit nicely into ASCII, or for
setting the instruction pointer. Since binaries always start at address `0`,
embedding the address of a block (e.g. `raw Main`) will end up setting the
instruction pointer to the given address when the binary is assembled.

This doesn't allow writing smaller amounts of data directly in the binary (e.g.
1 byte), at least for now.

`raw` always has a size of 4 bytes (the word size).

## Bytes

Write `bytes <BYTES>` to insert the bytes provided directly in the binary. This
is typically useful for embedding strings into the binary, because including
non-ACSII characters directly

Printing strings can be implemented this way by writing, for example:
```
label Message
    bytes Hello, world!
    end
```

The `label` is included to make it possible to refer to where the string is
placed.

Since there's no way to get the length of the embedded bytes, `end` was added to
make the string null-terminated.

`bytes` has a size of however many bytes you provide to it.

## End

You can place a null byte in the binary with the `end` line.

This is typically useful for ending the program or for padding.

`end` has a size of one byte.

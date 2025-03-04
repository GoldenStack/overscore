# Overscore

Overscore is a custom CPU emulator, instruction set architecture, machine code,
assembler, and (in the future) high-level (C, Zig, etc.) language.

# CPU

The CPU is a simple 32-bit CPU with 8 bit minimum addressable size and 8
instructions. The architecture size and minimum addressable size are extensively
configurable and modifying the values in-code should not break any "hardware".

All address space/memory is in one rwx (read-write-execute) block, including
registers and the instruction counter (address 0). This allows jumps to be
implemented with arithmetic on the instruction counter at address 0.

## Instruction Set

| Name  | Opcode | Description           | Total size (bytes) | C-like equivalent|
|-------|--------|-----------------------|--------------------|------------------|
| `set` | 0001   | Set immediate         | 9                  | `a = <constant>` |
| `mov` | 0010   | Move                  | 9                  | `a = b`          |
| `not` | 0011   | Not                   | 9                  | `a = ~b`         |
| `and` | 0100   | And                   | 9                  | `a = a & b`      |
| `add` | 0101   | Add                   | 9                  | `a = a + b`      |
| `irm` | 0110   | Indirect reading move | 9                  | `a = *b`         |
| `iwm` | 0111   | Indirect writing move | 9                  | `*a = b`         |
| `sys` | 1000   | System instruction    | 9                  | `a = sys(b)`*    |
> _*where sys is a standard IO function_

> _Note: C-like equivalents address with variables for simplicity; a more
> accurate representation for e.g. `mov` might be `mem[a] = mem[b]`._

The instruction set is intentionally relatively limited to maintain overall
simplicity. It [could be simpler](https://en.wikipedia.org/wiki/One-instruction_set_computer), 
but this has been intentionally avoided as to prevent the CPU from becoming a
[Turing tarpit](https://en.wikipedia.org/wiki/Turing_tarpit).

A null byte (opcode zero) exits. Otherwise, invalid opcodes always error.
Addressing invalid memory always errors (errors are defined as emulator exits).

Instruction size (in bytes) is calculated as `1 + 4 * words`. Again, this is 
extensively configurable; a more general equation is
`(UnitSize + WordSize * words) / 8`.

All instructions currently accept two arguments and are thus all the same size.

### Set immediate
| Name  | Total size (bytes) | Data | ...              |                 |
|-------|--------------------|------|------------------|-----------------|
| `set` | 9                  | 0001 | `write` (1 word) | `read` (1 word) |

Sets the value at `write` to the immediate (i.e., specified inline) `read`.
This treats `read` as a value and not a register.

### Move

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `mov` | 9                  | 0010 | `read` (1 word) | `write` (1 word) |

Sets the value of `write` to the value of `read`.

### Not

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `not` | 9                  | 0011 | `read` (1 word) | `write` (1 word) |

Sets the value of `write` to the binary complement of the value of `read`.

### And

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `and` | 9                  | 0100 | `read` (1 word) | `write` (1 word) |

Sets the value of `write` to the binary AND of the values of `read` and `write`.

### Add

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `and` | 9                  | 0101 | `read` (1 word) | `write` (1 word) |

Sets the value of `write` to the wrapping unsigned sum of the values of `read`
and `write`.

### Indirect reading move

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `irm` | 9                  | 0110 | `read` (1 word) | `write` (1 word) |

Sets the value of `write` to the value of the value of `read`. This is
equivalent to `mov` except instead of the value of `read` being interpreted as a
word, the value of `read` is treated as an address and the value of this address
is treated as a word.

### Indirect writing move

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `iwm` | 9                  | 0111 | `read` (1 word) | `write` (1 word) |

Sets the value of the value of `write` to the value of `read`. This is
equivalent to `mov` except instead of the value of `write` being set, the value
of `write` is interpreted as an address and the value of this address is written
to instead.

### System instruction

| Name  | Total size (bytes) | Data | ...             |                  |
|-------|--------------------|------|-----------------|------------------|
| `sys` | 9                  | 1000 | `read` (1 word) | `write` (1 word) |

Performs some arbitrary syscall. This is essentially a hardware/operating system
defined call, and is the only standard way of communicating with the
environment. Typically it might not be optimal to bottleneck all communication
through a single limited operation, but this is not the typical CPU.

# Assembly

The assembler is a simple text-based way to write machine code, with a few
higher-level features.

Comments are written with `//`. Semicolons are not supported as comments and
will cause an assembler error.

## Blocks
The directive `block Main` (where `Main` is the name of the block) allows
referring to the address of the next instruction by the name of the block. This
is useful for jumping to or reading from certain blocks, because their spot in
memory is not necessarily known.

Current assembler limitations prevent blocks from being referred to before they
are declared, making it only possible to jump backwards to blocks.

## Start

As the assembled binary is loaded at address `0`, the first word size of
the binary are used for storing the start address. This is done with
`start Main`, where `Main` is the name of the block.

The `start` directive is not subject to the limitation of blocks only being
referred to after their declaration.

## Instructions

Instructions can be referred to using their shortened name. The destination
address is placed second. For example:
```asm
    set AA  100 // Set address 100 (in hexadecimal) to AA
    not 100 104 // Flip all of the bits of 100, writing to 104 
    and 104 100 // Binary AND on 100 and 104, writing to 100
```

## Literals

Number literals can be formatted in hexadecimal, binary, or decimal.

By default, they're parsed as hexadecimal.

```asm
    set AA        0 // Set address 0 to AA (hexadecimal)
    set d170      0 // Set address 0 to 170 (decimal)
    set b10101010 0 // Set address 0 to 10101010 (binary)
    set x100      0 // Set address 0 to AA (hexadecimal)
```

## End

You can place a null byte in the binary with the `end` directive.

This is typically useful for ending the program, or for padding.

## Raw

Use `raw <BYTES>` to insert the bytes directly in the binary. This is useful for
embedding strings into the binary.

Printing strings can be implemented this way by writing, for example:
```asm
block Message
    raw Hello, world!
    end
```

The `block` is included to make it possible to refer to where the string is
placed.

Since there's no way to get the length of the embedded bytes, the `end`
directive was added to make the string null-terminated.

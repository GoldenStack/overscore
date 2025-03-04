# Overscore

Overscore is a custom CPU emulator, instruction set architecture, machine code,
assembler, and (in the future) high-level (C, Zig, etc.) language.

# CPU

32-bit CPU with 8 bit minimum addressable size. This is extensively configurable
and modifying the values in-code should not break any "hardware".

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
|-------|--------------------|------|-----------------|---------- -------|
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





# Overscore Assembly

The assembler provides a simple text-based way to write machine code, with a few
higher level features.

Lines of assembly correspond directly to bytes in the output binary, so lines
are processed in order, serialized, and written to the output binary. This makes
understanding the effects of your assembly easier, given the direct
correspondence.

Comments are written with `//`. Semicolons are not supported as comments and
will cause an assembler error.

Here's an example program. The next few sections will help with understanding
it.
```
raw Main

label Main
    mov10 100 AA
    mov11 104 100
    not1  104
    and11 100 104
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

Instructions are written with the instruction name (e.g. `mov11`), and then the
argument(s). The numbers at the end refer to the levels of indirection for the
instruction; check [CPU.md](CPU.md) for details. The sizes of each instruction
are also described there.

## Instructions

Instructions are referred to using their shortened name. The destination address
is typically the left argument. For example:
```
    mov10 100 AA  // Set address 100 (in hexadecimal) to AA
    mov11 104 100 // Copy address 100 to address 104
    not1  104     // Flip all the bits of address 104
    and11 100 104 // Binary AND on 100 and 104, writing to 100
```

This works the same for every instruction, so it should make sense from here.

## Literals

Number literals can be formatted in hexadecimal, binary, or decimal. By default,
they're parsed as hexadecimal.

When the documentations refers to an address or a word, it's referring to a
literal.

```
    mov10 0 AA        // Set address 0 to AA (hexadecimal)
    mov10 0 d170      // Set address 0 to 170 (decimal)
    mov10 0 b10101010 // Set address 0 to 10101010 (binary)
    mov10 0 xAA       // Set address 0 to AA (hexadecimal)
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

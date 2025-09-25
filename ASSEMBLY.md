# Overscore Assembly

The assembler provides a simple text-based way to write machine code, with a few
higher level features.

Lines of assembly correspond directly to bytes in the output binary, so lines
are processed in order, serialized, and written to the output binary. This
ensures the correspondence in the output binary is obvious. This usage of
"lines" is not without reason--the assembler enforces at most one line of
assembly per line in the text file.

Comments are written with `//`. Semicolons are not supported as comments and
will cause an assembler error.

Here's an example program. The next few sections will help with understanding
it.
```
word Main

label Main:
    mov [#100x] #AAx
    mov [#104x] [#100x]
    not [#104x]
    and [#100x] [#104]
    end
```

## Line Syntax

A line (that's not blank or a comment) consists of a single statement, which is
either an instruction or another control statement that emits specific bytes.

| Name                         | Syntax          | Output size  |
|------------------------------|-----------------|--------------|
| [Word](#word)                | `word <WORD>`   | 4 bytes      |
| [Label](#label)              | `label <NAME>`  | 0 bytes      |
| [Bytes](#bytes)              | `bytes <BYTES>` | variable     |
| [End](#end)                  | `end`           | 1 byte       |
| [Instruction](#instructions) | variable        | 4 or 9 bytes |

### Word

A word consists of a number literal or the name of a label. Number literals
always start with `#` to indicate that the token is a number, and if a number
literal is longer than a character, you need to specify its base at the end.

Bases are either `b` for binay, `d` for decimal, or `x` for hexidecimal. For
example, `#A6x`, `#166d`, and `#10100110b` all represent the same number (166).
You can specify a base for number literals with one digit, but this isn't
necessary.

You can also place the name of a label instead of a number! This will be
replaced with the address of the label when the program is compiled. Forward
references are allowed, meaning you can refer to a label that is defined later
in the file.

It's customary to place a word literal as the first line in the file, because
the first four bytes correspond to setting the starting position of the
instruction pointer. This explains the `word Main` and `label Main:` pattern.

A word always has the size of a CPU word, which is 4 bytes here.

### Label

Labels are zero-size lines that allow giving a name to a specific address in the
file. They don't correspond to any output binary directly, as labels are an
assembler construct. You can use them in the place of any number literal, and
when the file is assembled they will be replaced with the address of the first
line after the label.

This can be used for labelling code blocks and jumping to them, or for giving
names to static memory so that it can be used, like for the stack pointer.

For example, the label of the stack pointer looks like this:

```c
// Add a label for the stack and initialize the pointer
label Stack:
    word StackStart

// MORE CODE...

// Start the stack right after the code ends
label StackStart:
```

This declares the stack start to be after all of the code in the file, and the
stack pointer begins as a pointer to this location. Note that this
implementation means that the stack actually grows upwards instead of downwards,
the opposite of how the stack is normally structured.

### Bytes

The `bytes` statement simply embeds some bytes directly in the binary. You
specify a whitespace-separated list of numbers that fit into an unsigned byte,
and they are placed directly in the output binary. For example, this is how you
embed `"Hello!"` into the binary:

```c
bytes #48x #65x #6Cx #6Cx #6Fx #21x
```

This has a size of however many bytes you provide, as one would expect.

### End

The `end` statement includes a single `#FFx` byte in the binary, which is
understood by the CPU as a direction to immediately stop executing. It's
basically a shorthand for `bytes #FFx`.

### Instructions

Instructions are the core construct for communicating to the CPU. They consist
of unary instructions and binary instructions, which take up 5 bytes and 9
bytes, respectively. They're written with their name, followed by the arguments.

Arguments can be surrounded in "levels of indirection", which refers to the
amount of indirection that a value takes. For example, `8` refers to the number
8, but `[8]` refers to the *value* of the address with number 8. The output
always requires at least one level of indirection, since it wouldn't make sense
to reassign a number - only modifying the value of an address makes sense.

Most instructions support one level of indirection for output arguments, and
zero or one levels of indirection for the other input argument. The exception to
this is `mov`, which supports up to two levels of indirection for its arguments.

You can see [CPU.md](CPU.md) for more details on the inner workings of each
instruction, but generally they are referred to with their shortened name,
followed by the arguments.

Some examples:
```c
    mov [#100x] #AAx    // Set address 100 (in hexadecimal) to AA
    mov [#104x] #100x   // Copy address 100 to address 104
    not [#104x]         // Flip all the bits of address 104
    and [#100x] [#104x] // Binary AND on 100 and 104, writing to 100
```

This works the same for every instruction, so this should make sense from here
if you've read CPU.md.



# Overscore

Overscore is a from-scratch CPU emulator, instruction set, assembler, and (in
the future) high-level (C, Zig, etc) language. It's an entirely separate
computer ecosystem, starting from the ground up.

# CPU

The CPU is a simple 32-bit CPU with 8 bit minimum addressable size and a small
instruction set, with 8 core instructions and 19 instructions in total.

Extensive documentation for the CPU exists in [CPU.md](CPU.md).

# Assembly

The assembler provides a simple text-based way to write machine code, with a few
higher level features.

Here's an example program. Documentation for the assembly can be found in
[ASSEMBLY.md](ASSEMBLY.md).
```
raw Main

label Main
    mov10 100 AA
    mov11 104 100
    not1  104
    and11 100 104
    end
```

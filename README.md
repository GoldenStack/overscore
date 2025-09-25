# Overscore

Overscore is a from-scratch CPU emulator, instruction set, assembler, and
high-level programming language. It's an entirely separate computer ecosystem,
starting from the ground up.

A little more detail for the CPU, emulator, assembler, and language:
- The CPU is a simple 32-bit CPU with 8 bit minimum addressable size and a small instruction set designed from scratch, but with several variants per instruction.
- The emulator emulates the behaviour of instructions from the instruction set. It handles 400 million instructions per second on my machine.
- The assembler converts readable assembly directly into bytes and machine code, with a few higher level constructs.
- The language is a tokenizer, parser, interpreter, and compiler, all in one. It's the most complex part of this project by far.

Overscore serves as a modern look into the hardware and software ecosystem if it
were designed by a college freshman in 2025 (because it was). It's both an
experiment with what improvements can be made by designing everything from
scratch with a philosophy of simplicity in mind, as well as a learning project.

This is primarily a solo project, but if there are any questions (like "How do I
run this myself?"), suggestions, or just general thoughts (maybe even some ideas
for CPUs, instruction sets, or programming languages that you've had on your own
that you want to share!), feel free to open an issue or contact me otherwise! I
do follow the Zig philosophy of having a single individual with a dedicated
vision for this project, though.

# CPU

The CPU is a simple 32-bit CPU with 8 bit minimum addressable size and a small
instruction set, with 8 core instructions and 19 instructions in total.

Extensive documentation for the CPU exists in [CPU.md](CPU.md).

# Assembly

The assembler provides a simple text-based way to write machine code, with a few
higher level features.

Here's an example program. Documentation for the assembly can be found in
[ASSEMBLY.md](ASSEMBLY.md).
```c
word Main

label Main:
    mov [#100x] #AAx
    mov [#104x] [#100x]
    not [#104x]
    and [#100x] [#104]
    end
```

# Language

The language is a work in progress. Documentation will be added soon, but for
now you can take a look at the [rationale](https://gist.github.com/GoldenStack/09cb66ec29ff80e0aebda528d2cdb2e4)
which exposes several features of the language.

# Roadmap

A roadmap for the project exists in [ROADMAP.md](ROADMAP.md). It addresses
several ideas and proposals for the project, mainly centering on the CPU, ISA,
and assembler because the language is an incredibly complicated (and
work-in-progress) part on its own.

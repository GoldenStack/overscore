# Overscore

Overscore is a from-scratch CPU emulator, instruction set, assembler, and
high-level programming language. It's an entirely separate computer ecosystem,
starting from the ground up.

Overscore serves as a modern look into the hardware and software ecosystem if it
were designed by a college freshman in 2025 (because it was). It's both an
experiment with what improvements can be made by designing everything from
scratch with a philosophy of simplicity in mind, as well as a learning project.

This is primarily a solo project, but if there are any questions (like "How do I
run this myself?"), suggestions, or just general thoughts (maybe even some ideas
for CPUs, instruction sets, or programming languages that you've had on your own
that you want to share!), you can open an issue or message me on Discord (link
on [my website](https://goldenstack.net/)). I do follow the Zig philosophy of
having a single individual with a dedicated vision for this project, though.

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

# Language

The language is a work in progress. Documentation will be added soon.

# Roadmap

A roadmap for the project exists in [ROADMAP.md](ROADMAP.md). It addresses
several ideas and proposals for the project, mainly centering on the CPU, ISA,
and assembler because the language is an incredibly complicated (and
work-in-progress) part on its own.
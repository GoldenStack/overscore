
# TODO

Various ideas currently intended to be implemented, or under consideration for implementation, in the future.

## Hardened CPU architecture

Currently, the CPU is structured for essentially single program, single
permission execution. There is no invalid state handling and any error
immediately stops execution. Normal CPUs do not function this way, and the
current structure makes it essentially impossible to trust or limit any
sub-programs that need to be run after the CPU has booted.

This proposal addresses the idea of a more hardened CPU architecture, designed
for resilience after errors during execution and support for an idea similar to
having multiple privilege levels. Haywire programs should not be able to stop
execution, and ideally the special case of a null opcode ending execution would
be removed.

This proposal focuses on two central ideas:
- A _global descriptor table_ or equivalent, providing rules as to which
  sections of memory are accessible (reading, writing, jumping) from which
  positions, or perhaps a simpler system that accomplishes a similar thing
- An _interrupt descriptor table_ or equivalent, providing a set of addresses to
  jump to when it is necessary to handle certain interrupts. Following this
  table likely will ignore the global descriptor table due to the limited scope
  of this table, as well as the fact that interrupt handling can be designed
  with this knowledge.

A simple way to achieve the global descriptor table (GDT) likely entails having
a list of boundaries in memory, across which reads, writes, or jumps cannot
occur. This would encourage locality and keep the system simple, but might not
work out with the concept of virtual addresses.

This table would only permit adding region descriptor entries around regions
that the current region is not subject to. Essentially, it would be possible to
allocate any regions anywhere before there are any, but adding more would only
be able to make privilege more granular, and it's not possible to remove a
region descriptor entry from within the region, of course. This means that any
region-based activities should not be proxied through function calls because
they will apply from the called region and can potentially result in privilege
escalation.

Programs could be initialized by writing the program to memory and then adding a
barrier to it. How would barrier removal work, though? If you can't remove a
barrier you're affected by, programs could just remove barriers for other
programs. I suppose specifying a removal point on creation might be a good
option.

Entering a block would only be possible from the left, by jumping to the
instruction right before it starts and requiring the CPU to increase the
instruction pointer past the boundary. Exiting from the block, then, would only
be possible from the right, whether the limited block naturally reaches the exit
or if there's a jump to right before the right side of the block.

Interrupts would be another way to exit from blocks. Interrupts would be
implemented with an interrupt descriptor table (IDT), storing, at minimum, an
array (where the index is the interrupt id) of table entries, with each entry
containing the destination address to jump to on interrupt, as well as the
address to write the previous address that was being executed from.

Interrupts would be trusted to be jumped from anywhere because they have access
to the point they were jumped to from, so they can be designed around this
possibility.

One issue is that this system doesn't support the concept of *privilege levels*—
it only supports the idea of memory barriers. This means it might be impossible
to jump back to a given section in memory. A simple system could exist on top of
this, but again it would add more complexity.

## Micro-operations and macro-operations

The Overscore instruction set is very minimal. This is intended to keep the
complexity of the CPU low so that is both easy to implement and easy to program
in. However, keeping the complexity so low makes the CPU less and less useful,
inciting comments like "you're basically in esolang territory at that point".

Implementing micro-operations and macro-operations allows the complexity to
remain relatively low, but makes writing assembly much easier and provides more
room for optimizations.

Specifically, all current operations will become micro-operations, and several
larger macro-operations will be defined solely out of these micro-operations,
introducing no new features into the CPU. From an assembly programmer's
perspective, they might be considered as just syntax sugar. Thus, the macro-
instructions can kind of be thought of as macros.

Ideally, all operations are dependent on the word size, not the minimum
addressable size.

Implementation tactics may include macro-operation fusion.

## Minimum addressable size of one bit

Makes the minimum addressable size of the CPU equal to one (1) bit.

### Pros
- Makes multiplication easier
- Allows arbitrary bit shifting without an extra operation
- More variation when creating jump tables
- Doesn't increase any existing complexity when writing machine code

### Cons
- Significant performance decrease
- For emulator: not compatible with existing CPU architectures
- For physical circuit: requires many more wires

## Bracket syntax for opcodes

Allows indication of indirect references with brackets around the name (e.g.
`[Block]`), as is done in other assemblies, all under the `mov` instruction.
This means:
- `set a b` -> `mov [a] b`
- `mov a b` -> `mov [a] [b]`
- `irm a b` -> `mov [a] [[b]]`
- `iwm a b` -> `mov [[a]] [b]`

Since the left argument must always have always at least one layer of
indirection, it may be advisable to treat it as having an implicit layer of
indirection, as such:
- `set a b` -> `mov a b`
- `mov a b` -> `mov a [b]`
- `irm a b` -> `mov a [[b]]`
- `iwm a b` -> `mov [a] [b]`

This **would not** affect the instruction set; only the assembler is changed.

### Pros
- Easier to write assembly
- Insruction names make more sense when writing
- Easier to grasp indirection

### Cons
- Further of an abstraction from assembly
- Some combinations wouldn't be allowed for seemingly no good reason (e.g.
  `mov [[a]] [[b]]`) because of machine code restrictions
  - If `mov [[a]] [[b]]` is supported via conversion to existing instructions,
    the assembly is even further from machine code and makes less sense
- More typing for the same instructions

## Less detailed items

- Structure this like a CLI app, e.g. `cat main.asm | overscore assemble | overscore emulate`
- Improve error messages in the assembler
- Compress instruction sizes
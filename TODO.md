
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
  sections of memory are accessible (reading, writing, execution) from which
  positions, or perhaps a simpler system that accomplishes a similar thing
- An _interrupt descriptor table_ or equivalent, providing a set of addresses to
  jump to when it is necessary to handle certain interrupts. Following this
  table likely will ignore the global descriptor table due to the limited scope
  of this table, as well as the fact that interrupt handling can be designed
  with this knowledge.

TODO: Explain in more detail

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


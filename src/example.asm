raw Main // Set the instruction pointer to Main

label Message
    bytes Hello, world!
    raw x0000000A

label Main
    // Set the iteration index to start at Message
    set Message f00

label Loop
    // Clear bytes
    set 0 f10
    set 0 f11

    // Chop out the extra bytes
    irm f00 f10 // DE AD BE EF 00
    set 0 f11   // DE 00 00 00 00

    // Okay, so there are 256 possibilities.
    // The first one is the case where `c = 0`, so we just want to end,
    // so we just put one `end` instruction (well, 9 for padding).
    // Otherwise, we don't care about it.
    // It would be faster to include jumps that all jump to the same region, but
    // for simplicity we'll just make them all no-ops.
    // In total, we want to skip 9c, so we do that.

    mov f10 f14 // Copy the value once (1c)
    add f14 f14 // Double it (2c)
    add f14 f14 // Double it (4c)
    add f14 f14 // Double it (8c)
    add f10 f14 // Add c (9c)
    add f14 0  // Skip ahead!
    end
    raw 0
    raw 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
    set LookupEnd 0
label LookupEnd
        
    // Print characters
    set 1   f13  // DE 00 00 01 00
    sys f10 f10

    // Go to the next byte
    set 1   f10
    add f10 f00

    // Loop back to the start
    set Loop 0

    end

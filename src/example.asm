raw Main // Set the instruction pointer to Main

label Message
    bytes Hello, world!
    raw x0000000A

label Main
    // Set the iteration index to start at Message
    set f00 Message

label Loop
    // Clear bytes
    set f10 0
    set f11 0

    // Copy to other section and chop out the extra bytes
    irm f10 f00 // DE AD BE EF 00
    set f11 0   // DE 00 00 00 00

    // Okay, so there are 256 possibilities.
    // The first one is the case where `c = 0`, so we just want to end,
    // so we just put one `end` instruction (well, 9 for padding).
    // Otherwise, we don't care about it.
    // It would be faster to include jumps that all jump to the same region, but
    // for simplicity we'll just make them all no-ops.
    // In total, we want to skip 9c, so we do that.

    mov f14 f10 // Copy the value once (1c)
    add f14 f14 // Double it (2c)
    add f14 f14 // Double it (4c)
    add f14 f14 // Double it (8c)
    add f14 f10 // Add c (9c)
    add 0   f14  // Skip ahead!
    end
    raw 0
    raw 0
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
    set 0 LookupEnd
label LookupEnd
        
    // Print characters
    set f13 1  // DE 00 00 01 00
    sys f10 f10

    // Go to the next byte
    set f10 1
    add f00 f10

    // Loop back to the start
    set 0 Loop

    end

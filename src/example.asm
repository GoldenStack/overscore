start Main

block Message
    raw Hello, world!
    end

block Main
//    set b10101010 100 // Can replace b10101010 with AA or xAA or d170
//    not 100 104
//    and 100 104 100
    
    // Set the iteration index to start at Message
    set Message 400

block Loop
    // Clear bytes
    set 0 500
    set 0 501

    // Chop out the extra bytes
    irm 400 500 // DE AD BE EF 00
    set 0 501   // DE 00 00 00 00

    // TODO: Exit if equals zero

    // Print characters
    set 1   503  // DE 00 00 01 00
    sys 500 500

    // Go to the next byte
    set 1   500
    add 400 500 400

    // Loop back to the start
    set Loop 0

    end

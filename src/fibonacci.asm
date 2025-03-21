raw Main // Set the instruction pointer to the Main block

// Function calling convention:
// [4 units] Return address
// [? units] Parameters
// [4 units] Return value

label Tmp1 // Utility location for loading words into an address
    raw 0
label Tmp2
    raw 0
label Tmp3
    raw 0

// Add a label for the stack and initialize the pointer
label Stack
    raw StackStart

label Main
    // Allocate 12 bytes on the stack

    set Tmp1 4

    // [0..4] Return address
    set Tmp2  Main.AfterCall
    iwm Stack Tmp2
    add Stack Tmp1

    // [4..8] Param 1 (number)
    set Tmp2 d15 // Fibonacci of 15
    iwm Stack Tmp2
    add Stack Tmp1

    // [8..12] Return value
    set Tmp2 0
    iwm Stack Tmp2
    add Stack Tmp1

    // Jump to the call
    set 0 Fib
    
    // After the call...
label Main.AfterCall

    // Reset the stack pointer
    set Tmp1 xfffffff4
    add Stack Tmp1

    // Get the return value
    set Tmp1 8
    add Tmp1 Stack

    // Extract the number
    irm Tmp2 Tmp1

    // Output the number directly, since I can't be bothered to print it
    sys Tmp2 Tmp2

    end

label Fib
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    set Tmp2 xfffffff8
    add Tmp1 Tmp2
    irm Tmp2 Tmp1

    // Call Check with Tmp2
    mov Check.Value Tmp2
    set Check.ReturnAddr Fib.AfterFirstCall
    set 0 Check
label Fib.AfterFirstCall

    // Convert Tmp1 from 0 or 1 to all zeroes or all ones
    mov Tmp1 Check.Return
    set Tmp2 xffffffff
    add Tmp1 Tmp2
    not Tmp1 Tmp1
    
    // Jump to (Fib.One1 & Tmp1) + (Fib.Zero1 & ~Tmp1)

    // Tmp2 = Fib.One1 & Tmp1
    set Tmp2 Fib.One1
    and Tmp2 Tmp1

    // Tmp3 = Fib.Zero1 & ~Tmp1
    set Tmp3 Fib.Zero1
    not Tmp1 Tmp1
    and Tmp3 Tmp1

    // Tmp1 = Tmp2 + Tmp3
    mov Tmp1 Tmp2
    add Tmp1 Tmp3

    mov 0 Tmp1

label Fib.Zero1
    // mem[stack-4] = 0
    mov Tmp1 Stack
    set Tmp2 xfffffffc
    add Tmp1 Tmp2

    set Tmp2 0
    iwm Tmp1 Tmp2
    
    // Jump to mem[stack-12]
    mov Tmp1 Stack
    set Tmp2 fffffff4
    add Tmp1 Tmp2

    irm 0 Tmp1

label Fib.One1
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    set Tmp2 xfffffff8
    add Tmp1 Tmp2
    irm Tmp2 Tmp1

    // Since we're checking if it equals 1, subtract one (add xffffffff)
    set Tmp1 xffffffff
    add Tmp2 Tmp1

    // Call Check with Tmp2
    mov Check.Value Tmp2
    set Check.ReturnAddr Fib.AfterSecondCall
    set 0 Check

label Fib.AfterSecondCall

    // Convert Tmp1 from 0 or 1 to all zeroes or all ones
    mov Tmp1 Check.Return
    set Tmp2 xffffffff
    add Tmp1 Tmp2
    not Tmp1 Tmp1

    // Jump to (Fib.One2 & Tmp1) + (Fib.Zero2 & ~Tmp1)

    // Tmp2 = Fib.One2 & Tmp1
    set Tmp2 Fib.One2
    and Tmp2 Tmp1

    // Tmp3 = Fib.Zero2 & ~Tmp1
    set Tmp3 Fib.Zero2
    not Tmp1 Tmp1
    and Tmp3 Tmp1

    // Tmp1 = Tmp2 + Tmp3
    mov Tmp1 Tmp2
    add Tmp1 Tmp3

    mov 0 Tmp1

label Fib.Zero2
    // mem[stack-4] = 1
    mov Tmp1 Stack
    set Tmp2 xfffffffc
    add Tmp1 Tmp2

    set Tmp2 1
    iwm Tmp1 Tmp2
    
    // Jump to mem[stack-12]
    mov Tmp1 Stack
    set Tmp2 fffffff4
    add Tmp1 Tmp2

    irm 0 Tmp1

label Fib.One2

    // Allocate and zero 4 bytes on the stack
    set Tmp1 0
    iwm Stack Tmp1
    set Tmp1 4
    add Stack Tmp1

    // Return Fib(n-1) + Fib(n-2)

    set Tmp1 4

    // [0..4] Return address
    set Tmp2  Fib.AfterCall1
    iwm Stack Tmp2
    add Stack Tmp1

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    set Tmp2 xfffffff0
    add Tmp1 Tmp2
    irm Tmp2 Tmp1

    set Tmp3 ffffffff
    add Tmp2 Tmp3

    iwm Stack Tmp2
    set Tmp1 4
    add Stack Tmp1

    // [8..12] Return value
    set Tmp2 0
    iwm Stack Tmp2
    add Stack Tmp1

    // Jump to the call
    set 0 Fib
    
    // After the call...
label Fib.AfterCall1

    // Reset the stack pointer
    set Tmp1 xfffffff4
    add Stack Tmp1

    // Get the return value
    set Tmp1 8
    add Tmp1 Stack

    // Extract the number into Tmp2
    irm Tmp2 Tmp1

    // mem[stack-4] = Tmp2
    mov Tmp1 Stack
    set Tmp3 xfffffffc
    add Tmp1 Tmp3
    iwm Tmp1 Tmp2

    set Tmp1 4

    // [0..4] Return address
    set Tmp2  Fib.AfterCall2
    iwm Stack Tmp2
    add Stack Tmp1

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    set Tmp2 xfffffff0
    add Tmp1 Tmp2
    irm Tmp2 Tmp1

    set Tmp3 fffffffe
    add Tmp2 Tmp3

    iwm Stack Tmp2
    set Tmp1 4
    add Stack Tmp1

    // [8..12] Return value
    set Tmp2 0
    iwm Stack Tmp2
    add Stack Tmp1

    // Jump to the call
    set 0 Fib
    
    // After the call...
label Fib.AfterCall2

    // Reset the stack pointer
    set Tmp1 xfffffff4
    add Stack Tmp1

    // Get the return value
    set Tmp1 8
    add Tmp1 Stack

    // Extract the number into Tmp2
    irm Tmp2 Tmp1

    // mem[stack-4] += Tmp2
    mov Tmp1 Stack
    set Tmp3 xfffffffc
    add Tmp1 Tmp3

    irm Tmp3 Tmp1
    add Tmp2 Tmp3
    iwm Tmp1 Tmp2

    // Reset the stack pointer
    set Tmp1 xfffffffc
    add Stack Tmp1

    // mem[stack-4] = Tmp2
    mov Tmp1 Stack
    set Tmp3 xfffffffc
    add Tmp1 Tmp3
    iwm Tmp1 Tmp2

    // Jump to mem[stack-12]
    mov Tmp1 Stack
    set Tmp2 fffffff4
    add Tmp1 Tmp2

    irm 0 Tmp1

    end

// We can avoid using the stack for functions that are never called twice at the
// same time. Since the CPU is singlethreaded (for now), we can take advantage
// of this when we're calling non-stack functions that we know never use direct
// or indirect recursion.
//
// An optimizer can optimize functions into doing this, but this is not a
// particularly new invention as you can already analyze the call graph of a
// program to determine the maximum stack size, and then you can (potentially)
// replace the return instructions with the definite return address.

label Check
    // Restrict the value to one byte
    set Tmp2 x000000ff
    and Check.Value Tmp2

    mov Tmp1 Check.Value // Copy the value once (1x)
    add Tmp1 Tmp1        // Double it (2x)
    add Tmp1 Tmp1        // Double it (4x)
    add Tmp1 Tmp1        // Double it (8x)
    add Tmp1 Check.Value // Add x     (9x)

    add 0 Tmp1 // Skip ahead!

    set 0 Check.Zero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero
    set 0 Check.NonZero

label Check.Zero
    // Return 0
    set Check.Return 0
    mov 0 Check.ReturnAddr

label Check.NonZero
    // Return 1
    set Check.Return 1
    mov 0 Check.ReturnAddr

label Check.Value
    raw 0 // The parameter

label Check.Return
    raw 0 // The return value

label Check.ReturnAddr
    raw 0 // The return value

// Start the stack right after the code ends
label StackStart

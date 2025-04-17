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

    // [0..4] Return address
    mov20 Stack Main.AfterCall
    add10 Stack 4

    // [4..8] Param 1 (number)
    mov20 Stack d40 // Fibonacci of 15
    add10 Stack 4

    // [8..12] Return value
    mov20 Stack 0
    add10 Stack 4

    // Jump to the call
    mov10 0 Fib
    
    // After the call...
label Main.AfterCall

    // Reset the stack pointer
    sub10 Stack d12

    // Set Tmp2 to the return value
    mov11 Tmp1 Stack
    add10 Tmp1 8
    mov12 Tmp2 Tmp1

    // Output the number directly, since I can't be bothered to print it
    sys Tmp2

    end

label Fib
    // Retrieve the parameter from the stack and put it in Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 8
    mov12 Tmp2 Tmp1

    // Jump to Fib.One1 if it's not 1
    jnz Tmp2 Fib.One1

    // If not...
    // mem[stack-4] = 0
    mov11 Tmp1 Stack
    sub10 Tmp1 4

    mov20 Tmp1 0
    
    // Jump to mem[stack-12]
    mov11 Tmp1 Stack
    sub10 Tmp1 d12
    mov12 0 Tmp1

label Fib.One1
    // Retrieve the parameter from the stack and put it in Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 8
    mov12 Tmp2 Tmp1

    // Since we're checking if it equals 1, subtract one
    sub10 Tmp2 1
    jnz Tmp2 Fib.One2

    // If not...
    // mem[stack-4] = 1
    mov11 Tmp1 Stack
    sub10 Tmp1 4

    // Set the result to 1
    mov20 Tmp1 1
    
    // Jump to mem[stack-12]
    mov11 Tmp1 Stack
    sub10 Tmp1 d12
    mov12 0 Tmp1

label Fib.One2

    // Allocate and zero 4 bytes on the stack
    mov20 Stack 0
    add10 Stack 4

    // Return Fib(n-1) + Fib(n-2)

    // [0..4] Return address
    mov10 Tmp2 Fib.AfterCall1
    mov21 Stack Tmp2
    add10 Stack 4

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 d16
    mov12 Tmp2 Tmp1

    sub10 Tmp2 1

    mov21 Stack Tmp2
    add10 Stack 4

    // [8..12] Return value
    mov20 Stack 0
    add10 Stack 4

    // Jump to the call
    mov10 0 Fib
    
    // After the call...
label Fib.AfterCall1

    // Reset the stack pointer
    sub10 Stack d12

    // Get the return value
    mov10 Tmp1 8
    add11 Tmp1 Stack

    // Extract the number into Tmp2
    mov12 Tmp2 Tmp1

    // mem[stack-4] = Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 4
    mov21 Tmp1 Tmp2

    // [0..4] Return address
    mov10 Tmp2  Fib.AfterCall2
    mov21 Stack Tmp2
    add10 Stack 4

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 d16
    mov12 Tmp2 Tmp1

    sub10 Tmp2 2

    mov21 Stack Tmp2
    add10 Stack 4

    // [8..12] Return value
    mov20 Stack 0
    add10 Stack 4

    // Jump to the call
    mov10 0 Fib
    
    // After the call...
label Fib.AfterCall2

    // Reset the stack pointer
    sub10 Stack d12

    // Set Tmp2 to the return value
    mov11 Tmp1 Stack
    add10 Tmp1 8
    mov12 Tmp2 Tmp1

    // mem[stack-4] += Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 4

    mov12 Tmp3 Tmp1
    add11 Tmp2 Tmp3
    mov21 Tmp1 Tmp2

    // Reset the stack pointer
    sub10 Stack 4

    // mem[stack-4] = Tmp2
    mov11 Tmp1 Stack
    sub10 Tmp1 4
    mov21 Tmp1 Tmp2

    // Jump to mem[stack-12]
    mov11 Tmp1 Stack
    sub10 Tmp1 d12

    mov12 0 Tmp1

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

// Start the stack right after the code ends
label StackStart

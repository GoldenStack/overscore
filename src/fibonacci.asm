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
    iwmi Stack Main.AfterCall
    addi Stack 4

    // [4..8] Param 1 (number)
    iwmi Stack d30 // Fibonacci of 15
    addi Stack 4

    // [8..12] Return value
    iwmi Stack 0
    addi Stack 4

    // Jump to the call
    set 0 Fib
    
    // After the call...
label Main.AfterCall

    // Reset the stack pointer
    subi Stack d12

    // Set Tmp2 to the return value
    mov Tmp1 Stack
    addi Tmp1 8
    irm Tmp2 Tmp1

    // Output the number directly, since I can't be bothered to print it
    sys Tmp2 Tmp2

    end

label Fib
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    subi Tmp1 8
    irm Tmp2 Tmp1

    // Jump to Fib.One1 if it's not 1
    jnz Tmp2 Fib.One1

    // If not...
    // mem[stack-4] = 0
    mov Tmp1 Stack
    subi Tmp1 4

    iwmi Tmp1 0
    
    // Jump to mem[stack-12]
    mov Tmp1 Stack
    subi Tmp1 d12
    irm 0 Tmp1

label Fib.One1
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    subi Tmp1 8
    irm Tmp2 Tmp1

    // Since we're checking if it equals 1, subtract one
    subi Tmp2 1
    jnz Tmp2 Fib.One2

    // If not...
    // mem[stack-4] = 1
    mov Tmp1 Stack
    subi Tmp1 4

    // Set the result to 1
    iwmi Tmp1 1
    
    // Jump to mem[stack-12]
    mov Tmp1 Stack
    subi Tmp1 d12
    irm 0 Tmp1

label Fib.One2

    // Allocate and zero 4 bytes on the stack
    iwmi Stack 0
    addi Stack 4

    // Return Fib(n-1) + Fib(n-2)

    // [0..4] Return address
    set Tmp2  Fib.AfterCall1
    iwm Stack Tmp2
    addi Stack 4

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    subi Tmp1 d16
    irm Tmp2 Tmp1

    subi Tmp2 1

    iwm Stack Tmp2
    addi Stack 4

    // [8..12] Return value
    iwmi Stack 0
    addi Stack 4

    // Jump to the call
    set 0 Fib
    
    // After the call...
label Fib.AfterCall1

    // Reset the stack pointer
    subi Stack d12

    // Get the return value
    set Tmp1 8
    add Tmp1 Stack

    // Extract the number into Tmp2
    irm Tmp2 Tmp1

    // mem[stack-4] = Tmp2
    mov Tmp1 Stack
    subi Tmp1 4
    iwm Tmp1 Tmp2

    // [0..4] Return address
    set Tmp2  Fib.AfterCall2
    iwm Stack Tmp2
    addi Stack 4

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov Tmp1 Stack
    subi Tmp1 d16
    irm Tmp2 Tmp1

    subi Tmp2 2

    iwm Stack Tmp2
    addi Stack 4

    // [8..12] Return value
    iwmi Stack 0
    addi Stack 4

    // Jump to the call
    set 0 Fib
    
    // After the call...
label Fib.AfterCall2

    // Reset the stack pointer
    subi Stack d12

    // Set Tmp2 to the return value
    mov Tmp1 Stack
    addi Tmp1 8
    irm Tmp2 Tmp1

    // mem[stack-4] += Tmp2
    mov Tmp1 Stack
    subi Tmp1 4

    irm Tmp3 Tmp1
    add Tmp2 Tmp3
    iwm Tmp1 Tmp2

    // Reset the stack pointer
    subi Stack 4

    // mem[stack-4] = Tmp2
    mov Tmp1 Stack
    subi Tmp1 4
    iwm Tmp1 Tmp2

    // Jump to mem[stack-12]
    mov Tmp1 Stack
    subi Tmp1 d12

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

// Start the stack right after the code ends
label StackStart

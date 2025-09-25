word Main // Set the instruction pointer to the Main block

// Function calling convention:
// [4 units] Return address
// [? units] Parameters
// [4 units] Return value

label Tmp1: // Utility location for loading words into an address
    word #0
label Tmp2:
    word #0
label Tmp3:
    word #0

// Add a label for the stack and initialize the pointer
label Stack:
    word StackStart

label Main:
    // Allocate 12 bytes on the stack

    // [0..4] Return address
    mov [[Stack]] Main.AfterCall
    add [Stack] #4

    // [4..8] Param 1 (number)
    mov [[Stack]] #30d // The number to calculate fibonacci of
    add [Stack] #8     // Add 4, and 4 extra as the return value is undefined

    // Jump to the call
    mov [#0] Fib

    // After the call...
label Main.AfterCall:

    // Reset the stack pointer
    sub [Stack] #C

    // Set Tmp2 to the return value
    mov [Tmp1] [Stack]
    add [Tmp1] #8
    mov [Tmp2] [[Tmp1]]

    // Output the number directly, since I can't be bothered to print it
    sys [Tmp2]

    end

// fib = fn(word) -> word
label Fib:
    // [Tmp2] = [[Stack]-8]
    mov [Tmp1] [Stack]
    sub [Tmp1] #8
    mov [Tmp2] [[Tmp1]]

    // if ([Tmp2] != 0) goto Fib.Branch1
    jnz [Tmp2] Fib.Branch1

    // If not...
    // mem[stack-4] = 0
    mov [Tmp1] [Stack]
    sub [Tmp1] #4
    mov [[Tmp1]] #0

    // Jump to mem[stack-12]
    mov [Tmp1] [Stack]
    sub [Tmp1] #C
    mov [#0] [[Tmp1]]

label Fib.Branch1:
    // [Tmp2] = [[Stack]-8] (the parameter)
    mov [Tmp1] [Stack]
    sub [Tmp1] #8
    mov [Tmp2] [[Tmp1]]

    // if ([Tmp2]-1 != 0) goto Fib.Branch2
    sub [Tmp2] #1
    jnz [Tmp2] Fib.Branch2

    // [Stack-4] = 1
    mov [Tmp1] [Stack]
    sub [Tmp1] #4
    mov [[Tmp1]] #1

    // goto [[Stack]-12]
    mov [Tmp1] [Stack]
    sub [Tmp1] #C
    mov [#0] [[Tmp1]]

label Fib.Branch2:

    // Allocate and zero 4 bytes on the stack for the return value
    mov [[Stack]] #0
    add [Stack] #4

    // Return Fib(n-1) + Fib(n-2)

    // [0..4] Return address
    mov [[Stack]] Fib.AfterCall1
    add [Stack] #4

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov [Tmp1] [Stack]
    sub [Tmp1] #10x
    mov [Tmp2] [[Tmp1]]

    sub [Tmp2] #1

    mov [[Stack]] [Tmp2]
    add [Stack] #8 // Add 4, and 4 extra as the return value is undefined

    // Jump to the call
    mov [#0] Fib

    // After the call...
label Fib.AfterCall1:

    // Reset the stack pointer
    sub [Stack] #C

    // Get the return value
    mov [Tmp1] #8
    add [Tmp1] [Stack]

    // Extract the number into Tmp2
    mov [Tmp2] [[Tmp1]]

    // mem[stack-4] = Tmp2
    mov [Tmp1] [Stack]
    sub [Tmp1] #4
    mov [[Tmp1]] [Tmp2]

    // [0..4] Return address
    mov [Tmp2] Fib.AfterCall2
    mov [[Stack]] [Tmp2]
    add [Stack] #4

    // [4..8] Param 1 (number)
    // Retrieve the parameter from the stack and put it in Tmp2
    mov [Tmp1] [Stack]
    sub [Tmp1] #10x
    mov [Tmp2] [[Tmp1]]

    sub [Tmp2] #2

    mov [[Stack]] [Tmp2]
    add [Stack] #8 // Add 4, and 4 extra as the return value is undefined

    // Jump to the call
    mov [#0] Fib

    // After the call...
label Fib.AfterCall2:

    // Reset the stack pointer
    sub [Stack] #C

    // [Tmp2] = [[Stack]-8]
    mov [Tmp1] [Stack]
    add [Tmp1] #8
    mov [Tmp2] [[Tmp1]]

    // [[Stack]-4] += [Tmp2]
    mov [Tmp1] [Stack]
    sub [Tmp1] #4
    mov [Tmp3] [[Tmp1]]

    add [Tmp2] [Tmp3]
    mov [[Tmp1]] [Tmp2]

    // Reset the stack pointer
    sub [Stack] #4

    // [[Stack]-4] = [Tmp2]
    mov [Tmp1] [Stack]
    sub [Tmp1] #4
    mov [[Tmp1]] [Tmp2]

    // goto [[Stack]-12]
    mov [Tmp1] [Stack]
    sub [Tmp1] #C

    mov [#0] [[Tmp1]]

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
label StackStart:

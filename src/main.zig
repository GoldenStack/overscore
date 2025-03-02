const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");

pub fn main() !void {
    // Create an allocator and error if it leaks
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) @panic("Leaked memory!");
    }

    // Load the assembly and convert it to a slice
    const assembly = @embedFile("example.asm");
    const asm_slice: []const u8 = assembly[0..assembly.len];

    // Assemble the assembly into binary
    const binary = try Assembler.assemble(gpa.allocator(), asm_slice);
    defer binary.deinit();

    // Display the assembled binary
    std.debug.print("Assembled binary: {any}\n", .{binary.items});
    
    // Create a CPU and load the binary into memory
    var cpu = Cpu.init();
    @memcpy(cpu.memory[0..binary.items.len], binary.items);

    // Show memory before
    std.debug.print("Before: {any}\n", .{cpu.memory});

    // Keep running instructions while they can be read
    while (try cpu.prepare_instruction()) |instr| {
        std.debug.print("{any}\n", .{instr});
        cpu.follow(instr);
    }

    // Show memory after
    std.debug.print("After:  {any}\n", .{cpu.memory});
    
}

const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) @panic("Leaked memory!");
    }

    const assembly = @embedFile("example.asm");
    const asm2: []const u8 = assembly[0..assembly.len];

    const code = try Assembler.assemble(gpa.allocator(), asm2);
    defer code.deinit();

    std.debug.print("Assembled code: {any}\n", .{code.items});
    
    var cpu = Cpu.init();

    var working_memory = [_]Cpu.Unit{0} ** 20;
    working_memory[0] = 20;

    @memcpy(cpu.memory[0..working_memory.len], &working_memory);
    @memcpy(cpu.memory[working_memory.len..working_memory.len+code.items.len], code.items);

    std.debug.print("Before: {any}\n", .{cpu.memory});

    try cpu.loop();

    std.debug.print("After:  {any}\n", .{cpu.memory});
    
}

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

    const binary = try Assembler.assemble(gpa.allocator(), asm2);
    defer binary.deinit();

    std.debug.print("Assembled binary: {any}\n", .{binary.items});
    
    var cpu = Cpu.init();

    @memcpy(cpu.memory[0..binary.items.len], binary.items);

    std.debug.print("Before: {any}\n", .{cpu.memory});

    while (try cpu.prepare_instruction()) |instr| {
        std.debug.print("{any}\n", .{instr});
        cpu.follow(instr);
    }

    std.debug.print("After:  {any}\n", .{cpu.memory});
    
}

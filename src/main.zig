const std = @import("std");
const Cpu = @import("Cpu.zig");

pub fn main() !void {
    var cpu = Cpu.init();

    var working_memory = [_]Cpu.Unit{0} ** 20;
    working_memory[0] = 20;

    const code = [_]Cpu.Unit{
        1, 170, 0, 0, 0, 4, 0, 0, 0,
        3, 4, 0, 0, 0, 8, 0, 0, 0,
        4, 4, 0, 0, 0, 8, 0, 0, 0, 4, 0, 0, 0,
    };

    const memory = working_memory ++ code;
    const rawmem = cpu.memory[0..memory.len];
    @memcpy(rawmem, &memory);

    std.debug.print("{any}\n", .{cpu});

    try cpu.loop();

    std.debug.print("{any}\n", .{cpu});
    
}

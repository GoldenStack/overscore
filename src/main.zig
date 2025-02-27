const std = @import("std");
const Cpu = @import("cpu/Cpu.zig");

pub fn main() !void {
    var cpu = Cpu.init();

    cpu.follow(Cpu.Instruction{ .set = .{
        .reg = 2,
        .word = 500,
    } });

    std.debug.print("{any}\n", .{cpu});
}

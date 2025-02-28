const std = @import("std");
const Cpu = @import("Cpu.zig");

pub fn main() !void {
    var cpu = Cpu.init();

    cpu.follow(Cpu.Instruction{ .set = .{
        .addr = 0,
        .value = 0b10101010,
    } });

    std.debug.print("{any}\n", .{cpu});

    cpu.follow(Cpu.Instruction{ .not = .{
        .read = 0,
        .write = 4,
    } });

    std.debug.print("{any}\n", .{cpu});

    cpu.follow(Cpu.Instruction{ .@"and" = .{
        .read1 = 0,
        .read2 = 4,
        .write = 0,
    } });

    std.debug.print("{any}\n", .{cpu});
    
}

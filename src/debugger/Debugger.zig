const std = @import("std");
const Cpu = @import("../Cpu.zig");

cpu: *Cpu,

pub fn init(cpu: *Cpu) @This() {
    return .{
        .cpu = cpu,
    };
}

pub fn handle(self: *@This(), line: []const u8, out: *std.io.Writer) !?void {
    if (std.mem.eql(u8, line, "exit")) {
        try out.print("(odb) exiting\n", .{});
        return null;
    } else if (std.mem.eql(u8, line, "step")) {
        const instruction = try self.cpu.prepareInstruction() orelse {
            try out.print("(odb) no more instructions to run\n", .{});
            return;
        };

        try out.print("(odb) ran {any}\n", .{instruction});

        try self.cpu.follow(instruction);
    } else {
        try out.print("(odb) found {} chars\n", .{line.len});
    }
}

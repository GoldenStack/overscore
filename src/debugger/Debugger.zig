const std = @import("std");
const Cpu = @import("../Cpu.zig");
const command = @import("command.zig");

cpu: *Cpu,
ic: u64,

pub fn init(cpu: *Cpu) @This() {
    return .{
        .cpu = cpu,
        .ic = 0,
    };
}

fn help(self: *@This(), out: *std.io.Writer) !void {
    _ = self;

    try out.writeAll(
        \\overscore debugger help
        \\
        \\OPTIONS:
        \\  help     display this message
        \\  exit     exit the debugger
        \\  step     step a single instruction
        \\  count    view the number of instructions executed
        \\
    );
}

pub fn handle(self: *@This(), line: []const u8, out: *std.io.Writer) !?void {
    var tokenizer = command.Tokenizer.init(line);

    const cmd = try command.Command.read(&tokenizer);

    switch (cmd) {
        .exit => return null,
        .help => return try self.help(out),
        .step => {
            if (try self.cpu.prepareInstruction()) |instr| {
                try self.cpu.follow(instr);
                self.ic += 1;
            } else {
                try out.writeAll("(odb) reached ending instruction, stopped execution\n");
            }
        },
        .count => {
            try out.print("(odb) instruction counter is at {}\n", .{self.ic});
        },
    }
}

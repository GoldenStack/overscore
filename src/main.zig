const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");
const Tokenizer = @import("language/Tokenizer.zig");
const Parser = @import("language/Parser.zig");

pub fn main() !void {
    // Create an allocator and error if it leaks
    // var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    // defer _ = gpa.deinit();
    // const allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src_bytes = @embedFile("example.os");
    const src: []const u8 = src_bytes;

    const tokens = Tokenizer.tokenize(src);
    var parser = Parser.init(tokens, allocator);

    const container = parser.read_container() catch |err| {
        if (err == error.ParsingError) {
            std.debug.print("Error: {any} at row {any} col {any}\n", .{ parser.error_context, parser.tokens.row, parser.tokens.col });
            return;
        } else return err;
    };

    std.debug.print("container: {any}\n", .{container});

    // // Load the assembly and convert it to a slice
    // const assembly = @embedFile("fibonacci.asm");
    // const asm_slice: []const u8 = assembly[0..assembly.len];

    // // Assemble the assembly into binary
    // var binary = std.ArrayList(Cpu.Unit).init(allocator);
    // try Assembler.assemble(asm_slice, allocator, binary.writer());
    // defer binary.deinit();

    // // Display the assembled binary
    // std.debug.print("Assembled binary: {any}\n", .{binary.items});

    // // Create a CPU and load the binary into memory
    // var cpu = Cpu.init(sys);
    // @memcpy(cpu.memory[0..binary.items.len], binary.items);

    // // Keep running instructions while they can be read
    // while (try cpu.prepareInstruction()) |instr| {
    //     // std.debug.print("{any}\n", .{instr});
    //     try cpu.follow(instr);
    // }
}

fn sys(in: Cpu.Word) Cpu.Word {
    var bytes = [_]u8{0} ** 4;
    std.mem.writeInt(Cpu.Word, &bytes, in, .little);

    return switch (bytes[3]) {
        0 => std.io.getStdIn().reader().readByte() catch 0,
        1 => if (std.io.getStdOut().writer().writeByte(bytes[0])) 1 else |_| 0,
        else => 0,
    };
}

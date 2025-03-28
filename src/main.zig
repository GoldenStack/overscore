const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");
const tokenizer = @import("language/tokenizer.zig");
const Parser = @import("language/Parser.zig");
const Compiler = @import("language/Compiler.zig");

pub fn main() !void {
    // Create an allocator and error if it leaks
    // var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    // defer _ = gpa.deinit();
    // const allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = @embedFile("example.os");

    const tokens = tokenizer.Tokenizer.init(src);
    var parser = Parser.init(tokens, allocator);

    const container = parser.read_root() catch |err| {
        const loc = parser.tokens.loc;

        if (err == error.ParsingError) {
            std.debug.print("Error at {}: {any}\n", .{ loc, parser.error_context });

            // Find the current line
            const lineStart = if (std.mem.lastIndexOfScalar(u8, src[0..loc.pos], '\n')) |idx| idx + 1 else 0;
            const lineEnd = std.mem.indexOfScalarPos(u8, src, loc.pos, '\n') orelse src.len;

            const currentLine = src[lineStart..lineEnd];

            std.debug.print("> {s}\n", .{currentLine});
            const nextlen = parser.peek().value.len;
            for (0..2 + loc.col - 1) |_| std.debug.print(" ", .{});
            for (0..nextlen) |_| std.debug.print("^", .{});
            std.debug.print("\n", .{});

            return;
        } else return err;
    };

    std.debug.print("{any}\n", .{container});

    // Compile it!
    var compiler = Compiler.init(allocator);
    const compiled = compiler.compile(container) catch |err| {
        if (err == error.CompilerError) {
            std.debug.print("Error: {any}\n", .{compiler.error_context});
        } else return err;
    };

    std.debug.print("{any}\n", .{compiled});

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

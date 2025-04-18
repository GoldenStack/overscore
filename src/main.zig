const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");
const tokenizer = @import("language/tokenizer.zig");
const Parser = @import("language/Parser.zig");
const Ir = @import("language/Ir.zig");
const Interpreter = @import("language/Interpreter.zig");

pub fn main() !void {
    // const stdout = std.io.getStdOut().writer();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // const src = @embedFile("example2.os");

    // const tokens = tokenizer.Tokenizer.init(src);
    // var parser = Parser.init(allocator, tokens);

    // const container = parser.readRoot() catch |err| {
    //     if (err == error.SyntaxError) {
    //         try parser.error_context.?.display("example2.os", src, stdout);
    //         return;
    //     } else return err;
    // };

    // try Parser.ast.printContainer(src, container, stdout);
    // try stdout.writeByte('\n');

    // var ir = Ir.init(allocator, src);
    // const container_index = ir.convertContainer(container) catch |err| {
    //     if (err == error.IrError) {
    //         try ir.error_context.?.display("example2.os", src, stdout);
    //         return;
    //     } else return err;
    // };

    // try ir.printContainer(container_index, stdout);
    // try stdout.writeByte('\n');

    // var interpreter = Interpreter.init(allocator, src, ir);
    // const result = interpreter.evalMain(container_index) catch |err| {
    //     if (err == error.InterpreterError) {
    //         try interpreter.error_context.?.display("example2.os", src, stdout);
    //         return;
    //     } else return err;
    // };

    // try ir.printExpr(result, stdout);
    // try stdout.writeByte('\n');

    // Load the assembly and convert it to a slice
    const assembly = @embedFile("fibonacci.asm");
    const asm_slice: []const u8 = assembly[0..assembly.len];

    // Assemble the assembly into binary
    var binary = std.ArrayList(Cpu.Unit).init(allocator);
    try Assembler.assemble(asm_slice, allocator, binary.writer());
    defer binary.deinit();

    // Display the assembled binary
    // std.debug.print("Assembled binary: {any}\n", .{binary.items});

    // Create a CPU and load the binary into memory
    var cpu = Cpu.init(sys);
    @memcpy(cpu.memory[0..binary.items.len], binary.items);

    var counter: usize = 0;

    // Keep running instructions while they can be read
    while (try cpu.prepareInstruction()) |instr| {
        // std.debug.print("{any}\n", .{instr});
        counter += 1;
        try cpu.follow(instr);
    }

    std.debug.print("instruction count: {}\n", .{counter});
}

fn sys(in: Cpu.Word) Cpu.Word {
    var bytes = [_]u8{0} ** 4;
    std.mem.writeInt(Cpu.Word, &bytes, in, .little);

    std.debug.print("{any}\n", .{in});
    return 0;

    // return switch (bytes[3]) {
    //     0 => std.io.getStdIn().reader().readByte() catch 0,
    //     1 => if (std.io.getStdOut().writer().writeByte(bytes[0])) 1 else |_| 0,
    //     else => 0,
    // };
}

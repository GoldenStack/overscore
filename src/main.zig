const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");
const tokenizer = @import("language/tokenizer.zig");
const Parser = @import("language/Parser.zig");
const Ir = @import("language/Ir.zig");
const Interpreter = @import("language/Interpreter.zig");

const file = "example2.os";
const src = @embedFile(file);

pub fn main() !void {
    // Create memory and stdout
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const stdout = std.io.getStdOut().writer();

    // Parse the tokens into an AST
    const tokens = tokenizer.Tokenizer.init(src);
    var parser = Parser.init(allocator, tokens);
    const container = parser.readRoot() catch |err| return handle_error(err, parser);

    // Print the AST
    try Parser.ast.printExpr(src, container.value, stdout);
    try stdout.writeByte('\n');

    // Parse the tokens into IR
    var ir = Ir.init(allocator, src);
    const container_index = ir.convertExpr(container) catch |err| return handle_error(err, &ir);

    // Print the IR
    try ir.printExpr(container_index, stdout);
    try stdout.writeByte('\n');

    // Evaluate the 'main' variable in the IR
    var interpreter = Interpreter.init(allocator, src, &ir);
    const main_index = ir.indexGet(container_index).expr.container.defs.get("main") orelse @panic("No main expression found!");
    _ = interpreter.typeOf(main_index.index) catch |err| return handle_error(err, interpreter);
    // interpreter.evalDef(main_index) catch |err| return handle_error(err, interpreter);

    // for (ir.values.items) |*value| {
    //     if (value.type == null) continue;
    //     std.debug.print("{s} is ", .{value.expr_range.substr(src)});
    //     try ir.printExpr(value.type.?, stdout);
    //     std.debug.print("\n", .{});
    // }

    // Print the output IR
    try ir.printExpr(try interpreter.softEval(ir.indexOfGet(.def, main_index).value), stdout);
    std.debug.print("\n", .{});
    try ir.printExpr(main_index.index, stdout);
    try stdout.writeByte('\n');

    // // Load the assembly and convert it to a slice
    // const assembly = @embedFile("fibonacci.asm");
    // const asm_slice: []const u8 = assembly[0..assembly.len];

    // // Assemble the assembly into binary
    // var binary = std.ArrayList(Cpu.Unit).init(allocator);
    // try Assembler.assemble(asm_slice, allocator, binary.writer());
    // defer binary.deinit();

    // // Display the assembled binary
    // // std.debug.print("Assembled binary: {any}\n", .{binary.items});

    // // Create a CPU and load the binary into memory
    // var cpu = Cpu.init(sys);
    // @memcpy(cpu.memory[0..binary.items.len], binary.items);

    // var counter: usize = 0;

    // // Keep running instructions while they can be read
    // while (try cpu.prepareInstruction()) |instr| {
    //     // std.debug.print("{any}\n", .{instr});
    //     counter += 1;
    //     try cpu.follow(instr);
    // }

    // std.debug.print("instruction count: {}\n", .{counter});
}

fn handle_error(err: error{CodeError, OutOfMemory}, ctx: anytype) !void {
    if (err == error.CodeError) {
        const stdout = std.io.getStdOut().writer();
        try ctx.error_context.?.display(file, src, stdout);
        return;
    } else return err;
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

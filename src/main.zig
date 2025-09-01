const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("Assembler.zig");
const tokenizer = @import("language/tokenizer.zig");
const Parser = @import("language/Parser.zig");
const Ir = @import("language/Ir.zig");
const interpreter = @import("language/interpreter.zig");

const file = "example2.os";
const src = @embedFile(file);

pub fn main() !void {
    // Create memory and stdout
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try testCompile(allocator);

    try testAssembly(allocator);
}

fn testCompile(allocator: std.mem.Allocator) !void {
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
    const main_index = ir.at(.expr, container_index).container.defs.get("main") orelse @panic("No main expression found!");

    // Type check main and print
    const main_type = interpreter.typeOf(&ir, main_index.index) catch |err| return handle_error(err, ir);
    try ir.printExpr(main_type, stdout);
    std.debug.print("\n", .{});

    // Evaluate main. Note that this should not be done if compiling; this is the intrepretation process.
    const main_value = interpreter.eval(&ir, ir.atOf(.def, main_index).value) catch |err| return handle_error(err, &ir);

    // Print the output IR
    try ir.printExpr(main_value, stdout);
    std.debug.print("\n", .{});
}

fn testAssembly(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Load the assembly and convert it to a slice
    const assembly = @embedFile("fibonacci.asm");
    const asm_slice: []const u8 = assembly[0..assembly.len];
    var iter = Assembler.AssemblyIterator.init(asm_slice);

    // Assemble the assembly into binary
    var binary = std.ArrayList(Cpu.Unit).init(allocator);
    Assembler.assemble(&iter, allocator, binary.writer()) catch |err| {
        try stdout.print("Error {} on following line:\n{s}\n", .{ err, iter.maybe_error.? });
        return err;
    };
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

    try stdout.print("instruction count: {}\n", .{counter});
}

fn handle_error(err: error{ CodeError, OutOfMemory }, ctx: anytype) !void {
    if (err == error.CodeError) {
        const stdout = std.io.getStdOut().writer();
        try ctx.error_context.?.display(file, src, stdout);
        return err;
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

const std = @import("std");
const Cpu = @import("Cpu.zig");
const Assembler = @import("assembler/Assembler.zig");
const LanguageTokenizer = @import("language/tokenizer.zig").Tokenizer;
const AssemblyTokenizer = @import("assembler/tokenizer.zig").Tokenizer;
const Parser = @import("language/Parser.zig");
const Ir = @import("language/Ir.zig");
const interpreter = @import("language/interpreter.zig");

const cli = @import("cli");

var stdout_raw = std.fs.File.stdout().writer(&.{});
const stdout = &stdout_raw.interface;

// The global configuration structure.
var config = struct {
    file: []const u8 = undefined,
    debug: bool = false,
}{};

pub fn main() !void {
    var r = try cli.AppRunner.init(std.heap.page_allocator);

    const assembleCommand = cli.Command{
        .name = "assemble",
        .description = cli.Description{
            .one_line = "assembles file(s) into an output binary",
        },
        .options = try r.allocOptions(&.{
            .{
                .long_name = "debug",
                .help = "enable debug prints",
                .required = false,
                .value_ref = r.mkRef(&config.debug),
            },
        }),
        .target = cli.CommandTarget{
            .action = cli.CommandAction{
                .positional_args = cli.PositionalArgs{
                    .required = &.{
                        .{
                            .name = "file",
                            .help = "the file to assemble",
                            .value_ref = r.mkRef(&config.file),
                        }
                    },
                },
                .exec = assemble,
            },
        },
    };

    const emulateCommand = cli.Command{
        .name = "emulate",
        .description = cli.Description{
            .one_line = "emulates the given file from stdin",
        },
        .options = try r.allocOptions(&.{
            .{
                .long_name = "debug",
                .help = "enable debug prints",
                .required = false,
                .value_ref = r.mkRef(&config.debug),
            },
        }),
        .target = cli.CommandTarget{
            .action = cli.CommandAction{
                .exec = emulate,
            },
        },
    };

    const interpretCommand = cli.Command{
        .name = "interpret",
        .description = cli.Description{
            .one_line = "interprets the given source file",
        },
        .options = try r.allocOptions(&.{
            .{
                .long_name = "debug",
                .help = "enable debug prints",
                .required = false,
                .value_ref = r.mkRef(&config.debug),
            },
        }),
        .target = cli.CommandTarget{
            .action = cli.CommandAction{
                .positional_args = cli.PositionalArgs{
                    .required = &.{
                        .{
                            .name = "file",
                            .help = "the file to interpret",
                            .value_ref = r.mkRef(&config.file),
                        }
                    },
                },
                .exec = interpret,
            },
        },
    };

    const app = cli.App{
        .command = cli.Command{
            .name = "overscore",
            .options = try r.allocOptions(&.{}),
            .target = cli.CommandTarget{
                .subcommands = try r.allocCommands(&.{
                    assembleCommand,
                    emulateCommand,
                    interpretCommand,
                }),
            },
        },
    };
    return r.run(&app);
}

fn assemble() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = readFile(allocator, config.file) catch |err| return try readFileError(err, config.file);

    if (config.debug) {
        std.debug.print("{} bytes of source assembly loaded\n", .{src.len});
    }

    // Load the assembly and convert it to a slice
    const tokens = AssemblyTokenizer.init(src);
    var assembler = Assembler.init(allocator, tokens);

    // Assemble the assembly into binary
    var binary = std.ArrayList(Cpu.Unit).empty;
    assembler.assemble(binary.writer(allocator)) catch |err| return handleCodeError(err, config.file, &assembler);
    defer binary.deinit(allocator);

    try stdout.writeAll(binary.items);

    if (config.debug) {
        std.debug.print("{} bytes of binary output\n", .{binary.items.len});
    }
}

fn emulate() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Get a source
    var source = std.fs.File.stdin().reader(&.{});

    // Get a sink
    var sink_bytes = std.io.Writer.Allocating.init(allocator);
    const sink = &sink_bytes.writer;

    _ = try std.io.Reader.streamRemaining(&source.interface, sink);

    const binary = sink.buffer[0..sink.end];

    // Create a CPU and load the binary into memory
    var cpu = Cpu.init(sys);
    @memcpy(cpu.memory[0..binary.len], binary);

    var counter: usize = 0;

    // Keep running instructions while they can be read
    while (try cpu.prepareInstruction()) |instr| {
        // std.debug.print("{any}\n", .{instr});
        counter += 1;
        try cpu.follow(instr);
    }

    if (config.debug) {
        std.debug.print("{} instructions emulated\n", .{counter});
    }
}

fn interpret() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = readFile(allocator, config.file) catch |err| return try readFileError(err, config.file);

    // Parse the tokens into an AST
    const tokens = LanguageTokenizer.init(src);
    var parser = Parser.init(allocator, tokens);
    const container = parser.readRoot() catch |err| return handleCodeError(err, config.file, &parser);

    // Print the AST
    if (config.debug) {
        try stdout.writeAll("AST:\n");
        try Parser.ast.printExpr(src, container.value, stdout);
        try stdout.writeAll("\n\n");
    }

    // Parse the tokens into IR
    var ir = Ir.init(allocator, src);
    const container_index = ir.convertExpr(container) catch |err| return handleCodeError(err, config.file, &ir);

    // Print the IR
    if (config.debug) {
        try stdout.writeAll("IR:\n");
        try ir.printExpr(container_index, stdout);
        try stdout.writeAll("\n\n");
    }

    // Evaluate the 'main' variable in the IR
    const main_index = ir.at(.expr, container_index).container.defs.get("main") orelse @panic("No main expression found!");

    // Type check main
    const main_type = interpreter.typeOf(&ir, main_index.index) catch |err| return handleCodeError(err, config.file, &ir);

    // Print type of main
    if (config.debug) {
        try stdout.writeAll("Main type:\n");
        try ir.printExpr(main_type, stdout);
        try stdout.writeAll("\n\n");
    }

    // Evaluate main. Note that this should not be done if compiling; this is the intrepretation process.
    const main_value = interpreter.eval(&ir, ir.atOf(.def, main_index).value) catch |err| return handleCodeError(err, config.file, &ir);

    // Print the output IR
    try ir.printExpr(main_value, stdout);
    try stdout.writeAll("\n");
}

fn readFile(allocator: std.mem.Allocator, file: []const u8) ![:0]u8 {
    // Open the source file
    const source_file = try std.fs.cwd().openFile(file, .{});
    var source = source_file.reader(&.{});

    // Get a sink
    var sink_bytes = std.io.Writer.Allocating.init(allocator);
    const sink = &sink_bytes.writer;

    // Write everything, including a sentinel
    _ = try std.io.Reader.streamRemaining(&source.interface, sink);
    _ = try sink.writeByte(0);

    // Return the slice
    return sink.buffer[0..sink.end-1:0];
}

fn readFileError(err: anyerror, file: []const u8) !void {
    switch (err) {
        error.FileNotFound => try stdout.print("unknown file '{s}'\n", .{file}),
        else => try stdout.print("could not access file '{s}': {any}\n", .{file, err}),
    }

    if (config.debug) return err
    else return;
}

fn handleCodeError(err: anyerror, file: []const u8, ctx: anytype) !void {
    if (err == error.CodeError) {
        try ctx.error_context.?.display(file, ctx.src, stdout);
        if (!config.debug) return;
    }

    return err;
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

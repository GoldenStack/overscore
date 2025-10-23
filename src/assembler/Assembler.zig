const std = @import("std");
const Cpu = @import("../Cpu.zig");
const lex = @import("../lex.zig");
const err = @import("../err.zig");
const tokenizer = @import("tokenizer.zig");
const failure = @import("failure.zig");
const Ranged = lex.Ranged;
const Token = tokenizer.Token;
const Err = err.Err;

pub const WordValue = union(enum) {
    literal: Cpu.Word,
    label: lex.Range,
};

/// An address that can have any number of indirection values. Takes an enum.
pub fn Addr(Indir: type) type {
    return struct {
        pub const Indirection = Indir;

        indirection: Indir,
        value: WordValue,
    };
}

/// Generates an operator from a list of address types.
pub fn Op(args: anytype) type {
    var fields: [std.meta.fields(@TypeOf(args)).len]type = undefined;

    for (0.., args) |i, arg| {
        fields[i] = Ranged(Addr(arg));
    }

    return std.meta.Tuple(&fields);
}

const Out = enum(u8) { @"1" = 1 };
const In = enum(u8) { @"0", @"1" };

const Unary = Op(.{Out});
const Binary = Op(.{ Out, In });

const Instruction = union(enum) {
    // Unary operations
    not: Unary,
    sys: Unary,

    // Binary operations
    mov: Op(.{
        enum(u8) { @"1" = 1, @"2" },
        enum(u8) { @"0", @"1", @"2" },
    }), // mov is special
    @"and": Binary,
    @"or": Binary,
    add: Binary,
    sub: Binary,
    mul: Binary,
    jz: Binary,
    jnz: Binary,

    pub fn size(self: @This()) Cpu.Word {
        return switch (self) {
            inline else => |_, tag| 1 + Cpu.UnitsPerWord * std.meta.fields(std.meta.TagPayload(Instruction, tag)).len,
        };
    }

    pub fn opcode(self: @This()) Cpu.Opcode {
        // Incredibly janky lol. We get the opcode name and then comptime string
        // append the levels of indirection, but because the `inline else`
        // runtime-to-comptime trick doesn't work on tuples, we just handle
        // binary and unary manually.
        return switch (self) {
            inline else => |op, tag| switch (op.len) {
                1 => switch (op[0].value.indirection) {
                    inline else => |indir1| @field(Cpu.Opcode, @tagName(tag) ++ @tagName(indir1)),
                },
                2 => switch (op[0].value.indirection) {
                    inline else => |indir1| switch (op[1].value.indirection) {
                        inline else => |indir2| @field(Cpu.Opcode, @tagName(tag) ++ @tagName(indir1) ++ @tagName(indir2)),
                    },
                },
                else => unreachable,
            },
        };
    }
};

/// A line in assembly. This can be an instruction or one of many Assembler
/// constructs.
pub const Line = union(enum) {
    instruction: Instruction,

    word: WordValue, // A single CPU word
    bytes: []const u8, // A list of bytes, specified as digits or a string

    label: Ranged([]const u8), // The name of a label
    end,

    pub fn size(self: *const @This()) Cpu.Word {
        return switch (self.*) {
            .instruction => |instruction| instruction.size(),
            .word => Cpu.UnitsPerWord,
            .label => 0,
            .bytes => |line| @truncate(line.len),
            .end => 1,
        };
    }
};

// Standard fields
src: [:0]const u8,
allocator: std.mem.Allocator,
error_context: ?failure.Error = null,

tokens: tokenizer.Tokenizer,

pub fn init(allocator: std.mem.Allocator, tokens: tokenizer.Tokenizer) @This() {
    return .{
        .src = tokens.src,
        .tokens = tokens,
        .allocator = allocator,
    };
}

// Assembles a string of Overscore assembly language into a binary file for
// input into the CPU.
pub fn assemble(self: *@This(), writer: anytype) Err!void {
    var lines = std.ArrayList(Line).empty;
    var labels = std.StringHashMap(Cpu.Addr).init(self.allocator);
    defer lines.deinit(self.allocator);
    defer labels.deinit();

    // Read all the lines
    try self.readLines(&lines, &labels);

    // Now, write them
    for (lines.items) |line| try self.writeLine(line, &labels, writer);
}

fn writeLine(self: *@This(), line: Line, labels: *const std.StringHashMap(Cpu.Addr), writer: anytype) !void {
    try switch (line) {
        .instruction => |instruction| self.writeInstruction(instruction, labels, writer),
        .word => |word| try self.writeValue(word, labels, writer),
        .label => {},
        .bytes => |bytes| writer.writeAll(bytes),
        .end => writer.writeByte(0xff),
    };
}

fn writeInstruction(self: *@This(), instruction: Instruction, labels: *const std.StringHashMap(Cpu.Addr), writer: anytype) !void {
    try writer.writeInt(std.meta.Tag(Cpu.Opcode), @intFromEnum(instruction.opcode()), .little);

    switch (instruction) {
        inline else => |op| inline for (std.meta.fields(@TypeOf(op))) |field| {
            try self.writeValue(@field(op, field.name).value.value, labels, writer);
        },
    }
}

fn writeValue(self: *@This(), value: WordValue, labels: *const std.StringHashMap(Cpu.Addr), writer: anytype) !void {
    try writer.writeInt(Cpu.Word, switch (value) {
        .literal => |lit| lit,
        .label => |label| labels.get(label.substr(self.src)) orelse return self.fail(.{ .unknown_label = label }),
    }, .little);
}

// Parses lines from the given reader, writing the lines to the lines parameter
// and writing any parsed labels to the labels map.
pub fn readLines(self: *@This(), lines: *std.ArrayList(Line), labels: *std.StringHashMap(Cpu.Addr)) !void {
    var total_size: Cpu.Word = 0;

    while (self.tokens.peek().value != .eof) {
        const line = try self.readLine();

        // Find the value of the label, if possible
        switch (line) {
            .label => |label| try labels.put(label.range.substr(self.src), total_size),
            else => {},
        }

        // Add the size of the line
        total_size += line.size();

        try lines.append(self.allocator, line);
    }
}

pub fn readLine(self: *@This()) Err!Line {
    // Read all the newlines *here*. This is important since they're tokens
    // sometimes, so this can't just be done in the tokenizer uncontextually.
    while (self.tokens.peek().value == .newline) _ = try self.expect(.newline);

    const line_start = try self.expectMany(&.{ .not, .sys, .mov, .@"and", .@"or", .add, .sub, .mul, .jz, .jnz, .word, .bytes, .label, .end });

    const line: Line = line: switch (line_start.value) {
        // Parse all instructions
        inline .not, .sys, .mov, .@"and", .@"or", .add, .sub, .mul, .jz, .jnz => |tag| {
            break :line .{ .instruction = @unionInit(
                Instruction,
                @tagName(tag),
                try self.readOp(std.meta.TagPayload(Instruction, @field(
                    Instruction,
                    @tagName(tag),
                ))),
            ) };
        },

        .word => .{ .word = try self.readValue() },

        .bytes => {
            var bytes = std.ArrayList(Cpu.Unit).empty;
            while (self.tokens.peek().value == .number) {
                try bytes.append(self.allocator, try self.readNumber(Cpu.Unit));
            }

            break :line .{ .bytes = bytes.items };
        },
        .label => {
            const label = try self.expect(.ident);

            _ = try self.expect(.colon);

            break :line .{ .label = label.replace(label.range.substr(self.src)) };
        },
        .end => .end,

        else => unreachable,
    };

    _ = try self.expectMany(&.{ .eof, .newline });

    return line;
}

pub fn readOp(self: *@This(), Operation: type) Err!Operation {
    var op: Operation = undefined;

    inline for (std.meta.fields(Operation)) |field| {
        const addr = try self.readAddr(field.type.Child);

        @field(op, field.name) = addr;
    }

    return op;
}

pub fn readAddr(self: *@This(), Address: type) Err!Ranged(Address) {
    const addr = try lex.ranged(self, readRawAddr);

    const indirection: Address.Indirection = ind: inline for (std.meta.fields(Address.Indirection)) |field| {
        if (addr.value[0] == field.value) break :ind @enumFromInt(field.value);
    } else return self.fail(.{ .invalid_indirection = .{
        .argument = addr.range,
        .expected = extractValues(Address.Indirection),
        .found = addr.value[0],
    } });

    const new_addr: Address = .{
        .indirection = indirection,
        .value = addr.value[1],
    };

    return addr.replace(new_addr);
}

fn extractValues(Enum: type) []const @typeInfo(Enum).@"enum".tag_type {
    const const_values = comptime blk: {
        const fields = std.meta.fields(Enum);

        var values: [fields.len]@typeInfo(Enum).@"enum".tag_type = undefined;
        for (&values, fields) |*value, field| {
            value.* = @intCast(field.value);
        }

        break :blk values;
    };
    return &const_values;
}

fn readRawAddr(self: *@This()) Err!struct { usize, WordValue } {
    // Count the number of opening square brackets
    var opening: usize = 0;
    while (self.tokens.peek().value == .opening_square_bracket) {
        _ = try self.expect(.opening_square_bracket);
        opening += 1;
    }

    // Read a value
    const value = try self.readValue();

    // Close the square brackets
    for (0..opening) |_| _ = try self.expect(.closing_square_bracket);

    return .{ opening, value };
}

pub fn readValue(self: *@This()) Err!WordValue {
    return switch (self.tokens.peek().value) {
        .number => .{ .literal = try self.readNumber(Cpu.Word) },
        .ident => .{ .label = (try self.expect(.ident)).range },
        else => self.failExpected(&.{ .number, .ident }),
    };
}

fn readNumber(self: *@This(), Number: type) Err!Number {
    // Assumes there is at least one character, which is fair.
    const num = try self.expect(.number);
    const raw_num = num.range.substr(self.src);

    const without_number_sign = raw_num[1..];

    const head = without_number_sign[0 .. without_number_sign.len - 1];
    const base_char = without_number_sign[without_number_sign.len - 1];

    // Allow no suffix for one-character literals
    if (head.len == 0) switch (base_char) {
        'd', 'b', 'x' => {},
        else => return std.fmt.parseUnsigned(Number, &.{base_char}, 16) catch return self.fail(.{ .invalid_number_literal = .{
            .number = num.range,
            .base = 16,
        } }),
    };

    return switch (base_char) {
        inline 'x', 'd', 'b' => |tag| {
            const base = switch (tag) {
                'x' => 16,
                'd' => 10,
                'b' => 2,
                else => unreachable,
            };

            return std.fmt.parseUnsigned(Number, head, base) catch return self.fail(.{ .invalid_number_literal = .{
                .number = num.range,
                .base = base,
            } });
        },
        else => return self.fail(.{ .expected_base = num.range }),
    };
}

/// Consumes the next token from this parser if it's the provided tag. Returns
/// `null` otherwise.
pub fn consume(self: *@This(), comptime token: Token) ?Ranged(Token) {
    return self.consumeMany(&.{token});
}

/// Consumes the next token from this parser if it's any of the provided tags.
/// Returns `null` otherwise.
pub fn consumeMany(self: *@This(), comptime tokens: []const Token) ?Ranged(Token) {
    const next_tag = self.tokens.peek().value;

    inline for (tokens) |tag| {
        if (tag == next_tag) return self.tokens.next();
    } else return null;
}

/// Reads a token, returning an error if it's not equal to the given tag.
pub fn expect(self: *@This(), comptime token: Token) !Ranged(Token) {
    return self.expectMany(&.{token});
}

/// Reads a token, returning an error if it's not one of the given tags.
pub fn expectMany(self: *@This(), comptime tags: []const Token) !Ranged(Token) {
    return self.consumeMany(tags) orelse self.failExpected(tags);
}

/// Fails with an error message saying that the given tags were expected.
pub fn failExpected(self: *@This(), comptime tags: []const Token) error{CodeError} {
    return self.fail(.{ .expected_tag = .{
        .expected = tags,
        .found = self.tokens.peek(),
    } });
}

/// Fails, storing the given error context and returning an error.
pub fn fail(self: *@This(), @"error": failure.Error) error{CodeError} {
    self.error_context = @"error";
    return error.CodeError;
}

/// Returns the location of the tokenizer.
pub fn location(self: *@This()) lex.Location {
    return self.tokens.loc;
}

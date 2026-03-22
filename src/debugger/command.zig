const std = @import("std");

/// A parsed command. To be handled by the debugger.
pub const Command = union(enum) {
    exit: void,
    help: void,
    step: void,
    count: void,

    pub fn read(t: *Tokenizer) !Command {
        if (t.expect("exit")) {
            try t.ensureEOF();
            return .{ .exit = {} };
        } else if (t.expect("help") or t.expect("hlep")) {
            try t.ensureEOF();
            return .{ .help = {} };
        } else if (t.expect("step") or t.expect("s")) {
            try t.ensureEOF();
            return .{ .step = {} };
        } else if (t.expect("count") or t.expect("c")) {
            try t.ensureEOF();
            return .{ .count = {} };
        }

        return error.ExpectedCommandType;
    }
};

/// A simple tokenizer for command parsing.
pub const Tokenizer = struct {
    src: []const u8,
    pos: usize,

    /// Initialize the tokenizer at position 0
    pub fn init(src: []const u8) @This() {
        return .{
            .src = src,
            .pos = 0,
        };
    }

    /// Returns whether or not this tokenizer is entirely empty and cannot read
    /// any more characters.
    pub fn isEmpty(self: *@This()) bool {
        return self.pos == self.src.len;
    }

    /// Peeks the next character, returning 0 if one could not be found.
    pub fn peekChar(self: *@This()) u8 {
        return if (self.isEmpty()) 0 else self.src[self.pos];
    }

    /// Returns the next character, which is 0 if this tokenizer is empty.
    pub fn nextChar(self: *@This()) u8 {
        if (self.isEmpty()) return 0;

        const value = self.src[self.pos];
        self.pos += 1;

        return value;
    }

    /// Attempts to consume the provided string, returning whether or not it
    /// succeeded.
    pub fn expect(self: *@This(), seq: []const u8) bool {
        if (std.mem.startsWith(u8, self.src[self.pos..], seq)) {
            self.pos += seq.len;
            return true;
        } else {
            return false;
        }
    }

    /// Ensures that this tokenizer is empty by erroring if not.
    pub fn ensureEOF(self: *@This()) !void {
        return if (self.isEmpty()) {} else error.ExpectedEOF;
    }

    /// Skips all whitespace according to [std.ascii.isWhitespace].
    pub fn skipWhitespace(self: *@This()) void {
        while (std.ascii.isWhitespace(self.peekChar())) {
            _ = self.nextChar();
        }
    }
};

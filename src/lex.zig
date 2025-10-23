const std = @import("std");

/// A location in a source string. This contains an absolute position, as well
/// as its row and column.
pub const Location = struct {
    pos: usize,
    row: usize,
    col: usize,

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.print("line {} column {}", .{ self.row, self.col });
    }
};

/// A range between two values, simply storing a start and an end.
pub const Range = struct {
    start: Location,
    end: Location,

    pub fn substr(self: @This(), src: []const u8) []const u8 {
        return src[self.start.pos..self.end.pos];
    }

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.print("from {} to {}", .{ self.start, self.end });
    }
};

/// Calls the provided function with the given context variable. This only
/// supports one argument as it would be more complicated to handle others, as
/// the `.location() :: Self -> Location` function is used on the given
/// `context`.
///
/// This has the advantage over `Ranged(T).wrap(context, func)` as it doesn't
/// require specifying the wrapped type.
pub fn ranged(context: anytype, func: anytype) PseudoSequence(@typeInfo(@TypeOf(func)).@"fn".return_type.?) {
    const start: Location = context.location();

    const raw_value = func(context);
    const value = if (@typeInfo(@TypeOf(raw_value)) == .error_union) try raw_value else raw_value;

    return .{
        .range = .{
            .start = start,
            .end = context.location(),
        },
        .value = value,
    };
}

/// A range around a given type that indicates its location in a source file.
/// This simply adds a `range :: Range` field whilst wrapping it, as well as
/// some functions that obey some laws, although this is not quite an
/// applicative functor as there is no way to inject an arbitrary value into a
/// `Ranged(T)` without calling some associated function.
pub fn Ranged(T: type) type {
    return struct {
        range: Range,
        value: T,

        pub const Child = T;

        /// Wraps an infallible function from `context -> T`, producing a
        /// `Ranged(T)`.
        pub fn wrap(context: anytype, func: anytype) Ranged(T) {
            const start: Location = context.location();

            const value = func(context);

            return .{
                .range = .{
                    .start = start,
                    .end = context.location(),
                },
                .value = value,
            };
        }

        /// Replace the value of this range with another one.
        /// See `(<$) :: a -> f b -> f a`.
        pub fn replace(self: @This(), new_value: anytype) Ranged(@TypeOf(new_value)) {
            return .{
                .range = self.range,
                .value = new_value,
            };
        }

        /// Lift a function `a -> b` under this range.
        /// See `fmap :: (a -> b) -> f a -> f b`.
        pub fn map(self: @This(), context: anytype, func: anytype) Ranged(@typeInfo(@TypeOf(func)).@"fn".return_type.?) {
            return .{
                .range = self.range,
                .value = try func(context, self.value),
            };
        }

        /// Sequentially compose the given action onto this current range.
        /// See `(>>=) :: m a -> (a -> m b) -> m b` and
        /// `sequence :: t (f a) -> f (t a)`. This combines them into the single
        /// operation `f a -> (a -> f (t b)) -> t (f b)`.
        pub fn andThen(self: @This(), context: anytype, func: anytype) PseudoSequence(@typeInfo(@TypeOf(func)).@"fn".return_type.?) {
            const value = try func(context, self);

            return .{
                .range = .{
                    .start = self.range.start,
                    .end = context.location(),
                },
                .value = value,
            };
        }

        pub fn format(self: @This(), writer: anytype) !void {
            try writer.print("{f}", .{self.value});
        }
    };
}

// Adds a range type around the error union of a type, or just adds a
// range wrapper to the base type if it's not an error union.
// Having a sequence operation in itself is an anti-pattern in Zig
// because it stores errors, which means they are prevented from
// bubbling up naturally and eliminates the trace.
fn PseudoSequence(T: type) type {
    return switch (@typeInfo(T)) {
        .error_union => |err| err.error_set!Ranged(err.payload),
        else => Ranged(T),
    };
}

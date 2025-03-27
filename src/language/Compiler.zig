const std = @import("std");
const Parser = @import("Parser.zig");

/// The tag for types.
pub const TypeTag = enum {
    empty,
    unit,
    function,
    product,
    tagged_product,
    sum,
    tagged_sum,
    unique,
    @"type",
};

/// Possible fully-evaluated type expressions.
pub const Type = union(TypeTag) {
    /// The empty type. This has no inhabitants, and is thus equivalent to the
    /// concept of `noreturn`. This is equivalent to `sum {}`.
    empty,

    /// The unit type. This has exactly one inhabitant, so is equivalent to
    /// `null`, `void`, etc. This is equivalent to `product {}`.
    unit,

    /// A function that can be called. This is the only way of expressing any
    /// sort of "lazy" evaluation, because functions are not (and usually can
    /// not) be eagerly evaluated.
    ///
    /// A function signature works like this: `fn(x: i32, y: i32) x * y`.
    ///
    /// You can omit the type names for it to be coerceable to other types, but
    /// if no coercion happens, it works like Zig's `anytype`. This might be
    ///
    /// For example: `const Mul = fn(x, y) x * y;`
    ///
    function: struct {
        parameters: std.ArrayList(Type),
        @"return": *Type,
    },

    /// The product between multiple types. This is equivalent to a tuple.
    ///
    /// For example: `const Pos = product { u32, u32 }`
    product: std.ArrayList(Type),

    /// The product between multiple types, including a tag. This is equivalent
    /// to a struct.
    ///
    /// For example: `const Pos = product { x: u32, y: u32 }`
    tagged_product: std.StringHashMap(Type),

    /// The sum between multiple types. This is equivalent to a union.
    ///
    /// For example: `const Ip = sum { u32, [4]u8 }`
    sum: std.ArrayList(Type),

    /// The sum between multiple types, including a tag. This is equivalent to a
    /// tagged union.
    ///
    /// For example: `const Ip = sum { v4: u32, v6: u128 }`
    tagged_sum: std.StringHashMap(Type),

    /// A unique wrapper around a type.
    ///
    /// Since all types are, by default, equal by structure (i.e. `struct { u32
    /// } == struct { u32 }`), a unique type simply removes equality by
    /// identity, so, for example, `unique struct { u32 } != unique struct { u32
    /// }`.
    ///
    /// This does not necessarily affect coercion (i.e. `struct { 5 }` being
    /// coercible to the type `unique struct { u32 }`).
    unique: *Type,

    /// A type.
    ///
    /// When the value of an expression is a type, the type of the expression is
    /// `type`. This does lead to unsoundness in the type system due to
    /// Russell's paradox, likely the only point of unsoundness, but I don't
    /// really care.
    type,
};

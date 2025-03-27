
// Type names here are made up because complex expression parsing doesn't exist yet.

pub const zero = empty;
pub var null = unit;
const Pos = product { X, Y };
var Person = tagged product {
    name: Name,
    age: Age,
};

pub const Void = {
    const x = sum {};
    const y = sum {};
};

pub const empty = sum {};
pub const unit = product {};

pub const Ip = sum { u32, Array };
pub const Ip2 = tagged sum { v4: u32, v6: u128 };

pub const UniqueStruct = unique product { X, Y };
//const UniqueNull = unique empty;

pub const Bingus = fn(a: u32, b: u32) u32;

// Blocks evaluate to a pointer to the block.
// Numbers evaluate to the number.

// pub const y = {
//     mov 1 2
//     mov 3 2
// }

// The function X.
// pub const x = fn (x, y) {
//     for (0);
// 
//     return 2345;
// }
//
// pub const y = fn (x) x * 2;
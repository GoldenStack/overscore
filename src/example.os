
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

pub const X = (fn(a: u32, b: u32) u32 {
    const _ = ((((((((Void))))))));
    return (((((4)))));
});

pub const Four = X(2, 3);
pub const FourAgain = ((((((((((((X))))))))))))(2, 3);

pub const Bingus = fn(a: u32, b: u32) u32 {
    const _ = Void;
    return 5;
};

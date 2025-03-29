
// Type names here are made up because complex expression parsing doesn't exist yet.

pub const zero = empty;
pub var null = unit;
const Pos = product { X, Y };
var Person = product {
    name: Name,
    age: Age,
};

person: 2,
abc: 2,
//person,
//person: 2,

const Y = product {};
const Name = null;
var Age = null;
var u32 = product { sum {}, sum {} };
pub const Array = 2;
pub const u128 = 2;

pub const Void = {
    const x = sum {};
    const y = sum {};
};

pub const BeepBeep = fn (x: Person) Age {
    return x.age;
};

pub const empty = sum {};
pub const unit = product {};

pub const Ip = sum { u32, Array };
pub const Ip2 = sum { v4: u32, v6: u128 };

pub const UniqueStruct = unique product { X, Y };
//const UniqueNull = unique empty;

pub const X = (fn(a: u32, b: u32) u32 {
    const _ = ((((((((Void))))))));
    return (((((4)))));
});

pub const Four = X(2, 3);
pub const FourAgain = ((((((((((((X))))))))))))(2, 3);

const TripleGuardedSeven = fn() fn() fn() u32 {
    return fn() fn() u32 {
        return fn() u32 {
            return 7;
        };
    };
}()()();

pub const Bingus = fn(a: u32, b: u32) u32 {
    const _ = Void;
    return 5;
};

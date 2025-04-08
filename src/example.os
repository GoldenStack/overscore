
pub const main = fn () void {
    const number = second(5, CONSTANT);

    var ppep = if 1 then { return 2; } else 2;
    while 1234 do { {}; };
    do {} while 1234;

    return ((number));
};

pub const second = fn (first: 5, second: 10) 50 {
    return second;
};

const CONSTANT = (7);

} // Uses a parser bug to avoid parsing the rest of the file

pub const zero = empty;
pub var null = unit;
const Pos = product { X, Y };
var Person = product {
    name: Name,
    age: Age,
};

pub const Two = if 0 then 1 else 2;

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
    const unique = 2;
    const identifier = 250;
    const identifier2 = 505050;

    const a = do {} while 0;
    const b = while 0 do {};
};

//const unique = product {};

//var x456 = product { Y, a:   


//Y   };

pub const BeepBeep = fn (x: Person) Age {
    return x.age;
};

pub const recursive = fn () sum {} {
    return recursive();
};

pub const empty = sum {};
pub const unit = product {};

pub const Ip = sum { u32, Array };
pub const Ip2 = sum { v4: u32, v6: u128 };

pub const DistinctStruct = distinct product { X, Y };
//const DistinctNull = distinct empty;

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

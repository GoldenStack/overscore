//pub const TypeType: ((type)) = (type);
//pub const Type: (TypeType) = (word);
//pub const Value: ((((Type)))) = (((5)));
//pub const main: ((Type)) = (((Value)));

//pub const main: word = main;

//pub const main: word = abc.a.*;
pub const abc: product { a: word; } = def.*;
pub const def: product { a: word; } = container {
    pub const a = 2;
};

pub const beep: type = *word;
//pub const main: product { aa: (**(beep.*)); a: **word; } = other.*;
pub const main: sum { xy: word; } = container { pub const xy = other.a.*.*.*; pub const x = 2;};
//pub const main = main2.*.*.*.*;
pub const main2: ***word = main3;
pub const main3: **word = main4;
pub const main4: *word = main5;
pub const main5: word = 2;

pub const value: *word = other2.value2;

pub const other: product { aa: ***word; a: **word; } = container {
    pub const aa: ***word = a;
    pub const a: **word = value;
};

pub const other2: product { value2: word; } = container {
    pub const value2: word = 2;
};

// just to make sure parsing works :)
const b: word = 2;
pub var c: word = 2;
var d: word = 2;

//pub var .d = 5;
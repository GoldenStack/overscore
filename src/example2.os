//pub const TypeType: ((type)) = (type);
//pub const Type: (TypeType) = (word);
//pub const Value: ((((Type)))) = (((5)));
//pub const main: ((Type)) = (((Value)));

//pub const main: word = main;

pub const beep: type = *word;
pub const main: interface { aa: (**(beep.*)); a: **word; } = other.*;
//pub const main = main2.*.*.*.*;
pub const main2: ***word = main3;
pub const main3: **word = main4;
pub const main4: *word = main5;
pub const main5: word = 2;

pub const value: *word = other2.value2;

pub const other: interface { aa: ***word; a: **word; } = container {
    pub const aa: ***word = a;
    pub const a: **word = value;
};

pub const other2: interface { value2: word; } = container {
    pub const value2: word = 2;
};

// just to make sure parsing works :)
const b: word = 2;
pub var c: word = 2;
var d: word = 2;
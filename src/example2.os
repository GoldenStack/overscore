pub .main: MainType.* = main1->y.*;

//pub .main = container2->x.*;

pub .container2 = (
    pub .x = container2->y.*;
    pub .y = 2;
);

.main1: RGType.* = main2.*;

.main2: RGType.* = main3.*;

.main3: RGType.* = pub .y = other->b.*.*;

pub .MainType = word;
pub .RGType = (.y: word or .x: **word);

pub .other = (
    pub .aa: ***word = bb;
    pub .b: *word = value;
);

pub .bb = other.b;

pub .value = ((((2))));

//.main: (.x: word or .y: (.aa: ***word and .b: *word)) = .y: other.*;

//.other: (.aa: ***word and .b: *word) = (
//    .aa: ***word = bb,
//    .b: *word = value
//)

//.bb = other.b.*;

//.value = 2;

//pub const TypeType: ((type)) = (type);
//pub const Type: (TypeType) = (word);
//pub const Value: ((((Type)))) = (((5)));
//pub const main: ((Type)) = (((Value)));

//pub const main: word = main;

//pub const main: word = abc.a.*;
//pub const abc: product { a: word; } = def.*;
//pub const def: product { a: word; } = container {
//    pub const a = 2;
//};

//pub const beep: type = *word;
//pub const main: product { aa: (**(beep.*)); a: **word; } = other.*;
//pub const main: sum { xy: word; } = container { pub const xy = other.a.*.*.*; pub const x = 2;};
//pub const main = main2.*.*.*.*;
//pub const main2: ***word = main3;
//pub const main3: **word = main4;
//pub const main4: *word = main5;
//pub const main5: word = 2;

//pub const value: *word = other2.value2;

//pub const other: product { aa: ***word; a: **word; } = container {
//    pub const aa: ***word = a;
//    pub const a: **word = value;
//};

//pub const other2: product { value2: word; } = container {
//    pub const value2: word = 2;
//};

// just to make sure parsing works :)
//const b: word = 2;
//pub var c: word = 2;
//var d: word = 2;

//pub var .d = 5;

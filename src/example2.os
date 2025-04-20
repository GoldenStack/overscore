//pub const TypeType: ((type)) = (type);
//pub const Type: (TypeType) = (word);
//pub const Value: ((((Type)))) = (((5)));
//pub const main: ((Type)) = (((Value)));

//pub const main: word = main;

pub const main: word = other.aa;

pub const value: word = other2.value2;

pub const other: type = container {
    pub const aa: word = a;
    pub const a: word = value;

    const b: word = 2;
    pub var c: word = 2;
    var d: word = 2;
};

pub const other2: type = container {
    pub const value2: word = 2;
};

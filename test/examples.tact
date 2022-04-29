let Struct = struct {
    field: Type,
}

let Enum = enum {
    A, B
}

let Maybe(T: Type) = union {
    Just(T),
    Nothing,
}

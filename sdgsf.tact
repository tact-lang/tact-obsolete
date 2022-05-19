struct Foo {
    val field1: Int(32)
    val field2: Int(64)
}

fn serialize_Foo_builder(b: Builder, field1: Int(32), field2: Int(64)) -> Builder {
    let b = field1.serialize(b);
    let b = field2.serialize(b);
    return b
}

fn serialize_Foo(field1: Int(32), field2: Int(64)) -> Cell {
    let b = new_builder();
    let b = serialize_Foo_builder(b);
    let c = b.build();
    return c;
}
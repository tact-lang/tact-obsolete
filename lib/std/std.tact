struct Builder {
  val b: builtin_Builder

  fn new() -> Self {
    Self { b: builtin_builder_new() }
  }
  fn build(self: Self) -> builtin_Cell {
    builtin_builder_build(self.b)
  }
  fn serialize_int(self: Self, int: Integer, bits: Integer) -> Self {
    let b = builtin_builder_store_int(self.b, int, bits);
    Self { b: b }
  }
}

struct Int(bits: Integer) {
  val value: Integer

  fn new(i: Integer) -> Self {
    Self { value: i }
  }

  fn serialize(self: Self, builder: Builder) -> Builder {
    builder.serialize_int(self.value, bits)
  }

  impl From(Integer) {
    fn from(i: Integer) -> Self {
      Self { value: i }
    }
  }
}

struct SendRawMsgFlags {
  val value: Integer

  fn default() -> Self {
    Self { value: 0 }
  }

  fn carry_smart_contract_balance() -> Self {
    Self { value: 128 }
  }

  fn carry_rest_from_transaction_input() -> Self {
    Self { value: 64 }
  }
}

fn send_raw_msg(msg: builtin_Cell, flags: SendRawMsgFlags) -> VoidType {
  builtin_send_raw_msg(msg, flags.value);
}

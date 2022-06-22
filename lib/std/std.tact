struct Cell {
  val c: builtin_Cell
}

struct Builder {
  val b: builtin_Builder

  fn new() -> Self {
    Self { b: builtin_builder_new() }
  }
  fn build(self: Self) -> Cell {
    let c = builtin_builder_build(self.b);
    Cell { c: c }
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

struct AddrNone {}

// Interesting situation with this declaration: `bits` field should have count of bits
// deriving from the `len` field which requires more powerful dependent types than we have for now.
// Do we want to make such declarations possible?
struct AddrExtern { 
  val len: Int(9)
  val bits: Integer
}

union MsgAddressExt {
  case AddrNone
  case AddrExtern
}

struct AddressStd {
  val workchain_id: Int(8)
  val address: Int(256)
}

struct AddressVar {
  val len: Int(9)
  val workchain_id: Int(8)
  val address: Integer
}

union MsgAddressInt {
  case AddressStd
  case AddressVar
}

union MsgAddress {
  case MsgAddressExt
  case MsgAddressInt
}

struct ExtOutMsgInfo {
  val src: MsgAddress
  val dest: MsgAddressExt
  val created_lt: Int(64) // TODO: uint
  val created_at: Int(32) // TODO: uint
}

union CommonMsgInfoRelaxed {
  case ExtOutMsgInfo
  // TODO: int_msg_info
}

fn send_raw_msg(msg: Cell, flags: SendRawMsgFlags) -> VoidType {
  builtin_send_raw_msg(msg.c, flags.value);
}

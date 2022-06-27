struct Cell {
  val c: builtin_Cell
}

// Do not change place of builder struct - for internal reasons
// it should be second struct in the file.
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

struct LoadResultBase(S: Type, X: Type) {
  val slice: S
  val value: X
}

struct Slice {
  val s: builtin_Slice

  fn parse(cell: Cell) -> Self {
    Self { s: builtin_slice_begin_parse(cell.c) }
  }

  fn load_int(self: Self, bits: Integer) -> LoadResultBase(Self, Integer) {
    let output = builtin_slice_load_int(self.s, bits);
    let slice = Self { s: output.value1 };
    let int = output.value2;
    LoadResultBase(Self, Integer) { slice: slice, value: int }
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

  fn serialize(self: Self, b: Builder) -> Builder {
    let b = self.len.serialize(b);
    let b = b.serialize_int(self.bits, self.len.value);
    return b;
  }
}

union MsgAddressExt {
  case AddrNone
  case AddrExtern

  fn serialize(self: Self, b: Builder) -> Builder {
    switch(self) {
      case AddrNone a => {
        return b.serialize_int(0, 1);
      }
      case AddrExtern addr => {
        let b = b.serialize_int(1, 1);
        return addr.serialize(b);
      }
    }
  }
}

struct AddressStd {
  val workchain_id: Int(8)
  val address: Int(256)

  fn serialize(self: Self, b: Builder) -> Builder {
    let b = b.serialize_int(0, 0); // AnyCast
    serializer(Self)(self, b)
  }
}

struct AddressVar {
  val len: Int(9)
  val workchain_id: Int(8)
  val address: Integer

  fn serialize(self: Self, b: Builder) -> Builder {
    let b = b.serialize_int(0, 0); // AnyCast
    let b = self.len.serialize(b);
    let b = self.workchain_id.serialize(b);
    let b = b.serialize_int(self.address, self.len.value);
    return b;
  }
}

union MsgAddressInt {
  case AddressStd
  case AddressVar

  fn serialize(self: Self, b: Builder) -> Builder {
    switch(self) {
      case AddressStd addr => {
        let b = b.serialize_int(0, 1);
        return addr.serialize(b);
      }
      case AddressVar addr => {
        let b = b.serialize_int(1, 1);
        return addr.serialize(b);
      }
    }
  }
}

union MsgAddress {
  case MsgAddressExt
  case MsgAddressInt

  fn serialize(self: Self, b: Builder) -> Builder {
    switch(self) {
      case MsgAddressExt addr_ext => {
        let b = b.serialize_int(0, 1);
        return addr_ext.serialize(b);
      }
      case MsgAddressInt addr_int => {
        let b = b.serialize_int(1, 1);
        return addr_int.serialize(b);
      }
    }
  }
}

struct ExtOutMsgInfo {
  val src: MsgAddress
  val dest: MsgAddressExt
  val created_lt: Int(64) // TODO: uint
  val created_at: Int(32) // TODO: uint

  fn serialize(self: Self, b: Builder) -> Builder {
    let b = self.src.serialize(b);
    let b = self.dest.serialize(b);
    let b = self.created_lt.serialize(b);
    let b = self.created_at.serialize(b);
    return b;
  }
}

union CommonMsgInfoRelaxed {
  case ExtOutMsgInfo
  // TODO: int_msg_info

  fn serialize(self: Self, b: Builder) -> Builder {
    switch(self) {
      case ExtOutMsgInfo info => {
        let b = b.serialize_int(3, 2); // 0b11
        return info.serialize(b);
      }
    }
  }
}

fn send_raw_msg(msg: Cell, flags: SendRawMsgFlags) -> VoidType {
  builtin_send_raw_msg(msg.c, flags.value);
}

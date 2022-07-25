struct Cell {
  val c: builtin_Cell
}

// Do not change place of builder struct - for internal reasons
// it should be second struct in the file.
struct Builder {
  val inner: builtin_Builder

  fn new() -> Self {
    Self { inner: builtin_builder_new() }
  }
  fn build(self: Self) -> Cell {
    Cell { c: builtin_builder_build(self.inner) }
  }
  fn serialize_int(self: Self, int: Integer, bits: Integer) -> Self {
    Self { inner: builtin_builder_store_int(self.inner, int, bits) }
  }
  fn serialize_uint(self: Self, uint: Integer, bits: Integer) -> Self {
    Self { inner: builtin_builder_store_uint(self.inner, uint, bits) }
  }
  fn serialize_coins(self: Self, c: Integer) -> Self {
    Self { inner: builtin_builder_store_coins(self.inner, c) }
  }
}

struct Slice {
  val s: builtin_Slice

  fn parse(cell: Cell) -> Self {
    Self { s: builtin_slice_begin_parse(cell.c) }
  }

  fn load_int(self: Self, bits: Integer) -> LoadResult[Integer] {
    let output = builtin_slice_load_int(self.s, bits);
    let slice = Self { s: output.value1 };
    let int = output.value2;
    LoadResult[Integer] { slice: slice, value: int }
  }

  fn load_uint(self: Self, bits: Integer) -> LoadResult[Integer] {
    let output = builtin_slice_load_uint(self.s, bits);
    let slice = Self { s: output.value1 };
    let int = output.value2;
    LoadResult[Integer] { slice: slice, value: int }
  }
}

struct Coins {
  val value: Integer

  fn new(c: Integer) -> Self {
    Self { value: c }
  }

  impl Serialize {
    fn serialize(self: Self, builder: Builder) -> Builder {
      builder.serialize_coins(self.value)
    } 
  }
}

struct Int[bits: Integer] {
  val value: Integer

  fn new(i: Integer) -> Self {
    Self { value: i }
  }

  impl Serialize {
    fn serialize(self: Self, builder: Builder) -> Builder {
      builder.serialize_int(self.value, bits)
    } 
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res = s.load_int(bits);
      let {slice, value} = res;
      
      LoadResult[Self] { 
        slice: slice, 
        value: Self { value: value }
      }
    }
  }

  impl From(Integer) {
    fn from(i: Integer) -> Self {
      Self { value: i }
    }
  }
}

struct Uint[bits: Integer] {
  val value: Integer

  fn new(i: Integer) -> Self {
    Self { value: i }
  }

  impl Serialize {
    fn serialize(self: Self, builder: Builder) -> Builder {
      builder.serialize_uint(self.value, bits)
    } 
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res = s.load_uint(bits);
      
      LoadResult[Self] { 
        slice: res.slice, 
        value: Self { value: res.value }
      }
    }
  }

  impl From(Integer) {
    fn from(i: Integer) -> Self {
      Self { value: i }
    }
  }
}

struct AddrNone {
  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      return b;
    }
  }
}

// Interesting situation with this declaration: `bits` field should have count of bits
// deriving from the `len` field which requires more powerful dependent types than we have for now.
// Do we want to make such declarations possible?
struct AddrExtern { 
  val len: Int[9]
  val bits: Integer

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      let b = self.len.serialize(b);
      let b = b.serialize_int(self.bits, self.len.value);
      return b;
    }
  }

  impl Deserialize {
    fn deserialize(slice: Slice) -> LoadResult[Self] {
      let {value as len, slice} = Int[9].deserialize(slice);
      let {value as bits, slice} = slice.load_int(len);

      LoadResult[Self] { 
        slice: slice, 
        value: Self { 
          len: len,
          bits: bits,
        }
      }
    } 
  }
}

union MsgAddressExt {
  case AddrNone
  case AddrExtern

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_discr = s.load_int(1);
      if (builtin_equal(res_discr.value, 0)) {
        return LoadResult[Self].new(res_discr.slice, AddrNone{});
      } else if (builtin_equal(res_discr.value, 1)) {
        let res_addr = AddrExtern.deserialize(res_discr.slice);
        return LoadResult[Self].new(res_addr.slice, res_addr.value);
      } else {
        throw(0);
      }
    }
  }
}

struct AddressStd {
  val workchain_id: Int[8]
  val address: Int[256]

  fn new(workchain_id: Int[8], address: Int[256]) -> Self {
    Self {
      workchain_id: workchain_id,
      address: address,
    }
  }

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      let b = b.serialize_int(0, 0); // AnyCast
      serializer[Self](self, b)
    }
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_anycast = s.load_int(1);
      if (builtin_equal(res_anycast.value, 0)) {
        let res_workchain = Int[8].deserialize(res_anycast.slice);
        let res_address = Int[256].deserialize(res_workchain.slice);
        return LoadResult[Self]
          .new(res_address.slice, Self {
            workchain_id: res_workchain.value,
            address: res_address.value,
          });
      } else {
        throw(0); // unreachable
      }
    }
  }
}

struct AddressVar {
  val len: Int[9]
  val workchain_id: Int[8]
  val address: Integer

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      let b = b.serialize_int(0, 0); // AnyCast
      let b = serializer[Self](self, b);
      return b;
    }
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_anycast = s.load_int(1);
      if (builtin_equal(res_anycast.value, 0)) {
        let res_len = Int[9].deserialize(res_anycast.slice);
        let res_workchain = Int[8].deserialize(res_len.slice);
        let res_address = res_workchain.slice.load_int(res_len);
        return LoadResult[Self]
          .new(res_address.slice, Self {
            len: res_len.value,
            workchain_id: res_workchain.value,
            address: res_address.value,
          });
      } else {
        throw(0); // unreachable
      }
    }
  }
}

union MsgAddressInt {
  case AddressStd
  case AddressVar

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_discr = s.load_int(1);
      if (builtin_equal(res_discr.value, 0)) {
        let res_addr = AddressStd.deserialize(res_discr.slice);
        return LoadResult[Self].new(res_addr.slice, res_addr.value);
      } else {
        let res_addr = AddressVar.deserialize(res_discr.slice);
        return LoadResult[Self].new(res_addr.slice, res_addr.value);
      }
    }
  }
}
union MsgAddress {
  case MsgAddressExt
  case MsgAddressInt

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_discr = s.load_int(1);
      if (builtin_equal(res_discr.value, 0)) {
        let res_addr = MsgAddressExt.deserialize(res_discr.slice);
        return LoadResult[Self].new(res_addr.slice, res_addr.value);
      } else {
        let res_addr = MsgAddressInt.deserialize(res_discr.slice);
        return LoadResult[Self].new(res_addr.slice, res_addr.value);
      }
    }
  }
}

struct ExtOutMsgInfo {
  val src: MsgAddressInt
  val dest: MsgAddressExt
  val created_lt: Int[64] // TODO: uint
  val created_at: Int[32] // TODO: uint

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_src = MsgAddressInt.deserialize(s);
      let res_dest = MsgAddressExt.deserialize(res_src.slice);
      let res_created_lt = Int[64].deserialize(res_dest.slice);
      let res_created_at = Int[64].deserialize(res_created_lt.slice);
      return LoadResult[Self]
        .new(res_created_at.slice, Self {
          src: res_src.value,
          dest: res_dest.value,
          created_lt: res_created_lt.value,
          created_at: res_created_at.value,
        });
    }
  }
}

struct ExtOutMsgInfoRelaxed {
  val src: MsgAddress
  val dest: MsgAddressExt
  val created_lt: Int[64] // TODO: uint
  val created_at: Int[32] // TODO: uint

  impl Serialize {
    // TODO: There is bugs when tried to use `serializer` fn here.
    fn serialize(self: Self, b: Builder) -> Builder {
      let b = self.src.serialize(b);
      let b = self.dest.serialize(b);
      let b = self.created_lt.serialize(b);
      let b = self.created_at.serialize(b);
      return b;
    }
  }
}

union CommonMsgInfo {
  case ExtOutMsgInfo
  // TODO: int_msg_info, ext_in_msg_info

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_discr1 = s.load_int(1);
      if (builtin_equal(res_discr1.value, 0)) {
        /* TODO: int_msg_info */
      } else {
        let res_discr2 = res_discr1.slice.load_int(1);
        if (builtin_equal(res_discr2.value, 0)) {
          /* TODO: ext_in_msg_info */
        } else {
          let res_info = ExtOutMsgInfo.deserialize(res_discr2.slice);
          return LoadResult[Self].new(res_info.slice, res_info.value);
        }
      }
    }
  }
}

struct IntMsgInfoRelaxedFlags {
  val ihr_disabled: Int[1]
  val bounce: Int[1]
  val bounced: Int[1]

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }
}

struct IntMsgInfoRelaxedAddresses {
  val src: MsgAddressInt
  val dst: MsgAddressInt

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }
}

struct IntMsgInfoRelaxedCoins {
  val ihr_fee: Coins
  val fwd_fee: Coins

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }
}

struct Timestamps {
  val created_lt: Int[64] // TODO: uint
  val created_at: Int[32] // TODO: uint

  fn zeros() -> Self {
    Self {
      created_lt: 0,
      created_at: 0,
    }
  }

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }
}

struct IntMsgInfoRelaxed {
  val flags: IntMsgInfoRelaxedFlags
  val addresses: IntMsgInfoRelaxedAddresses
  val coins: IntMsgInfoRelaxedCoins
  val timestamps: Timestamps

  fn new(
    flags: IntMsgInfoRelaxedFlags, 
    dst: MsgAddressInt,
    coins: IntMsgInfoRelaxedCoins,
  ) -> Self {
    Self {
      flags: flags,
      addresses: IntMsgInfoRelaxedAddresses {
        src: AddressStd.new(0, 0), // it will be replaced by smartcontract address by TVM
        dst: dst,
      },
      coins: coins,
      timestamps: Timestamps.zeros(), // it will be replaced by current timestamps by TVM
    }
  }

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      serializer[Self](self, b)
    }
  }
}

union CommonMsgInfoRelaxed {
  case ExtOutMsgInfoRelaxed
  case IntMsgInfoRelaxed

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      switch(self) {
        case IntMsgInfoRelaxed info => {
          let b = b.serialize_int(0, 1);
          return info.serialize(b);
        }
        case ExtOutMsgInfoRelaxed info => {
          let b = b.serialize_int(3, 2); // 0b11
          return info.serialize(b);
        }
      }
    }
  }
}

struct MessageRelaxed[X: Serialize] {
  val info: CommonMsgInfoRelaxed
  val body: X

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      let b = self.info.serialize(b);
      let b = b.serialize_int(0, 1); // init
      let b = b.serialize_int(0, 1); // body discriminant
      let b = self.body.serialize(b);
      return b;
    }
  }
}

struct Message[X: Deserialize] {
  val info: CommonMsgInfo
  val body: X

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let res_info = CommonMsgInfo.deserialize(s);
      let res_init = res_info.slice.load_int(1);

      if (builtin_equal(res_init.value, 0)) {
        let res_body_discr = res_init.slice.load_int(1);
        if (builtin_equal(res_body_discr.value, 0)) {
          let body = X.deserialize(res_body_discr.slice);
          let mes = Self { info: res_info.value, body: body.value };
          return LoadResult[Self].new(body.slice, mes);
        } else {
          /* TODO: cells */
        }
      } else {
        throw(0);
      }
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

fn send_raw_msg(msg: Cell, flags: SendRawMsgFlags) -> VoidType {
  builtin_send_raw_msg(msg.c, flags.value);
}

struct Cell {
  val c: builtin_Cell
}

// Do not change place of builder struct - for internal reasons
// it should be second struct in the file.
struct Builder {
  val inner: builtin_Builder

  fn new() -> Self {
    Self { inner: builtin_begin_cell() }
  }
  fn build(self: Self) -> Cell {
    Cell { c: builtin_end_cell(self.inner) }
  }
  fn serialize_int(self: Self, int: Integer, bits: Integer) -> Self {
    Self { inner: builtin_store_int(self.inner, int, bits) }
  }
  fn serialize_uint(self: Self, uint: Integer, bits: Integer) -> Self {
    Self { inner: builtin_store_uint(self.inner, uint, bits) }
  }
  fn serialize_coins(self: Self, c: Integer) -> Self {
    Self { inner: builtin_store_grams(self.inner, c) }
  }
  fn serialize_ref(self: Self, cell: Cell) -> Self {
    Self { inner: builtin_store_ref(self.inner, cell.c) }
  }
  fn serialize_maybe_ref(self: Self, maybe_cell: Cell) -> Self {
    Self { inner: builtin_store_maybe_ref(self.inner, maybe_cell.c) }
  }
  fn bits(self: Self) -> Integer {
    builtin_builder_bits(self.inner)
  }
  fn refs(self: Self) -> Integer {
    builtin_builder_refs(self.inner)
  }
  fn depth(self: Self) -> Integer {
    builtin_builder_depth(self.inner)
  }
}

struct Slice {
  val s: builtin_Slice

  fn parse(cell: Cell) -> Self {
    Self { s: builtin_begin_parse(cell.c) }
  }

  fn load_int(self: Self, bits: Integer) -> LoadResult[Integer] {
    let output = builtin_load_int(self.s, bits);
    let slice = Self { s: output.value1 };
    let int = output.value2;
    LoadResult[Integer] { slice: believe_me(slice), value: int }
  }

  fn load_uint(self: Self, bits: Integer) -> LoadResult[Integer] {
    let output = builtin_load_uint(self.s, bits);
    let slice = Self { s: output.value1 };
    let int = output.value2;
    LoadResult[Integer] { slice: believe_me(slice), value: int }
  }

  fn load_coins(self: Self) -> LoadResult[Integer] {
    let output = builtin_load_grams(self.s);
    let slice = Self { s: output.value1 };
    let coins = output.value2 ;
    LoadResult[Integer] { slice: believe_me(slice), value: coins }
  }

  fn load_ref(self: Self) -> LoadResult[Cell] {
    let output = builtin_load_ref(self.s);
    let slice = Self { s: output.value1 };
    let ref = Cell { c: output.value2 };
    LoadResult[Cell] { slice: believe_me(slice), value: ref }
  }

  fn load_bits(self: Self, bits: Integer) -> LoadResult[Self] {
    let output = builtin_load_bits(self.s, bits);
    let slice = Self { s: output.value1 };
    let slice2 = Self { s: output.value2 };
    LoadResult[Self] { slice: believe_me(slice), value: believe_me(slice2) }
  }

  /* Developer notes: if you add `LoadResult[...]` type, don't forget to
     increment Slice id in the `builtins.ml` */

  fn refs_count(self: Self) -> Integer {
    builtin_slice_refs(self.s)
  }
  fn bits(self: Self) -> Integer {
    builtin_slice_bits(self.s)
  }

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      Builder { inner: builtin_store_slice(b.inner, self.s) }
    }
  }
}

struct RefCell {
  val inner: Cell

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      b.serialize_ref(self.inner)
    }
  }
  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let {slice, value} = s.load_ref();
      LoadResult[Self].new(Self { inner: value }, slice)
    } 
  }
}

struct SliceBits[N: Integer] {
  val inner: Slice

  @derive impl Serialize {}

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let {value, slice} = s.load_bits(N);
      LoadResult[Self].new(Self { inner: value }, slice)
    } 
  }
}

fn thrown(n: Integer) {
  builtin_throw(n);
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

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let {slice, value} = s.load_coins();
      LoadResult[Self].new(Self { value: value }, slice)
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

  impl From[Integer] {
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

  impl From[Integer] {
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

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      return LoadResult[Self].new(s, Self{});
    }
  }
}

// Interesting situation with this declaration: `bits` field should have count of bits
// deriving from the `len` field which requires more powerful dependent types than we have for now.
// Do we want to make such declarations possible?
struct AddrExtern { 
  val len: Int9
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
      let {value as len, slice} = Int9.deserialize(slice);
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
  @discriminator(0, 2) // 0b00
  case AddrNone
  @discriminator(1, 2) // 0b01
  case AddrExtern

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

struct AddressStd {
  val workchain_id: Int8
  val address: Int256

  fn new(workchain_id: Int8, address: Int256) -> Self {
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
      if (res_anycast.value == 0) {
        return deserializer[Self](s);
      } else {
        thrown(0); // unreachable
      }
    }
  }
}

struct AddressVar {
  val len: Int9
  val workchain_id: Int32
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
      let {value as anycast, slice} = s.load_int(1);
      if (anycast == 0) {
        let {value as len, slice} = Int9.deserialize(slice);
        let {value as workchain_id, slice} = Int32.deserialize(slice);
        let {value as address, slice} = slice.load_int(len);
        return LoadResult[Self]
          .new(slice, Self {
            len: len,
            workchain_id: workchain_id,
            address: address,
          });
      } else {
        thrown(0); // unreachable
      }
    }
  }
}

union MsgAddressInt {
  @discriminator(2, 2) // 0b10
  case AddressStd
  @discriminator(3, 2) // 0b11
  case AddressVar

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

union MsgAddress {
  case AddrNone
  case AddrExtern
  case AddressStd
  case AddressVar

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

struct ExtOutMsgInfoRelaxed {
  val src: MsgAddress
  val dest: MsgAddressExt
  val created_lt: Uint64
  val created_at: Uint32

  @derive
  impl Serialize {}
}

struct Timestamps {
  val created_lt: Uint64
  val created_at: Uint32

  fn zeros() -> Self {
    Self {
      created_lt: 0,
      created_at: 0,
    }
  }

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

struct IntMsgInfoFlags {
  val ihr_disabled: Uint1
  val bounce: Uint1
  val bounced: Uint1

  fn new(ihr_disabled: Bool, bounce: Bool) -> Self {
    fn bool_to_int(x: Bool) -> Uint1 {
      if (x) {
        return 1;
      } else {
        return 0;
      }
    }
    Self {
      ihr_disabled: bool_to_int(ihr_disabled),
      bounce: bool_to_int(bounce),
      bounced: 0, // will be rewrited by the validator
    }
  }

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

struct IntMsgInfoAddresses {
  val src: MsgAddressInt
  val dst: MsgAddressInt

  @derive
  impl Deserialize {}
}

struct IntMsgInfoCoins {
  val amount: Coins
  val _extra_currencies: Uint1 // we ignore extra_currencies for now
  val ihr_fee: Coins
  val fwd_fee: Coins

  fn new(amount: Coins, ihr_fee: Coins, fwd_fee: Coins) -> Self {
    Self {
      amount: amount,
      _extra_currencies: 0,
      ihr_fee: ihr_fee,
      fwd_fee: fwd_fee,
    }
  }

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

struct IntMsgInfo {
  val flags: IntMsgInfoFlags
  val addresses: IntMsgInfoAddresses
  val coins: IntMsgInfoCoins
  val timestamps: Timestamps

  fn new(
    flags: IntMsgInfoFlags, 
    dst: MsgAddressInt,
    coins: IntMsgInfoCoins,
  ) -> Self {
    Self {
      flags: flags,
      addresses: IntMsgInfoAddresses {
        src: AddressStd.new(0, 0), // it will be replaced by smartcontract address by TVM
        dst: dst,
      },
      coins: coins,
      timestamps: Timestamps.zeros(), // it will be replaced by current timestamps by TVM
    }
  }

  @derive
  impl Serialize {}

  @derive
  impl Deserialize {}
}

struct ExtInMsgInfo {
  val src: MsgAddressExt
  val dest: MsgAddressInt
  val import_fee: Coins

  @derive
  impl Deserialize {}
}

union CommonMsgInfo {
  case IntMsgInfo
  @discriminator(2, 2) // 0b10
  case ExtInMsgInfo

  @derive
  impl Deserialize {}
}

union CommonMsgInfoRelaxed {
  case ExtOutMsgInfoRelaxed
  case IntMsgInfo

  impl Serialize {
    fn serialize(self: Self, b: Builder) -> Builder {
      switch(self) {
        case IntMsgInfo info => {
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
      let {slice, value as info} = CommonMsgInfo.deserialize(s);
      let {slice, value as init} = slice.load_int(1);

      if (init == 0) {
        let {slice, value as discr} = slice.load_int(1);
        if (discr == 0) {
          let {slice, value as body} = X.deserialize(slice);
          // FIXME: weird bug
          let mes = Self { info: info, body: believe_me(body) };
          return LoadResult[Self].new(mes, slice);
        } else {
          /* TODO: cells */
          thrown(0);
        }
      } else {
        thrown(0);
      }
    }
  }
}

struct SendRawMsgFlags {
  val value: Int8

  fn default() -> Self {
    Self { value: 0 }
  }

  fn carry_remaining_balance(self: Self) -> Self {
    Self { value: self.value.value + 128 }
  }

  fn carry_remaining_input(self: Self) -> Self {
    Self { value: self.value.value + 64 }
  }

  @derive
  impl Deserialize {}

  @derive
  impl Serialize {}
}

fn send_raw_msg(msg: Cell, flags: SendRawMsgFlags) -> VoidType {
  builtin_send_raw_message(msg.c, flags.value.value);
}

fn send_internal[X: Serialize](header: IntMsgInfo, body: X, flags: SendRawMsgFlags) -> VoidType {
  let info: CommonMsgInfoRelaxed = header;
  let msg = MessageRelaxed[X] { info: info, body: body };
  let ce = msg.serialize(Builder.new()).build();
  send_raw_msg(ce, flags);
}

fn send_external[X: Serialize](header: ExtOutMsgInfoRelaxed, body: X, flags: SendRawMsgFlags) -> VoidType {
  let info: CommonMsgInfoRelaxed = header;
  let msg = MessageRelaxed[X] { info: info, body: body };
  let ce = msg.serialize(Builder.new()).build();
  send_raw_msg(ce, flags);
}

fn hash_of_slice(s: Slice) -> Uint256 {
  return Uint256.new(builtin_slice_hash(s.s));
}

fn is_signature_valid(hash: Uint256, sign: SliceBits[512], pubkey: Uint256) -> Bool {
  return builtin_check_signature(hash.value, sign.inner.s, pubkey.value);
}

struct Signature {
  val _sig: SliceBits[512]
  val _rest: Slice

  fn is_valid(self: Self, public_key: Uint256) -> Bool {
    return is_signature_valid(hash_of_slice(self._rest), self._sig, public_key);
  }

  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let {value as sig, slice} = SliceBits[512].deserialize(s);
      LoadResult[Self].new(Self{_sig: sig, _rest: slice}, slice)
    }
  }
}

/* Deserialize rest of the slice into this struct */
struct RestSlice {
  val inner: Slice
  
  impl Deserialize {
    fn deserialize(s: Slice) -> LoadResult[Self] {
      let empty_slice = builtin_slice_last(s.s, 0);
      LoadResult[Self].new(Self{inner: s}, empty_slice)
    }
  }
}

struct SignedBody[X: Deserialize] {
  val _sign: Signature
  val _rest: X

  fn verify_body(self: Self, pubkey: Uint256) -> X {
    if (self._sign.is_valid(pubkey)) {
      return self._rest;
    } else {
      thrown(0);
      // This will never be called but without this return, FunC does not compile
      // this code.
      return self._rest;
    }
  }

  impl Deserialize {
    fn deserialize(slice: Slice) -> LoadResult[Self] {
      let {value as sign, slice} = Signature.deserialize(slice);
      let {value as rest, slice} = X.deserialize(slice);
      LoadResult[Self].new(
        Self { _sign: sign, _rest: believe_me(rest) },
        slice
      )
    }
  }
}

struct Globals {
  fn load_state() -> Cell {
    Cell { c: builtin_get_data() }
  }
  fn save_state(state: Cell) {
    builtin_set_data(state.c)
  }
  fn get_now() -> Int[32] {
    builtin_now()
  }
  fn get_my_address() -> AddressStd {
    AddressStd.deserialize(builtin_my_address()).value
  }
  fn get_balance() -> Coins {
    struct Balance {
      val tons: Coins
      val extra_currencies: Cell
    }
    let balance: Balance = believe_me(builtin_get_balance());
    return balance.tons;
  }
  fn get_logical_time() -> Int[64] {
    builtin_cur_lt()
  }
  fn get_block_logical_time() -> Int[64] {
    builtin_block_lt()
  }
}

struct RemainingBalance{}
struct RemainingInput{}
union Money {
  case Coins
  case RemainingBalance
  case RemainingInput
}

struct RequestBuilder[X: Serialize] {
  val msg_flags: IntMsgInfoFlags
  val send_flags: SendRawMsgFlags
  val coins: IntMsgInfoCoins
  val is_body_setup: Bool // TODO: make `Option[T]` type.
  val body: X

  fn new() -> Self {
    Self {
      msg_flags: IntMsgInfoFlags.new(false, false),
      send_flags: SendRawMsgFlags.default(),
      coins: IntMsgInfoCoins.new(Coins.new(0), Coins.new(0), Coins.new(0)),
      is_body_setup: false,
      body: believe_me(0),
    }
  }

  fn can_be_bounced(self: Self) -> Self {
    Self {
      msg_flags: IntMsgInfoFlags { ihr_disabled: self.msg_flags.ihr_disabled, bounce: 1, bounced: 0 },
      send_flags: self.send_flags,
      body: self.body,
      is_body_setup: self.is_body_setup,
      coins: self.coins,
    }
  }
  fn ihr_disabled(self: Self) -> Self {
    Self {
      msg_flags: IntMsgInfoFlags { ihr_disabled: 1, bounce: self.msg_flags.bounce, bounced: 0 },
      send_flags: self.send_flags,
      body: self.body,
      is_body_setup: self.is_body_setup,
      coins: self.coins,
    }
  }
  fn money(self: Self, money: Money) -> Self {
    switch (money) {
      case Coins coins => {
        return Self {
          msg_flags: self.msg_flags,
          send_flags: self.send_flags,
          body: self.body,
          is_body_setup: self.is_body_setup,
          coins: IntMsgInfoCoins.new(coins, self.coins.ihr_fee, self.coins.fwd_fee),
        };
      }
      case RemainingBalance _ => {
        return Self {
          msg_flags: self.msg_flags,
          send_flags: self.send_flags.carry_remaining_balance(),
          body: self.body,
          is_body_setup: self.is_body_setup,
          coins: self.coins,
        };
      }
      case RemainingInput _ => {
        return Self {
          msg_flags: self.msg_flags,
          send_flags: self.send_flags.carry_remaining_input(),
          body: self.body,
          is_body_setup: self.is_body_setup,
          coins: self.coins,
        };
      }
    }
  }
  fn body(self: Self, body: X) -> Self {
    Self {
      msg_flags: self.msg_flags,
      send_flags: self.send_flags,
      body: body,
      is_body_setup: true,
      coins: self.coins,
    }
  }
  fn send_to(self: Self, dst: MsgAddressInt) {
    if (self.is_body_setup) {
      // TODO: I don't know why MessageRelaxed[X] type does not work here.
      let info = IntMsgInfo.new(self.msg_flags, dst, self.coins);
      let b = info.serialize(Builder.new());
      let b = b.serialize_int(0, 1); // init
      let b = b.serialize_int(0, 1); // body discriminant
      let b = self.body.serialize(b);
      send_raw_msg(b.build(), self.send_flags);
    } else {
      thrown(87);
    }
  }
}

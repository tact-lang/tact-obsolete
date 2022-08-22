struct MsgBodyData {
  val subwallet: Uint[32]
  val valid_until: Uint[32]
  val seqno: Uint[32]

  @derive impl Deserialize {}
}
struct MsgBody {
  val signature: Signature
  val data: MsgBodyData
  val rest: RestSlice

  @derive impl Deserialize {}
}

struct WalletState {
  val seqno: Uint[32]
  val subwallet: Uint[32]
  val public_key: Uint[256]

  @derive impl Deserialize {}
  @derive impl Serialize {}
}

struct NextMessage {
  val cell: RefCell
  val flags: SendRawMsgFlags
  
  @derive impl Deserialize {}
}

fn recv_internal(_: Slice) {}

fn recv_external(input: Slice) {
  let body = MsgBody.deserialize(input).value;
  let data = body.data;
  let state = WalletState.deserialize(Slice.parse(builtin_get_data())).value;

  if (data.valid_until.value <= builtin_now()) { thrown(35) }
  if (data.seqno.value != state.seqno.value) { thrown(33) }
  if (data.subwallet.value != state.subwallet.value) { thrown(34) }
  //if (builtin_not(body.signature.is_valid(state.public_key))) { thrown(35) }

  builtin_accept_message();

  let slice = body.rest.inner;
  while (slice.refs_count() != 0) {
    let {value as next, slice as new_slice} = NextMessage.deserialize(slice);
    slice = new_slice;
    send_raw_msg(next.cell.inner, next.flags);
  }

  let new_state = WalletState {
    seqno: state.seqno.value + 1,
    subwallet: state.subwallet,
    public_key: state.public_key
  };
  let new_state = new_state.serialize(Builder.new()).build();
  builtin_set_data(new_state.c);
}

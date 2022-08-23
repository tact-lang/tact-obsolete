struct MsgBody {
  val subwallet: Uint32
  val valid_until: Uint32
  val seqno: Uint32

  @derive impl Deserialize {}
}

struct WalletState {
  val seqno: Uint32
  val subwallet: Uint32
  val public_key: Uint256

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
  let {value as signed, slice} = SignedBody[MsgBody].deserialize(input);
  let state = WalletState.deserialize(Slice.parse(Globals.load_state())).value;

  let body = signed.verify_body(state.public_key);

  if (body.valid_until.value <= builtin_now()) { thrown(35) }
  if (body.seqno.value != state.seqno.value) { thrown(33) }
  if (body.subwallet.value != state.subwallet.value) { thrown(34) }

  builtin_accept_message();

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
  Globals.save_state(new_state);
}

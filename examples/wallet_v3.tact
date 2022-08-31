struct MsgBody {
  val subwallet: Uint32
  val valid_until: Uint32
  val seqno: Uint32
}

struct NextMessage {
  val cell: RefCell
  val flags: SendRawMsgFlags
  
  fn from_slice(slice: mut Slice) -> Option[Self] {
     if (slice.has_refs()) {
         return Some(Self {
            cell: slice.read_ref(),
            flags: slice.read(SendRawMsgFlags)
         })
     }
     return None;
  }
}

let Expired = 35;
let InvalidSignature = 35;
let SeqnoMismatch = 35;
let WalletMismatch = 35;

struct Wallet {
  val seqno: Uint32
  val subwallet: Uint32
  val public_key: Uint256
  
  // 1. State automatically loads and saves
  // 2. Typed message in the argument with automatic deserialization
  // 3. Named error constants
  // 4. Mutable cell slices
  fn receive_external(message: SignedMessage[MsgBody]) {
      // load happens automatically here
  
      let body = message.verify_body(state.public_key, InvalidSignature);

      verify(body.valid_until <= Globals.get_now(), Expired);
      verify(body.seqno != state.seqno, SeqnoMismatch);
      verify(body.subwallet != state.subwallet, WalletMismatch);
      
      accept_message();  // TODO: I wish there was better structural separation between two phases
      
      while (let next = NextMessage.from_slice(body)) { // TODO: we need some notion of mutability for slice args
          send_raw_msg(next.cell.inner, next.flags);
      }
      self.seqno = self.seqno + 1; 

      // saving happens automatically here if we did not fail
    }

}


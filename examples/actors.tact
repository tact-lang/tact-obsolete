struct EmptyMsg {
    val value: Int32
    @derive impl Deserialize {}
}
actor Test {
    val test: Int32
    fn receive_internal(self: Self, msg: EmptyMsg) {

    }
}

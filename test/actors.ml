open Shared.Disabled

let%expect_test "Empty Methods Generated" =
  let source = {|
      actor Test { }
    |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ())
      (current_actor
       ((actor_name Test) (actor_fields ())
        (actor_receive_external
         ((function_signature ((function_params ()) (function_returns HoleType)))
          (function_impl (Fn (Block ())))))
        (actor_receive_internal
         ((function_signature ((function_params ()) (function_returns HoleType)))
          (function_impl (Fn (Block ())))))))
      (structs
       ((133
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 132)
          (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Recv Internal Ovveriding" =
  let source =
    {|
    struct EmptyMsg { @derive impl Deserialize {} }
    actor Test {
      fn receive_internal(self: Self, msg: EmptyMsg) {

      }
    }
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((EmptyMsg (Value (Type (StructType 133))))))
      (current_actor
       ((actor_name Test) (actor_fields ())
        (actor_receive_external
         ((function_signature ((function_params ()) (function_returns HoleType)))
          (function_impl (Fn (Block ())))))
        (actor_receive_internal
         ((function_signature
           ((function_params ((body (StructType 7))))
            (function_returns VoidType)))
          (function_impl
           (Fn
            (Block
             ((DestructuringLet
               ((destructuring_let ((value value) (slice slice)))
                (destructuring_let_expr
                 (FunctionCall
                  ((Value
                    (Function
                     ((function_signature
                       ((function_params ((s (StructType 7))))
                        (function_returns (StructType 134))))
                      (function_impl
                       (Fn
                        (Return
                         (FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params ((slice (StructType 7))))
                                (function_returns (StructType 135))))
                              (function_impl
                               (Fn
                                (Block
                                 ((Return
                                   (Value
                                    (Struct
                                     ((Value (Type (StructType 135)))
                                      ((slice (Reference (slice (StructType 7))))
                                       (value
                                        (Value
                                         (Struct
                                          ((Value (Type (StructType 133))) ()))))))))))))))))
                           ((Reference (s (StructType 7)))) false))))))))
                   ((Reference (body HoleType))) false)))
                (destructuring_let_rest false)))
              (Let
               ((state
                 (StructField
                  ((FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((slice (StructType 7))))
                          (function_returns (StructType 138))))
                        (function_impl
                         (Fn
                          (Block
                           ((Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 138)))
                                ((slice (Reference (slice (StructType 7))))
                                 (value
                                  (Value
                                   (Struct ((Value (Type (StructType 137))) ()))))))))))))))))
                     ((FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params ((cell (StructType 1))))
                             (function_returns (StructType 7))))
                           (function_impl
                            (Fn
                             (Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 7)))
                                 ((s
                                   (FunctionCall
                                    ((ResolvedReference
                                      (builtin_begin_parse <opaque>))
                                     ((StructField
                                       ((Reference (cell (StructType 1))) c
                                        (BuiltinType Cell))))
                                     false)))))))))))))
                        ((FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params ())
                                (function_returns (StructType 1))))
                              (function_impl
                               (Fn
                                (Return
                                 (Value
                                  (Struct
                                   ((Value (Type (StructType 1)))
                                    ((c
                                      (FunctionCall
                                       ((ResolvedReference
                                         (builtin_get_data <opaque>))
                                        () false)))))))))))))
                           () false)))
                        false)))
                     false))
                   value (StructType 137))))))
              (Let
               ((new_state
                 (FunctionCall
                  ((Value
                    (Function
                     ((function_signature
                       ((function_params
                         ((self (StructType 137)) (msg (StructType 133))))
                        (function_returns (StructType 137))))
                      (function_impl
                       (Fn (Block ((Return (Reference (self HoleType))))))))))
                   ((Reference (state (StructType 137)))
                    (Reference (value HoleType)))
                   false)))))
              (Expr
               (FunctionCall
                ((Value
                  (Function
                   ((function_signature
                     ((function_params ((state (StructType 1))))
                      (function_returns HoleType)))
                    (function_impl
                     (Fn
                      (Return
                       (FunctionCall
                        ((ResolvedReference (builtin_set_data <opaque>))
                         ((StructField
                           ((Reference (state (StructType 1))) c
                            (BuiltinType Cell))))
                         false))))))))
                 ((FunctionCall
                   ((Value
                     (Function
                      ((function_signature
                        ((function_params ((self (StructType 3))))
                         (function_returns (StructType 1))))
                       (function_impl
                        (Fn
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 1)))
                             ((c
                               (FunctionCall
                                ((ResolvedReference (builtin_end_cell <opaque>))
                                 ((StructField
                                   ((Reference (self (StructType 3))) inner
                                    (BuiltinType Builder))))
                                 false)))))))))))))
                    ((FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 137)) (b (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn (Block ((Return (Reference (b (StructType 3)))))))))))
                       ((Reference (new_state (StructType 137)))
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ())
                               (function_returns (StructType 3))))
                             (function_impl
                              (Fn
                               (Return
                                (Value
                                 (Struct
                                  ((Value (Type (StructType 3)))
                                   ((inner
                                     (FunctionCall
                                      ((ResolvedReference
                                        (builtin_begin_cell <opaque>))
                                       () false)))))))))))))
                          () false)))
                       false)))
                    false)))
                 false)))))))))))
      (structs
       ((138
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 137))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 137)) (s (StructType 7))))
                  (function_returns (StructType 138))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 138)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 137)))))))))))))))
            (uty_impls ()) (uty_id 138) (uty_base_id -500)))))
        (137
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((receive_internal
               ((function_signature
                 ((function_params
                   ((self (StructType 137)) (msg (StructType 133))))
                  (function_returns HoleType)))
                (function_impl (Fn (Block ())))))))
            (uty_impls ()) (uty_id 137) (uty_base_id 136)))))
        (135
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 133))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 133)) (s (StructType 7))))
                  (function_returns (StructType 135))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 135)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 133)))))))))))))))
            (uty_impls ()) (uty_id 135) (uty_base_id -500)))))
        (134
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 133))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 133)) (s (StructType 7))))
                  (function_returns (StructType 134))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 134)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 133)))))))))))))))
            (uty_impls ()) (uty_id 134) (uty_base_id -500)))))
        (133
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 134))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((slice (StructType 7))))
                          (function_returns (StructType 135))))
                        (function_impl
                         (Fn
                          (Block
                           ((Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 135)))
                                ((slice (Reference (slice (StructType 7))))
                                 (value
                                  (Value
                                   (Struct ((Value (Type (StructType 133))) ()))))))))))))))))
                     ((Reference (s (StructType 7)))) false)))))))))
            (uty_impls
             (((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 134))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params ((slice (StructType 7))))
                             (function_returns (StructType 135))))
                           (function_impl
                            (Fn
                             (Block
                              ((Return
                                (Value
                                 (Struct
                                  ((Value (Type (StructType 135)))
                                   ((slice (Reference (slice (StructType 7))))
                                    (value
                                     (Value
                                      (Struct
                                       ((Value (Type (StructType 133))) ()))))))))))))))))
                        ((Reference (s (StructType 7)))) false))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (2
        (((st_sig_fields ())
          (st_sig_methods
           ((receive_internal
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 55)))))
                (msg (StructType 133))))
              (function_returns HoleType)))))
          (st_sig_base_id 136) (st_sig_id 55))
         ((st_sig_fields ())
          (st_sig_methods
           ((deserialize
             ((function_params ((s (StructType 7))))
              (function_returns
               (TypeCall
                (func
                 (Value
                  (Function
                   ((function_signature
                     ((function_is_type) (function_params ((T (TypeN 0))))
                      (function_returns (StructSig 0))))
                    (function_impl (BuiltinFn (<fun> <opaque>)))))))
                (args
                 ((Value (Type (ExprType (Reference (Self (StructSig 54))))))))))))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Actors Codegen" =
  let source =
    {|
      struct EmptyMsg { @derive impl Deserialize {} }
      actor Test {
        fn receive_internal(self: Self, msg: EmptyMsg) {

        }
      }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    forall A -> A get1(tuple t) asm "1 INDEX";
    int builtin_or(int i1, int i2) impure {
      return _|_(i1, i2);
    }
    int builtin_and(int i1, int i2) impure {
      return _&_(i1, i2);
    }
    int builtin_gt(int i1, int i2) impure {
      return _>_(i1, i2);
    }
    int builtin_geq(int i1, int i2) impure {
      return _>=_(i1, i2);
    }
    int builtin_lt(int i1, int i2) impure {
      return _<_(i1, i2);
    }
    int builtin_leq(int i1, int i2) impure {
      return _<=_(i1, i2);
    }
    int builtin_neq(int i1, int i2) impure {
      return _!=_(i1, i2);
    }
    int builtin_eq(int i1, int i2) impure {
      return _==_(i1, i2);
    }
    int builtin_bit_or(int i1, int i2) impure {
      return _|_(i1, i2);
    }
    int builtin_bit_and(int i1, int i2) impure {
      return _&_(i1, i2);
    }
    int builtin_div(int i1, int i2) impure {
      return _/_(i1, i2);
    }
    int builtin_mul(int i1, int i2) impure {
      return _*_(i1, i2);
    }
    int builtin_sub(int i1, int i2) impure {
      return _-_(i1, i2);
    }
    int builtin_add(int i1, int i2) impure {
      return _+_(i1, i2);
    }
    int builtin_not(int c) impure {
      return func_bit_not(c);
    }
    forall A, B -> B believe_me(A i) asm "NOP";
    _ builtin_accept_message() impure {
      return accept_message();
    }
    int builtin_slice_hash(slice s) impure {
      return slice_hash(s);
    }
    int builtin_check_signature(int h, slice s, int k) impure {
      return check_signature(h, s, k);
    }
    int builtin_block_lt() impure {
      return block_lt();
    }
    int builtin_cur_lt() impure {
      return cur_lt();
    }
    _ builtin_get_balance() impure {
      return get_balance();
    }
    slice builtin_my_address() impure {
      return my_address();
    }
    int builtin_now() impure {
      return now();
    }
    _ builtin_set_data(cell d) impure {
      return set_data(d);
    }
    cell builtin_get_data() impure {
      return get_data();
    }
    _ builtin_throw(int e) impure {
      return throw(e);
    }
    _ builtin_send_raw_message(cell c, int f) impure {
      return send_raw_message(c, f);
    }
    (int, int) builtin_divmod(int i1, int i2) impure {
      return divmod(i1, i2);
    }
    _ builtin_end_parse(slice s) impure {
      return end_parse(s);
    }
    int builtin_slice_bits(slice s) impure {
      return slice_bits(s);
    }
    int builtin_slice_refs(slice s) impure {
      return slice_refs(s);
    }
    slice builtin_slice_last(slice s, int l) impure {
      return slice_last(s, l);
    }
    (slice, cell) builtin_load_ref(slice s) impure {
      return load_ref(s);
    }
    (slice, int) builtin_load_grams(slice s) impure {
      return load_grams(s);
    }
    (slice, slice) builtin_load_bits(slice s, int bs) impure {
      return load_bits(s, bs);
    }
    (slice, int) builtin_load_uint(slice s, int bs) impure {
      return load_uint(s, bs);
    }
    (slice, int) builtin_load_int(slice s, int bs) impure {
      return load_int(s, bs);
    }
    slice builtin_begin_parse(cell c) impure {
      return begin_parse(c);
    }
    int builtin_builder_depth(builder b) impure {
      return builder_depth(b);
    }
    int builtin_builder_refs(builder b) impure {
      return builder_refs(b);
    }
    int builtin_builder_bits(builder b) impure {
      return builder_bits(b);
    }
    builder builtin_store_maybe_ref(builder b, cell c) impure {
      return store_maybe_ref(b, c);
    }
    builder builtin_store_slice(builder b, slice s) impure {
      return store_slice(b, s);
    }
    builder builtin_store_ref(builder b, cell c) impure {
      return store_ref(b, c);
    }
    builder builtin_store_grams(builder b, int c) impure {
      return store_grams(b, c);
    }
    builder builtin_store_uint(builder b, int i, int bs) impure {
      return store_uint(b, i, bs);
    }
    builder builtin_store_int(builder b, int i, int bs) impure {
      return store_int(b, i, bs);
    }
    cell builtin_end_cell(builder b) impure {
      return end_cell(b);
    }
    builder builtin_begin_cell() impure {
      return begin_cell();
    }
    _ thrown(int n) impure {
      return builtin_throw(n);
    }
    _ send_raw_msg(cell msg, int flags) impure {
      return builtin_send_raw_message(msg, flags);
    }
    int f0(int i) impure {
      return i;
    }
    int hash_of_slice(slice s) impure {
      return f0(builtin_slice_hash(s));
    }
    int is_signature_valid(int hash, slice sign, int pubkey) impure {
      return builtin_check_signature(hash, sign, pubkey);
    }
    [slice, []] f2(slice slice_) impure {
      return [slice_, []];
    }
    [slice, []] f1(slice s) impure {
      return f2(s);
    }
    cell f3() impure {
      return builtin_get_data();
    }
    slice f4(cell cell_) impure {
      return builtin_begin_parse(cell_);
    }
    [slice, []] f5(slice slice_) impure {
      return [slice_, []];
    }
    [] f6([] self, [] msg) impure {
      return self;
    }
    builder f7() impure {
      return builtin_begin_cell();
    }
    builder f8([] self, builder b) impure {
      return b;
    }
    cell f9(builder self) impure {
      return builtin_end_cell(self);
    }
    _ f10(cell state) impure {
      return builtin_set_data(state);
    }
    _ recv_internal(slice body) impure {
      [slice slice_, [] value] = f1(body);
      [] state = get1(func_believe_me(f5(f4(f3()))));
      [] new_state = f6(state, value);
      f10(f9(f8(new_state, f7())));
    }
    _ recv_external() impure {
    } |}]

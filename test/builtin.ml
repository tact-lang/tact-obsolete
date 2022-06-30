open Shared

let%expect_test "Int(bits) constructor" =
  let source =
    {|
      let i = Int(257).new(100);
      let overflow = Int(8).new(513);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((overflow (Value (Struct (23 ((value (Value (Integer 513))))))))
        (i (Value (Struct (55 ((value (Value (Integer 100))))))))))
      (structs
       ((55
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 55))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (55 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 55)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 55))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 8))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (8
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (55
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 55))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (55 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType -1))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 55)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 55))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType -3))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 6))))
                     (function_returns (StructType 8))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 6)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (8
                            ((slice
                              (StructField
                               ((Reference (res (StructType 5))) slice
                                (StructType 6))))
                             (value
                              (Value
                               (Struct
                                (55
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 5))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 55))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (55 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 55)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "Int(bits) serializer" =
  let source =
    {|
      fn test(b: Builder) {
        let i = Int(32).new(100);
        i.serialize(b);
      }
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((b (StructType 3)))) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((i
                    (FunctionCall
                     ((ResolvedReference (new <opaque>)) ((Value (Integer 100))))))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (serialize <opaque>))
                    ((Reference (i (StructType 42)))
                     (Reference (b (StructType 3))))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "demo struct serializer" =
  let source =
    {|
      struct T {
        val a: Int(32)
        val b: Int(16)
      }
      let T_serializer = serializer(T);

      fn test() {
        let b = Builder.new();
        T_serializer(T{a: Int(32).new(0), b: Int(16).new(1)}, b);
      }
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((b (FunctionCall ((ResolvedReference (new <opaque>)) ())))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (T_serializer <opaque>))
                    ((Value
                      (Struct
                       (57
                        ((a
                          (FunctionCall
                           ((ResolvedReference (new <opaque>))
                            ((Value (Integer 0))))))
                         (b
                          (FunctionCall
                           ((ResolvedReference (new <opaque>))
                            ((Value (Integer 1))))))))))
                     (Reference (b (StructType 3))))))))))))))))
        (T_serializer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 57)) (b (StructType 3))))
              (function_returns (StructType 3))))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 42)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 42))) value
                                  IntegerType))
                                (Value (Integer 32))))))))))))
                      ((StructField
                        ((Reference (self (StructType 57))) a (StructType 42)))
                       (Reference (b (StructType 3)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 55)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 55))) value
                                  IntegerType))
                                (Value (Integer 16))))))))))))
                      ((StructField
                        ((Reference (self (StructType 57))) b (StructType 55)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (T (Value (Type (StructType 57))))))
      (structs
       ((57
         ((struct_fields
           ((a ((field_type (StructType 42))))
            (b ((field_type (StructType 55))))))
          (struct_methods ()) (struct_impls ()) (struct_id 57)))
        (55
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 55))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (55 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 55)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 55))) value IntegerType))
                     (Value (Integer 16)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 8))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                   (Return
                    (Value
                     (Struct
                      (8
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (55
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 55))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (55 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType -1))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 55)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 55))) value IntegerType))
                          (Value (Integer 16))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType -3))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 6))))
                     (function_returns (StructType 8))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 6)))
                              (Value (Integer 16))))))))
                        (Return
                         (Value
                          (Struct
                           (8
                            ((slice
                              (StructField
                               ((Reference (res (StructType 5))) slice
                                (StructType 6))))
                             (value
                              (Value
                               (Struct
                                (55
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 5))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 55))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (55 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 55)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "from interface" =
  let source =
    {|
      struct Value {
        val a: Integer
        impl From(Integer) {
          fn from(x: Integer) -> Self {
            Self{a: x}
          }
        }
      }
      fn check(y: Value) { y }

      let var = check(10);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((var (Value (Struct (56 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 56))))
              (function_returns (StructType 56))))
            (function_impl (Fn ((Return (Reference (y (StructType 56)))))))))))
        (Value (Value (Type (StructType 56))))))
      (structs
       ((56
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 56))))
              (function_impl
               (Fn
                ((Return (Value (Struct (56 ((a (Reference (x IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns (StructType 56))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value (Struct (56 ((a (Reference (x IntegerType))))))))))))))))))))
          (struct_id 56)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "tensor2" =
  let source =
    {|
    fn test() {
      let x = builtin_divmod(10, 2);
    }
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Let
                ((x
                  (FunctionCall
                   ((ResolvedReference (builtin_divmod <opaque>))
                    ((Value (Integer 10)) (Value (Integer 2)))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))
      |}]

let%expect_test "slice api" =
  let source =
    {|
      fn test(cell: Cell) {
        let slice = Slice.parse(cell);
        let result = slice.load_int(10);
        let slice2: Slice = result.slice;
        let int: Integer = result.value;
      }
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((cell (StructType 1))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((slice
                    (FunctionCall
                     ((ResolvedReference (parse <opaque>))
                      ((Reference (cell (StructType 1)))))))))
                 (Let
                  ((result
                    (FunctionCall
                     ((ResolvedReference (load_int <opaque>))
                      ((Reference (slice (StructType 6))) (Value (Integer 10))))))))
                 (Let
                  ((slice2
                    (FunctionCall
                     ((MkFunction
                       ((function_signature
                         ((function_params ((v (StructType 6))))
                          (function_returns (StructType 6))))
                        (function_impl
                         (Fn
                          ((Return
                            (StructField
                             ((Reference (result (StructType 5))) slice
                              (StructType 6)))))))))
                      ((StructField
                        ((Reference (result (StructType 5))) slice
                         (StructType 6)))))))))
                 (Let
                  ((int
                    (FunctionCall
                     ((MkFunction
                       ((function_signature
                         ((function_params ((v IntegerType)))
                          (function_returns IntegerType)))
                        (function_impl
                         (Fn
                          ((Return
                            (StructField
                             ((Reference (result (StructType 5))) value
                              IntegerType))))))))
                      ((StructField
                        ((Reference (result (StructType 5))) value IntegerType)))))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "demo serializer interface derivation" =
  let source =
    {|
      let T = derive_serialize(struct {
        val a: Int(32)
        val b: Int(16)
      });

      let U = derive_serialize(union {
         case T
      });

      fn test() {
        let b = Builder.new();
        let t = T{a: Int(32).new(0), b: Int(16).new(1)};
        let u: U = t;
        let b = t.serialize(b);
        let b = u.serialize(b);

      }
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((b (FunctionCall ((ResolvedReference (new <opaque>)) ())))))
                 (Let
                  ((t
                    (Value
                     (Struct
                      (57
                       ((a
                         (FunctionCall
                          ((ResolvedReference (new <opaque>))
                           ((Value (Integer 0))))))
                        (b
                         (FunctionCall
                          ((ResolvedReference (new <opaque>))
                           ((Value (Integer 1)))))))))))))
                 (Let
                  ((u
                    (Value
                     (UnionVariant
                      ((Struct
                        (57
                         ((a (Value (Struct (42 ((value (Value (Integer 0))))))))
                          (b (Value (Struct (55 ((value (Value (Integer 1)))))))))))
                       59))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (serialize <opaque>))
                      ((ResolvedReference (t <opaque>))
                       (Reference (b (StructType 3)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (serialize <opaque>))
                      ((ResolvedReference (u <opaque>))
                       (Reference (b (StructType 3))))))))))))))))))
        (U (Value (Type (UnionType 59)))) (T (Value (Type (StructType 57))))))
      (structs
       ((57
         ((struct_fields
           ((a ((field_type (StructType 42))))
            (b ((field_type (StructType 55))))))
          (struct_methods
           ((serialize
             ((function_signature
               ((function_params ((self (StructType 57)) (b (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((b
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self (StructType 42)) (builder (StructType 3))))
                             (function_returns (StructType 3))))
                           (function_impl
                            (Fn
                             ((Return
                               (FunctionCall
                                ((ResolvedReference (serialize_int <opaque>))
                                 ((Reference (builder (StructType 3)))
                                  (StructField
                                   ((Reference (self (StructType 42))) value
                                    IntegerType))
                                  (Value (Integer 32))))))))))))
                        ((StructField
                          ((Reference (self (StructType 57))) a (StructType 42)))
                         (Reference (b (StructType 3)))))))))
                   (Let
                    ((b
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self (StructType 55)) (builder (StructType 3))))
                             (function_returns (StructType 3))))
                           (function_impl
                            (Fn
                             ((Return
                               (FunctionCall
                                ((ResolvedReference (serialize_int <opaque>))
                                 ((Reference (builder (StructType 3)))
                                  (StructField
                                   ((Reference (self (StructType 55))) value
                                    IntegerType))
                                  (Value (Integer 16))))))))))))
                        ((StructField
                          ((Reference (self (StructType 57))) b (StructType 55)))
                         (Reference (b (StructType 3)))))))))
                   (Return (Reference (b (StructType 3)))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType -1))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 57)) (b (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((b
                           (FunctionCall
                            ((Value
                              (Function
                               ((function_signature
                                 ((function_params
                                   ((self (StructType 42))
                                    (builder (StructType 3))))
                                  (function_returns (StructType 3))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (FunctionCall
                                     ((ResolvedReference
                                       (serialize_int <opaque>))
                                      ((Reference (builder (StructType 3)))
                                       (StructField
                                        ((Reference (self (StructType 42))) value
                                         IntegerType))
                                       (Value (Integer 32))))))))))))
                             ((StructField
                               ((Reference (self (StructType 57))) a
                                (StructType 42)))
                              (Reference (b (StructType 3)))))))))
                        (Let
                         ((b
                           (FunctionCall
                            ((Value
                              (Function
                               ((function_signature
                                 ((function_params
                                   ((self (StructType 55))
                                    (builder (StructType 3))))
                                  (function_returns (StructType 3))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (FunctionCall
                                     ((ResolvedReference
                                       (serialize_int <opaque>))
                                      ((Reference (builder (StructType 3)))
                                       (StructField
                                        ((Reference (self (StructType 55))) value
                                         IntegerType))
                                       (Value (Integer 16))))))))))))
                             ((StructField
                               ((Reference (self (StructType 57))) b
                                (StructType 55)))
                              (Reference (b (StructType 3)))))))))
                        (Return (Reference (b (StructType 3))))))))))))))))))
          (struct_id 57)))
        (55
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 55))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (55 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 55)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 55))) value IntegerType))
                     (Value (Integer 16)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 8))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                   (Return
                    (Value
                     (Struct
                      (8
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (55
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 55))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (55 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType -1))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 55)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 55))) value IntegerType))
                          (Value (Integer 16))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType -3))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 6))))
                     (function_returns (StructType 8))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 6)))
                              (Value (Integer 16))))))))
                        (Return
                         (Value
                          (Struct
                           (8
                            ((slice
                              (StructField
                               ((Reference (res (StructType 5))) slice
                                (StructType 6))))
                             (value
                              (Value
                               (Struct
                                (55
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 5))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 55))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (55 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 55)))))
      (unions
       ((59
         ((cases (((StructType 57) (Discriminator 0))))
          (union_methods
           ((serialize
             ((function_signature
               ((function_params ((self (UnionType 59)) (b (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Switch
                  ((switch_condition (Reference (self (UnionType 59))))
                   (branches
                    (((branch_ty (StructType 57)) (branch_var var)
                      (branch_stmt
                       (Block
                        ((Let
                          ((b
                            (Primitive
                             (StoreInt
                              (builder
                               (StructField
                                ((Reference (b (StructType 3))) b
                                 (BuiltinType Builder))))
                              (length (Value (Integer 0)))
                              (integer (Value (Integer 0))) (signed false))))))
                         (Let
                          ((b
                            (FunctionCall
                             ((Value
                               (Function
                                ((function_signature
                                  ((function_params
                                    ((self (StructType 57)) (b (StructType 3))))
                                   (function_returns (StructType 3))))
                                 (function_impl
                                  (Fn
                                   ((Block
                                     ((Let
                                       ((b
                                         (FunctionCall
                                          ((Value
                                            (Function
                                             ((function_signature
                                               ((function_params
                                                 ((self (StructType 42))
                                                  (builder (StructType 3))))
                                                (function_returns (StructType 3))))
                                              (function_impl
                                               (Fn
                                                ((Return
                                                  (FunctionCall
                                                   ((ResolvedReference
                                                     (serialize_int <opaque>))
                                                    ((Reference
                                                      (builder (StructType 3)))
                                                     (StructField
                                                      ((Reference
                                                        (self (StructType 42)))
                                                       value IntegerType))
                                                     (Value (Integer 32))))))))))))
                                           ((StructField
                                             ((Reference (self (StructType 57)))
                                              a (StructType 42)))
                                            (Reference (b (StructType 3)))))))))
                                      (Let
                                       ((b
                                         (FunctionCall
                                          ((Value
                                            (Function
                                             ((function_signature
                                               ((function_params
                                                 ((self (StructType 55))
                                                  (builder (StructType 3))))
                                                (function_returns (StructType 3))))
                                              (function_impl
                                               (Fn
                                                ((Return
                                                  (FunctionCall
                                                   ((ResolvedReference
                                                     (serialize_int <opaque>))
                                                    ((Reference
                                                      (builder (StructType 3)))
                                                     (StructField
                                                      ((Reference
                                                        (self (StructType 55)))
                                                       value IntegerType))
                                                     (Value (Integer 16))))))))))))
                                           ((StructField
                                             ((Reference (self (StructType 57)))
                                              b (StructType 55)))
                                            (Reference (b (StructType 3)))))))))
                                      (Return (Reference (b (StructType 3))))))))))))
                              ((Reference (var (StructType 57)))
                               (Reference (b (StructType 3)))))))))
                         (Return (Reference (b (StructType 3)))))))))))))))))))
          (union_impls
           (((impl_interface (Value (Type (InterfaceType -1))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((self (UnionType 59)) (b (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Switch
                       ((switch_condition (Reference (self (UnionType 59))))
                        (branches
                         (((branch_ty (StructType 57)) (branch_var var)
                           (branch_stmt
                            (Block
                             ((Let
                               ((b
                                 (Primitive
                                  (StoreInt
                                   (builder
                                    (StructField
                                     ((Reference (b (StructType 3))) b
                                      (BuiltinType Builder))))
                                   (length (Value (Integer 0)))
                                   (integer (Value (Integer 0))) (signed false))))))
                              (Let
                               ((b
                                 (FunctionCall
                                  ((Value
                                    (Function
                                     ((function_signature
                                       ((function_params
                                         ((self (StructType 57))
                                          (b (StructType 3))))
                                        (function_returns (StructType 3))))
                                      (function_impl
                                       (Fn
                                        ((Block
                                          ((Let
                                            ((b
                                              (FunctionCall
                                               ((Value
                                                 (Function
                                                  ((function_signature
                                                    ((function_params
                                                      ((self (StructType 42))
                                                       (builder (StructType 3))))
                                                     (function_returns
                                                      (StructType 3))))
                                                   (function_impl
                                                    (Fn
                                                     ((Return
                                                       (FunctionCall
                                                        ((ResolvedReference
                                                          (serialize_int
                                                           <opaque>))
                                                         ((Reference
                                                           (builder
                                                            (StructType 3)))
                                                          (StructField
                                                           ((Reference
                                                             (self
                                                              (StructType 42)))
                                                            value IntegerType))
                                                          (Value (Integer 32))))))))))))
                                                ((StructField
                                                  ((Reference
                                                    (self (StructType 57)))
                                                   a (StructType 42)))
                                                 (Reference (b (StructType 3)))))))))
                                           (Let
                                            ((b
                                              (FunctionCall
                                               ((Value
                                                 (Function
                                                  ((function_signature
                                                    ((function_params
                                                      ((self (StructType 55))
                                                       (builder (StructType 3))))
                                                     (function_returns
                                                      (StructType 3))))
                                                   (function_impl
                                                    (Fn
                                                     ((Return
                                                       (FunctionCall
                                                        ((ResolvedReference
                                                          (serialize_int
                                                           <opaque>))
                                                         ((Reference
                                                           (builder
                                                            (StructType 3)))
                                                          (StructField
                                                           ((Reference
                                                             (self
                                                              (StructType 55)))
                                                            value IntegerType))
                                                          (Value (Integer 16))))))))))))
                                                ((StructField
                                                  ((Reference
                                                    (self (StructType 57)))
                                                   b (StructType 55)))
                                                 (Reference (b (StructType 3)))))))))
                                           (Return
                                            (Reference (b (StructType 3))))))))))))
                                   ((Reference (var (StructType 57)))
                                    (Reference (b (StructType 3)))))))))
                              (Return (Reference (b (StructType 3))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 60))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 57)))))))
                     (function_returns (UnionType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 57))))))
                         59)))))))))))))))
          (union_id 59)))))
      (interfaces
       ((60
         ((interface_methods
           ((from
             ((function_params ((from (StructType 57))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)))
|}]

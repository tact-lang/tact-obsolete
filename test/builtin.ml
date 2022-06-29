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
       ((overflow (Value (Struct (27 ((value (Value (Integer 513))))))))
        (i (Value (Struct (59 ((value (Value (Integer 100))))))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
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
                    ((Reference (i (StructType 46)))
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
                       (61
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
             ((function_params ((self (StructType 61)) (b (StructType 3))))
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
                            ((self (StructType 46)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 46))) value
                                  IntegerType))
                                (Value (Integer 32))))))))))))
                      ((StructField
                        ((Reference (self (StructType 61))) a (StructType 46)))
                       (Reference (b (StructType 3)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 59)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 59))) value
                                  IntegerType))
                                (Value (Integer 16))))))))))))
                      ((StructField
                        ((Reference (self (StructType 61))) b (StructType 59)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (T (Value (Type (StructType 61))))))
      (structs
       ((61
         ((struct_fields
           ((a ((field_type (StructType 46))))
            (b ((field_type (StructType 59))))))
          (struct_methods ()) (struct_impls ()) (struct_id 61)))
        (59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 16)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 16))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 16))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 16))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
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
       ((var (Value (Struct (60 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 60))))
              (function_returns (StructType 60))))
            (function_impl (Fn ((Return (Reference (y (StructType 60)))))))))))
        (Value (Value (Type (StructType 60))))))
      (structs
       ((60
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 60))))
              (function_impl
               (Fn
                ((Return (Value (Struct (60 ((a (Reference (x IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns (StructType 60))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value (Struct (60 ((a (Reference (x IntegerType))))))))))))))))))))
          (struct_id 60)))))
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
                      ((Reference (slice (StructType 7))) (Value (Integer 10))))))))
                 (Let
                  ((slice2
                    (FunctionCall
                     ((MkFunction
                       ((function_signature
                         ((function_params ((v (StructType 7))))
                          (function_returns (StructType 7))))
                        (function_impl
                         (Fn
                          ((Return
                            (StructField
                             ((Reference (result (StructType 6))) slice
                              (StructType 7)))))))))
                      ((StructField
                        ((Reference (result (StructType 6))) slice
                         (StructType 7)))))))))
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
                             ((Reference (result (StructType 6))) value
                              IntegerType))))))))
                      ((StructField
                        ((Reference (result (StructType 6))) value IntegerType)))))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

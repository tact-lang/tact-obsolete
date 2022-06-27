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
       ((overflow (Value (Struct (17 ((value (Value (Integer 513))))))))
        (i (Value (Struct (38 ((value (Value (Integer 100))))))))))
      (structs
       ((38
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 38))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (38 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 38)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 38))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 38))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (38 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 5))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 38))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (38 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 38)))))
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
                    ((Reference (i (StructType 32)))
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
                       (40
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
             ((function_params ((self (StructType 40)) (b (StructType 3))))
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
                            ((self (StructType 32)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (FunctionCall
                                 ((ResolvedReference (serialize_int <opaque>))
                                  ((Reference (builder (StructType 3)))
                                   (StructField
                                    ((Reference (self (StructType 32))) value
                                     IntegerType))
                                   (Value (Integer 32)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 40))) a (StructType 32)))
                       (Reference (b (StructType 3)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 38)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (FunctionCall
                                 ((ResolvedReference (serialize_int <opaque>))
                                  ((Reference (builder (StructType 3)))
                                   (StructField
                                    ((Reference (self (StructType 38))) value
                                     IntegerType))
                                   (Value (Integer 16)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 40))) b (StructType 38)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (T (Value (Type (StructType 40))))))
      (structs
       ((40
         ((struct_fields
           ((a ((field_type (StructType 32))))
            (b ((field_type (StructType 38))))))
          (struct_methods ()) (struct_impls ()) (struct_id 40)))
        (38
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 38))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (38 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 38)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 38))) value IntegerType))
                        (Value (Integer 16))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 38))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (38 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 5))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 38))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (38 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 38)))))
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
       ((var (Value (Struct (39 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 39))))
              (function_returns (StructType 39))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (y (StructType 39))))))))))))))
        (Value (Value (Type (StructType 39))))))
      (structs
       ((39
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 39))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (39 ((a (Reference (x IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 5))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns (StructType 39))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value (Struct (39 ((a (Reference (x IntegerType)))))))))))))))))))))))
          (struct_id 39)))))
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
              ((Block
                ((Let
                  ((x
                    (FunctionCall
                     ((ResolvedReference (builtin_divmod <opaque>))
                      ((Value (Integer 10)) (Value (Integer 2)))))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))
      |}]

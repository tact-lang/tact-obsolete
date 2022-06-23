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
       ((overflow (Value (Struct (14 ((value (Value (Integer 513))))))))
        (i (Value (Struct (30 ((value (Value (Integer 100))))))))))
      (structs
       ((30
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 30))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (30 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 30)) (builder (StructType 3))))
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
                         ((Reference (self (StructType 30))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 30))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (30 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface
              (Value
               (Type
                (InterfaceType
                 ((interface_methods
                   ((from
                     ((function_params ((from IntegerType)))
                      (function_returns SelfType))))))))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 30))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (30 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 30)))))
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
                  ((i (Value (Struct (25 ((value (Value (Integer 100))))))))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (serialize <opaque>))
                    ((ResolvedReference (i <opaque>))
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
                       (32
                        ((a (Value (Struct (25 ((value (Value (Integer 0))))))))
                         (b (Value (Struct (30 ((value (Value (Integer 1))))))))))))
                     (Reference (b (StructType 3))))))))))))))))
        (T_serializer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 32)) (b (StructType 3))))
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
                            ((self (StructType 25)) (builder (StructType 3))))
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
                                    ((Reference (self (StructType 25))) value
                                     IntegerType))
                                   (Value (Integer 32)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 32))) a (StructType 25)))
                       (Reference (b (StructType 3)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 30)) (builder (StructType 3))))
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
                                    ((Reference (self (StructType 30))) value
                                     IntegerType))
                                   (Value (Integer 16)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 32))) b (StructType 30)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (T (Value (Type (StructType 32))))))
      (structs
       ((32
         ((struct_fields
           ((a ((field_type (StructType 25))))
            (b ((field_type (StructType 30))))))
          (struct_methods ()) (struct_impls ()) (struct_id 32)))
        (30
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 30))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (30 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 30)) (builder (StructType 3))))
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
                         ((Reference (self (StructType 30))) value IntegerType))
                        (Value (Integer 16))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 30))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (30 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface
              (Value
               (Type
                (InterfaceType
                 ((interface_methods
                   ((from
                     ((function_params ((from IntegerType)))
                      (function_returns SelfType))))))))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 30))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (30 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 30)))))
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
       ((var (Value (Struct (31 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 31))))
              (function_returns (StructType 31))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (y (StructType 31))))))))))))))
        (Value (Value (Type (StructType 31))))))
      (structs
       ((31
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 31))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (31 ((a (Reference (x IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface
              (Value
               (Type
                (InterfaceType
                 ((interface_methods
                   ((from
                     ((function_params ((from IntegerType)))
                      (function_returns SelfType))))))))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns (StructType 31))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value (Struct (31 ((a (Reference (x IntegerType)))))))))))))))))))))))
          (struct_id 31)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

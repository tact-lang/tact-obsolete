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
       ((overflow (Value (Struct (6 ((value (Value (Integer 513))))))))
        (i (Value (Struct (5 ((value (Value (Integer 100))))))))))
      (structs
       ((6
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 6))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (6 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 6)) (builder (StructType 1))))
                (function_returns (StructType 1))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 1)))
                        (StructField
                         ((Reference (self (StructType 6))) value IntegerType))
                        (Value (Integer 8))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 6))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (6 ((value (Reference (i IntegerType))))))))))))))))))
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
                     (function_returns (StructType 6))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (6 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 6)))
        (5
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 5))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (5 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 5)) (builder (StructType 1))))
                (function_returns (StructType 1))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 1)))
                        (StructField
                         ((Reference (self (StructType 5))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 5))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (5 ((value (Reference (i IntegerType))))))))))))))))))
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
                     (function_returns (StructType 5))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (5 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 5)))))
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
             ((function_params ((b (StructType 1)))) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let ((i (Value (Struct (5 ((value (Value (Integer 100))))))))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (serialize <opaque>))
                    ((ResolvedReference (i <opaque>))
                     (Reference (b (StructType 1))))))))))))))))))
      (structs
       ((5
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 5))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (5 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 5)) (builder (StructType 1))))
                (function_returns (StructType 1))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 1)))
                        (StructField
                         ((Reference (self (StructType 5))) value IntegerType))
                        (Value (Integer 32))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 5))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (5 ((value (Reference (i IntegerType))))))))))))))))))
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
                     (function_returns (StructType 5))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (5 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 5)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
                       (8
                        ((a (Value (Struct (5 ((value (Value (Integer 0))))))))
                         (b (Value (Struct (6 ((value (Value (Integer 1))))))))))))
                     (Reference (b (StructType 1))))))))))))))))
        (T_serializer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 8)) (b (StructType 1))))
              (function_returns (StructType 1))))
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
                            ((self (StructType 5)) (builder (StructType 1))))
                           (function_returns (StructType 1))))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (FunctionCall
                                 ((ResolvedReference (serialize_int <opaque>))
                                  ((Reference (builder (StructType 1)))
                                   (StructField
                                    ((Reference (self (StructType 5))) value
                                     IntegerType))
                                   (Value (Integer 32)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 8))) a (StructType 5)))
                       (Reference (b (StructType 1)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 6)) (builder (StructType 1))))
                           (function_returns (StructType 1))))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (FunctionCall
                                 ((ResolvedReference (serialize_int <opaque>))
                                  ((Reference (builder (StructType 1)))
                                   (StructField
                                    ((Reference (self (StructType 6))) value
                                     IntegerType))
                                   (Value (Integer 16)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 8))) b (StructType 6)))
                       (Reference (b (StructType 1)))))))))
                 (Return (Reference (b (StructType 1)))))))))))))
        (T (Value (Type (StructType 8))))))
      (structs
       ((8
         ((struct_fields
           ((a ((field_type (StructType 5)))) (b ((field_type (StructType 6))))))
          (struct_methods ()) (struct_impls ()) (struct_id 8)))
        (6
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 6))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (6 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 6)) (builder (StructType 1))))
                (function_returns (StructType 1))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 1)))
                        (StructField
                         ((Reference (self (StructType 6))) value IntegerType))
                        (Value (Integer 16))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 6))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (6 ((value (Reference (i IntegerType))))))))))))))))))
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
                     (function_returns (StructType 6))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (6 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 6)))
        (5
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 5))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (5 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 5)) (builder (StructType 1))))
                (function_returns (StructType 1))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 1)))
                        (StructField
                         ((Reference (self (StructType 5))) value IntegerType))
                        (Value (Integer 32))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 5))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (5 ((value (Reference (i IntegerType))))))))))))))))))
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
                     (function_returns (StructType 5))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (5 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 5)))))
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
       ((var (Value (Struct (6 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 6))))
              (function_returns (StructType 6))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (y (StructType 6))))))))))))))
        (Value (Value (Type (StructType 6))))))
      (structs
       ((6
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 6))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr (Value (Struct (6 ((a (Reference (x IntegerType))))))))))))))))))
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
                     (function_returns (StructType 6))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value (Struct (6 ((a (Reference (x IntegerType)))))))))))))))))))))))
          (struct_id 6)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

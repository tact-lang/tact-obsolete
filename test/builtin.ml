open Shared

let%expect_test "Int(bits) constructor" =
  let source =
    {|
            let i = Int(257).new(100);
            let overflow = Int(8).new(513);
          |}
  in
  pp_compile source ;
  [%expect
    {|
             (Ok
              ((bindings
                ((overflow
                  (Value
                   (Struct
                    ((Value (Type (StructType 26))) ((value (Value (Integer 513))))))))
                 (i
                  (Value
                   (Struct
                    ((Value (Type (StructType 83))) ((value (Value (Integer 100))))))))))
               (structs
                ((84
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (StructType 83))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (StructType 83))))
                         (function_returns (StructType 84))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 84)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (StructType 83))))))))))))))))
                   (struct_impls ()) (struct_id 84) (struct_base_id -500)))
                 (83
                  ((struct_fields ((value ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 83))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 83)))
                              ((value (Reference (i IntegerType)))))))))))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 83)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 83))) value IntegerType))
                              (Value (Integer 257)))))))))))
                     (deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 84))))
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
                               ((Value (Type (StructType 84)))
                                ((slice
                                  (StructField
                                   ((Reference (res (StructType 5))) slice (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 83)))
                                     ((value
                                       (StructField
                                        ((Reference (res (StructType 5))) value
                                         IntegerType))))))))))))))))))))
                     (from
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 83))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 83)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (struct_impls
                    (((impl_interface -1)
                      (impl_methods
                       ((serialize
                         ((function_signature
                           ((function_params
                             ((self (StructType 83)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 83))) value IntegerType))
                                 (Value (Integer 257))))))))))))))
                     ((impl_interface -3)
                      (impl_methods
                       ((deserialize
                         ((function_signature
                           ((function_params ((s (StructType 6))))
                            (function_returns (StructType 84))))
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
                                  ((Value (Type (StructType 84)))
                                   ((slice
                                     (StructField
                                      ((Reference (res (StructType 5))) slice
                                       (StructType 6))))
                                    (value
                                     (Value
                                      (Struct
                                       ((Value (Type (StructType 83)))
                                        ((value
                                          (StructField
                                           ((Reference (res (StructType 5))) value
                                            IntegerType)))))))))))))))))))))))
                     ((impl_interface 10)
                      (impl_methods
                       ((from
                         ((function_signature
                           ((function_params ((i IntegerType)))
                            (function_returns (StructType 83))))
                          (function_impl
                           (Fn
                            ((Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 83)))
                                 ((value (Reference (i IntegerType))))))))))))))))))
                   (struct_id 83) (struct_base_id 9)))))
               (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
               (union_signs
                (5
                 (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
                   (un_sig_base_id 77))
                  ((un_sig_cases ((StructType 55))) (un_sig_methods ())
                   (un_sig_base_id 60))
                  ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
                   (un_sig_base_id 44))
                  ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
                   (un_sig_base_id 38))
                  ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
                   (un_sig_base_id 20))))))) |}]

let%expect_test "Int(bits) serializer" =
  let source =
    {|
            fn test(b: Builder) {
              let i = Int(32).new(100);
              i.serialize(b);
            }
          |}
  in
  pp_compile source ;
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
                             (Value
                              (Struct
                               ((Value (Type (StructType 52)))
                                ((value (Value (Integer 100))))))))))
                          (Return
                           (FunctionCall
                            ((ResolvedReference (serialize <opaque>))
                             ((Reference (i (StructType 52)))
                              (Reference (b (StructType 3))))))))))))))))))
               (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
               (struct_signs (0 ()))
               (union_signs
                (5
                 (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
                   (un_sig_base_id 77))
                  ((un_sig_cases ((StructType 55))) (un_sig_methods ())
                   (un_sig_base_id 60))
                  ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
                   (un_sig_base_id 44))
                  ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
                   (un_sig_base_id 38))
                  ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
                   (un_sig_base_id 20))))))) |}]

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
  pp_compile source ;
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
                          (Return
                           (FunctionCall
                            ((ResolvedReference (T_serializer <opaque>))
                             ((Value
                               (Struct
                                ((Value (Type (StructType 86)))
                                 ((a
                                   (Value
                                    (Struct
                                     ((Value (Type (StructType 52)))
                                      ((value (Value (Integer 0))))))))
                                  (b
                                   (Value
                                    (Struct
                                     ((Value (Type (StructType 83)))
                                      ((value (Value (Integer 1))))))))))))
                              (Reference (b (StructType 3))))))))))))))))
                 (T_serializer
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((self (StructType 86)) (b (StructType 3))))
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
                                     ((self (StructType 52)) (builder (StructType 3))))
                                    (function_returns (StructType 3))))
                                  (function_impl
                                   (Fn
                                    ((Return
                                      (FunctionCall
                                       ((ResolvedReference (serialize_int <opaque>))
                                        ((Reference (builder (StructType 3)))
                                         (StructField
                                          ((Reference (self (StructType 52))) value
                                           IntegerType))
                                         (Value (Integer 32))))))))))))
                               ((StructField
                                 ((Reference (self (StructType 86))) a (StructType 52)))
                                (Reference (b (StructType 3)))))))))
                          (Let
                           ((b
                             (FunctionCall
                              ((Value
                                (Function
                                 ((function_signature
                                   ((function_params
                                     ((self (StructType 83)) (builder (StructType 3))))
                                    (function_returns (StructType 3))))
                                  (function_impl
                                   (Fn
                                    ((Return
                                      (FunctionCall
                                       ((ResolvedReference (serialize_int <opaque>))
                                        ((Reference (builder (StructType 3)))
                                         (StructField
                                          ((Reference (self (StructType 83))) value
                                           IntegerType))
                                         (Value (Integer 16))))))))))))
                               ((StructField
                                 ((Reference (self (StructType 86))) b (StructType 83)))
                                (Reference (b (StructType 3)))))))))
                          (Return (Reference (b (StructType 3)))))))))))))
                 (T (Value (Type (StructType 86))))))
               (structs
                ((86
                  ((struct_fields
                    ((a ((field_type (StructType 52))))
                     (b ((field_type (StructType 83))))))
                   (struct_methods ()) (struct_impls ()) (struct_id 86)
                   (struct_base_id 85)))
                 (84
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (StructType 83))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (StructType 83))))
                         (function_returns (StructType 84))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 84)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (StructType 83))))))))))))))))
                   (struct_impls ()) (struct_id 84) (struct_base_id -500)))
                 (83
                  ((struct_fields ((value ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 83))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 83)))
                              ((value (Reference (i IntegerType)))))))))))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 83)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 83))) value IntegerType))
                              (Value (Integer 16)))))))))))
                     (deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 84))))
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
                               ((Value (Type (StructType 84)))
                                ((slice
                                  (StructField
                                   ((Reference (res (StructType 5))) slice (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 83)))
                                     ((value
                                       (StructField
                                        ((Reference (res (StructType 5))) value
                                         IntegerType))))))))))))))))))))
                     (from
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 83))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 83)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (struct_impls
                    (((impl_interface -1)
                      (impl_methods
                       ((serialize
                         ((function_signature
                           ((function_params
                             ((self (StructType 83)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 83))) value IntegerType))
                                 (Value (Integer 16))))))))))))))
                     ((impl_interface -3)
                      (impl_methods
                       ((deserialize
                         ((function_signature
                           ((function_params ((s (StructType 6))))
                            (function_returns (StructType 84))))
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
                                  ((Value (Type (StructType 84)))
                                   ((slice
                                     (StructField
                                      ((Reference (res (StructType 5))) slice
                                       (StructType 6))))
                                    (value
                                     (Value
                                      (Struct
                                       ((Value (Type (StructType 83)))
                                        ((value
                                          (StructField
                                           ((Reference (res (StructType 5))) value
                                            IntegerType)))))))))))))))))))))))
                     ((impl_interface 10)
                      (impl_methods
                       ((from
                         ((function_signature
                           ((function_params ((i IntegerType)))
                            (function_returns (StructType 83))))
                          (function_impl
                           (Fn
                            ((Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 83)))
                                 ((value (Reference (i IntegerType))))))))))))))))))
                   (struct_id 83) (struct_base_id 9)))))
               (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
               (union_signs
                (5
                 (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
                   (un_sig_base_id 77))
                  ((un_sig_cases ((StructType 55))) (un_sig_methods ())
                   (un_sig_base_id 60))
                  ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
                   (un_sig_base_id 44))
                  ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
                   (un_sig_base_id 38))
                  ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
                   (un_sig_base_id 20))))))) |}]

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
  pp_compile source ;
  [%expect
    {|
             (Ok
              ((bindings
                ((var
                  (Value
                   (Struct ((Value (Type (StructType 84))) ((a (Value (Integer 10))))))))
                 (check
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((y (StructType 84))))
                       (function_returns (StructType 84))))
                     (function_impl (Fn ((Return (Reference (y (StructType 84)))))))))))
                 (Value (Value (Type (StructType 84))))))
               (structs
                ((84
                  ((struct_fields ((a ((field_type IntegerType)))))
                   (struct_methods
                    ((from
                      ((function_signature
                        ((function_params ((x IntegerType)))
                         (function_returns (StructType 84))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 84)))
                              ((a (Reference (x IntegerType)))))))))))))))
                   (struct_impls
                    (((impl_interface 10)
                      (impl_methods
                       ((from
                         ((function_signature
                           ((function_params ((x IntegerType)))
                            (function_returns (StructType 84))))
                          (function_impl
                           (Fn
                            ((Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 84)))
                                 ((a (Reference (x IntegerType))))))))))))))))))
                   (struct_id 84) (struct_base_id 83)))))
               (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
               (union_signs
                (5
                 (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
                   (un_sig_base_id 77))
                  ((un_sig_cases ((StructType 55))) (un_sig_methods ())
                   (un_sig_base_id 60))
                  ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
                   (un_sig_base_id 44))
                  ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
                   (un_sig_base_id 38))
                  ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
                   (un_sig_base_id 20))))))) |}]

let%expect_test "tensor2" =
  let source =
    {|
            fn test() {
              let x = builtin_divmod(10, 2);
            }
          |}
  in
  pp_compile source ;
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
               (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
               (struct_signs (0 ()))
               (union_signs
                (5
                 (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
                   (un_sig_base_id 77))
                  ((un_sig_cases ((StructType 55))) (un_sig_methods ())
                   (un_sig_base_id 60))
                  ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
                   (un_sig_base_id 44))
                  ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
                   (un_sig_base_id 38))
                  ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
                   (un_sig_base_id 20))))))) |}]

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
  pp_compile source ;
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
               (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
               (struct_signs (0 ()))
               (union_signs
                (5
                 (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
                   (un_sig_base_id 77))
                  ((un_sig_cases ((StructType 55))) (un_sig_methods ())
                   (un_sig_base_id 60))
                  ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
                   (un_sig_base_id 44))
                  ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
                   (un_sig_base_id 38))
                  ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
                   (un_sig_base_id 20))))))) |}]

(* module Config = struct
     include Tact.Located.Disabled
   end

   module Syntax = Tact.Syntax.Make (Config)
   module Parser = Tact.Parser.Make (Config)
   module Lang = Tact.Lang.Make (Config)
   module Show = Tact.Show.Make (Config)
   module Interpreter = Tact.Interpreter
   module Errors = Tact.Errors
   module Zint = Tact.Zint
   module C = Tact.Compiler
   module Codegen = Tact.Codegen_func
   module Func = Tact.Func
   include Core

   type error = [Lang.error | Interpreter.error] [@@deriving sexp_of]

   let make_errors e = new Errors.errors e

   let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

   let strip_if_exists_in_other o1 o2 ~equal =
     List.filter o1 ~f:(fun o1_item -> not @@ List.exists o2 ~f:(equal o1_item))

   let strip : program:Lang.program -> previous:Lang.program -> Lang.program =
    fun ~program ~previous ->
     { program with
       bindings =
         strip_if_exists_in_other program.bindings previous.bindings
           ~equal:Lang.equal_binding;
       structs =
         strip_if_exists_in_other program.structs previous.structs
           ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
       unions =
         strip_if_exists_in_other program.unions previous.unions
           ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
       interfaces =
         strip_if_exists_in_other program.interfaces previous.interfaces
           ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
       struct_signs =
         Lang.Arena.strip_if_exists program.struct_signs previous.struct_signs }

   let compile_pass p prev_program errors =
     let c = new Lang.constructor ~program:prev_program errors in
     let p' = c#visit_program () p in
     p'

   let build_program ?(errors = make_errors Show.show_error)
       ?(prev_program = Lang.default_program ()) ?(_strip_defaults = true) ~codegen
       p =
     (* let std =
          let c = new Lang.constructor ~program:prev_program errors in
          let p' = c#visit_program () (parse_program Tact.Builtin.std) in
          p'
        in
        (* This will make a deep copy of the std. Lang.constructor mutates input program,
           so we need deep copy of an std if we want to strip std bindings later. *)
        let std_copy = {std with bindings = std.bindings} in *)
     let p' = compile_pass p (Lang.default_program ()) errors in
     let p' = strip ~program:p' ~previous:prev_program in
     errors#to_result p'
     |> Result.map_error ~f:(fun errors ->
            let errs = List.map errors ~f:(fun (_, err, _) -> err) in
            (errs, p') )
     |> Result.map ~f:codegen

   let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

   and sexp_of_errors =
     sexp_of_pair (List.sexp_of_t sexp_of_error) Lang.sexp_of_program

   and print_sexp e =
     pp_sexp (Result.sexp_of_t Lang.sexp_of_program sexp_of_errors e)

   let pp_compile ?(prev_program = Lang.default_program ())
       ?(_strip_defaults = true) s =
     parse_program s
     |> build_program ~prev_program ~_strip_defaults ~codegen:(fun x -> x)
     |> print_sexp

   let%expect_test "tensor2" =
     let source =
       {|
       struct Cell {
         val c: builtin_Cell
       }

       // Do not change place of builder struct - for internal reasons
       // it should be second struct in the file.
       struct Builder {
         val b: builtin_Builder

         fn new() -> Self {
           Self { b: builtin_builder_new() }
         }
         fn build(self: Self) -> Cell {
           let c = builtin_builder_build(self.b);
           Cell { c: c }
         }
         fn serialize_int(self: Self, int: Integer, bits: Integer) -> Self {
           let b = builtin_builder_store_int(self.b, int, bits);
           Self { b: b }
         }
         fn serialize_coins(self: Self, c: Integer) -> Self {
           let b = builtin_builder_store_coins(self.b, c);
           Self { b: b }
         }
       }

       struct Slice {
         val s: builtin_Slice

         fn parse(cell: Cell) -> Self {
           Self { s: builtin_slice_begin_parse(cell.c) }
         }

         fn load_int(self: Self, bits: Integer) -> LoadResult(Integer) {
           let output = builtin_slice_load_int(self.s, bits);
           let slice = Self { s: output.value1 };
           let int = output.value2;
           LoadResult(Integer) { slice: slice, value: int }
         }
       }

       struct Int(bits: Integer) {
         val value: Integer

         fn new(i: Integer) -> Self {
           Self { value: i }
         }

         impl Serialize {
           fn serialize(self: Self, builder: Builder) -> Builder {
             builder.serialize_int(self.value, bits)
           }
         }

         impl Deserialize {
           fn deserialize(s: Slice) -> LoadResult(Self) {
             let res = s.load_int(bits);

             LoadResult(Self) {
               slice: res.slice,
               value: Self { value: res.value }
             }
           }
         }

         impl From(Integer) {
           fn from(i: Integer) -> Self {
             Self { value: i }
           }
         }
       }

       struct AddressVar {
         val len: Int(9)
         val workchain_id: Int(8)
         val address: Integer

         impl Serialize {
           fn serialize(self: Self, b: Builder) -> Builder {
             let b = b.serialize_int(0, 0); // AnyCast
             let b = serializer(Self)(self, b);
             return b;
           }
         }

         impl Deserialize {
           fn deserialize(s: Slice) -> LoadResult(Self) {
             let res_anycast = s.load_int(1);
             if (builtin_equal(res_anycast.value, 0)) {
               let res_len = Int(9).deserialize(res_anycast.slice);
               let res_workchain = Int(8).deserialize(res_len.slice);
               let res_address = res_workchain.slice.load_int(res_len);
               return LoadResult(Self)
                 .new(res_address.slice, Self {
                   len: res_len.value,
                   workchain_id: res_workchain.value,
                   address: res_address.value,
                 });
             } else {
               /* TODO: Anycast is unsupported by TON for now, what we should do here? */
             }
           }
         }
       }

       union MsgAddressInt {
         case AddressVar
         impl Deserialize {
           fn deserialize(s: Slice) -> LoadResult(Self) {
             let res_discr = s.load_int(1);
             let res_addr = AddressVar.deserialize(res_discr.slice);
             let s = res_addr.slice;
           }
         }
       }

           |}
     in
     pp_compile source ;
     [%expect
       {|
             (Ok
              ((bindings
                ((MsgAddressInt (Value (Type (UnionType 18))))
                 (AddressVar (Value (Type (StructType 14))))
                 (Int
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((bits IntegerType)))
                       (function_returns (StructSig 4))))
                     (function_impl
                      (Fn
                       ((Return
                         (MkStructDef
                          ((mk_struct_fields
                            ((value (ResolvedReference (Integer <opaque>)))))
                           (mk_methods
                            ((new
                              (MkFunction
                               ((function_signature
                                 ((function_params ((i IntegerType)))
                                  (function_returns
                                   (ExprType (Reference (Self (StructSig 4)))))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Value
                                     (Struct
                                      ((Reference (Self (StructSig 4)))
                                       ((value (Reference (i IntegerType))))))))))))))
                             (serialize
                              (MkFunction
                               ((function_signature
                                 ((function_params
                                   ((self (ExprType (Reference (Self (StructSig 4)))))
                                    (builder (StructType 3))))
                                  (function_returns (StructType 3))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (FunctionCall
                                     ((ResolvedReference (serialize_int <opaque>))
                                      ((Reference (builder (StructType 3)))
                                       (StructField
                                        ((Reference
                                          (self
                                           (ExprType (Reference (Self (StructSig 4))))))
                                         value IntegerType))
                                       (Reference (bits IntegerType))))))))))))
                             (deserialize
                              (MkFunction
                               ((function_signature
                                 ((function_params ((s (StructType 6))))
                                  (function_returns
                                   (ExprType
                                    (FunctionCall
                                     ((ResolvedReference (LoadResult <opaque>))
                                      ((Value
                                        (Type (ExprType (Reference (Self (StructSig 4)))))))))))))
                                (function_impl
                                 (Fn
                                  ((Block
                                    ((Let
                                      ((res
                                        (FunctionCall
                                         ((ResolvedReference (load_int <opaque>))
                                          ((Reference (s (StructType 6)))
                                           (Reference (bits IntegerType))))))))
                                     (Return
                                      (Value
                                       (Struct
                                        ((FunctionCall
                                          ((ResolvedReference (LoadResult <opaque>))
                                           ((Reference (Self (StructSig 4))))))
                                         ((slice
                                           (StructField
                                            ((Reference (res (StructType 5))) slice
                                             (StructType 6))))
                                          (value
                                           (Value
                                            (Struct
                                             ((Reference (Self (StructSig 4)))
                                              ((value
                                                (StructField
                                                 ((Reference (res (StructType 5))) value
                                                  IntegerType)))))))))))))))))))))
                             (from
                              (MkFunction
                               ((function_signature
                                 ((function_params ((i IntegerType)))
                                  (function_returns
                                   (ExprType (Reference (Self (StructSig 4)))))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Value
                                     (Struct
                                      ((Reference (Self (StructSig 4)))
                                       ((value (Reference (i IntegerType))))))))))))))))
                           (mk_impls
                            (((mk_impl_interface (ResolvedReference (Serialize <opaque>)))
                              (mk_impl_methods
                               ((serialize
                                 (MkFunction
                                  ((function_signature
                                    ((function_params
                                      ((self (ExprType (Reference (Self (StructSig 4)))))
                                       (builder (StructType 3))))
                                     (function_returns (StructType 3))))
                                   (function_impl
                                    (Fn
                                     ((Return
                                       (FunctionCall
                                        ((ResolvedReference (serialize_int <opaque>))
                                         ((Reference (builder (StructType 3)))
                                          (StructField
                                           ((Reference
                                             (self
                                              (ExprType (Reference (Self (StructSig 4))))))
                                            value IntegerType))
                                          (Reference (bits IntegerType)))))))))))))))
                             ((mk_impl_interface
                               (ResolvedReference (Deserialize <opaque>)))
                              (mk_impl_methods
                               ((deserialize
                                 (MkFunction
                                  ((function_signature
                                    ((function_params ((s (StructType 6))))
                                     (function_returns
                                      (ExprType
                                       (FunctionCall
                                        ((ResolvedReference (LoadResult <opaque>))
                                         ((Value
                                           (Type
                                            (ExprType (Reference (Self (StructSig 4)))))))))))))
                                   (function_impl
                                    (Fn
                                     ((Block
                                       ((Let
                                         ((res
                                           (FunctionCall
                                            ((ResolvedReference (load_int <opaque>))
                                             ((Reference (s (StructType 6)))
                                              (Reference (bits IntegerType))))))))
                                        (Return
                                         (Value
                                          (Struct
                                           ((FunctionCall
                                             ((ResolvedReference (LoadResult <opaque>))
                                              ((Reference (Self (StructSig 4))))))
                                            ((slice
                                              (StructField
                                               ((Reference (res (StructType 5))) slice
                                                (StructType 6))))
                                             (value
                                              (Value
                                               (Struct
                                                ((Reference (Self (StructSig 4)))
                                                 ((value
                                                   (StructField
                                                    ((Reference (res (StructType 5)))
                                                     value IntegerType))))))))))))))))))))))))
                             ((mk_impl_interface (Value (Type (InterfaceType 8))))
                              (mk_impl_methods
                               ((from
                                 (MkFunction
                                  ((function_signature
                                    ((function_params ((i IntegerType)))
                                     (function_returns
                                      (ExprType (Reference (Self (StructSig 4)))))))
                                   (function_impl
                                    (Fn
                                     ((Return
                                       (Value
                                        (Struct
                                         ((Reference (Self (StructSig 4)))
                                          ((value (Reference (i IntegerType)))))))))))))))))))
                           (mk_struct_id 7) (mk_struct_sig 4)))))))))))
                 (Slice (Value (Type (StructType 6))))
                 (Builder (Value (Type (StructType 3))))
                 (Cell (Value (Type (StructType 1))))
                 (LoadResult
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((T (TypeN 0)))) (function_returns (StructSig 0))))
                     (function_impl (BuiltinFn (<fun> <opaque>)))))))))
               (structs
                ((19
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (UnionType 18))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (UnionType 18))))
                         (function_returns (StructType 19))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 19)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (UnionType 18))))))))))))))))
                   (struct_impls ()) (struct_id 19) (struct_base_id -500)))
                 (16
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (StructType 14))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (StructType 14))))
                         (function_returns (StructType 16))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 16)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (StructType 14))))))))))))))))
                   (struct_impls ()) (struct_id 16) (struct_base_id -500)))
                 (15
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (StructType 14))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (StructType 14))))
                         (function_returns (StructType 15))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 15)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (StructType 14))))))))))))))))
                   (struct_impls ()) (struct_id 15) (struct_base_id -500)))
                 (14
                  ((struct_fields
                    ((len ((field_type (StructType 9))))
                     (workchain_id ((field_type (StructType 11))))
                     (address ((field_type IntegerType)))))
                   (struct_methods
                    ((serialize
                      ((function_signature
                        ((function_params ((self (StructType 14)) (b (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((b
                               (FunctionCall
                                ((ResolvedReference (serialize_int <opaque>))
                                 ((Reference (b (StructType 3))) (Value (Integer 0))
                                  (Value (Integer 0))))))))
                            (Let
                             ((b
                               (FunctionCall
                                ((Value
                                  (Function
                                   ((function_signature
                                     ((function_params
                                       ((self (StructType 14)) (b (StructType 3))))
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
                                                    ((self (StructType 9))
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
                                                           (self (StructType 9)))
                                                          value IntegerType))
                                                        (Value (Integer 9))))))))))))
                                              ((StructField
                                                ((Reference (self (StructType 14))) len
                                                 (StructType 9)))
                                               (Reference (b (StructType 3)))))))))
                                         (Let
                                          ((b
                                            (FunctionCall
                                             ((Value
                                               (Function
                                                ((function_signature
                                                  ((function_params
                                                    ((self (StructType 11))
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
                                                           (self (StructType 11)))
                                                          value IntegerType))
                                                        (Value (Integer 8))))))))))))
                                              ((StructField
                                                ((Reference (self (StructType 14)))
                                                 workchain_id (StructType 11)))
                                               (Reference (b (StructType 3)))))))))
                                         (Return (Reference (b (StructType 3))))))))))))
                                 ((Reference (self (StructType 14)))
                                  (Reference (b (StructType 3)))))))))
                            (Return (Reference (b (StructType 3)))))))))))
                     (deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 15))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((res_anycast
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 6))) (Value (Integer 1))))))))
                            (Break
                             (If
                              ((if_condition
                                (FunctionCall
                                 ((ResolvedReference (builtin_equal <opaque>))
                                  ((StructField
                                    ((Reference (res_anycast (StructType 5))) value
                                     IntegerType))
                                   (Value (Integer 0))))))
                               (if_then
                                (Block
                                 ((Let
                                   ((res_len
                                     (FunctionCall
                                      ((ResolvedReference (deserialize <opaque>))
                                       ((StructField
                                         ((Reference (res_anycast (StructType 5))) slice
                                          (StructType 6)))))))))
                                  (Let
                                   ((res_workchain
                                     (FunctionCall
                                      ((ResolvedReference (deserialize <opaque>))
                                       ((StructField
                                         ((Reference (res_len (StructType 10))) slice
                                          (StructType 6)))))))))
                                  (Let
                                   ((res_address
                                     (FunctionCall
                                      ((ResolvedReference (load_int <opaque>))
                                       ((StructField
                                         ((Reference (res_workchain (StructType 12)))
                                          slice (StructType 6)))
                                        (Reference (res_len (StructType 10)))))))))
                                  (Return
                                   (FunctionCall
                                    ((Value
                                      (Function
                                       ((function_signature
                                         ((function_params
                                           ((s (StructType 6)) (v (StructType 14))))
                                          (function_returns (StructType 16))))
                                        (function_impl
                                         (Fn
                                          ((Return
                                            (Value
                                             (Struct
                                              ((Value (Type (StructType 16)))
                                               ((slice (Reference (s (StructType 6))))
                                                (value (Reference (v (StructType 14)))))))))))))))
                                     ((StructField
                                       ((Reference (res_address (StructType 5))) slice
                                        (StructType 6)))
                                      (Value
                                       (Struct
                                        ((Value (Type (StructType 14)))
                                         ((len
                                           (StructField
                                            ((Reference (res_len (StructType 10))) value
                                             (StructType 9))))
                                          (workchain_id
                                           (StructField
                                            ((Reference (res_workchain (StructType 12)))
                                             value (StructType 11))))
                                          (address
                                           (StructField
                                            ((Reference (res_address (StructType 5)))
                                             value IntegerType))))))))))))))
                               (if_else ((Block ())))))))))))))))
                   (struct_impls
                    (((impl_interface -1)
                      (impl_methods
                       ((serialize
                         ((function_signature
                           ((function_params ((self (StructType 14)) (b (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Block
                              ((Let
                                ((b
                                  (FunctionCall
                                   ((ResolvedReference (serialize_int <opaque>))
                                    ((Reference (b (StructType 3))) (Value (Integer 0))
                                     (Value (Integer 0))))))))
                               (Let
                                ((b
                                  (FunctionCall
                                   ((Value
                                     (Function
                                      ((function_signature
                                        ((function_params
                                          ((self (StructType 14)) (b (StructType 3))))
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
                                                       ((self (StructType 9))
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
                                                              (self (StructType 9)))
                                                             value IntegerType))
                                                           (Value (Integer 9))))))))))))
                                                 ((StructField
                                                   ((Reference (self (StructType 14))) len
                                                    (StructType 9)))
                                                  (Reference (b (StructType 3)))))))))
                                            (Let
                                             ((b
                                               (FunctionCall
                                                ((Value
                                                  (Function
                                                   ((function_signature
                                                     ((function_params
                                                       ((self (StructType 11))
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
                                                              (self (StructType 11)))
                                                             value IntegerType))
                                                           (Value (Integer 8))))))))))))
                                                 ((StructField
                                                   ((Reference (self (StructType 14)))
                                                    workchain_id (StructType 11)))
                                                  (Reference (b (StructType 3)))))))))
                                            (Return (Reference (b (StructType 3))))))))))))
                                    ((Reference (self (StructType 14)))
                                     (Reference (b (StructType 3)))))))))
                               (Return (Reference (b (StructType 3))))))))))))))
                     ((impl_interface -3)
                      (impl_methods
                       ((deserialize
                         ((function_signature
                           ((function_params ((s (StructType 6))))
                            (function_returns (StructType 15))))
                          (function_impl
                           (Fn
                            ((Block
                              ((Let
                                ((res_anycast
                                  (FunctionCall
                                   ((ResolvedReference (load_int <opaque>))
                                    ((Reference (s (StructType 6))) (Value (Integer 1))))))))
                               (Break
                                (If
                                 ((if_condition
                                   (FunctionCall
                                    ((ResolvedReference (builtin_equal <opaque>))
                                     ((StructField
                                       ((Reference (res_anycast (StructType 5))) value
                                        IntegerType))
                                      (Value (Integer 0))))))
                                  (if_then
                                   (Block
                                    ((Let
                                      ((res_len
                                        (FunctionCall
                                         ((ResolvedReference (deserialize <opaque>))
                                          ((StructField
                                            ((Reference (res_anycast (StructType 5)))
                                             slice (StructType 6)))))))))
                                     (Let
                                      ((res_workchain
                                        (FunctionCall
                                         ((ResolvedReference (deserialize <opaque>))
                                          ((StructField
                                            ((Reference (res_len (StructType 10))) slice
                                             (StructType 6)))))))))
                                     (Let
                                      ((res_address
                                        (FunctionCall
                                         ((ResolvedReference (load_int <opaque>))
                                          ((StructField
                                            ((Reference (res_workchain (StructType 12)))
                                             slice (StructType 6)))
                                           (Reference (res_len (StructType 10)))))))))
                                     (Return
                                      (FunctionCall
                                       ((Value
                                         (Function
                                          ((function_signature
                                            ((function_params
                                              ((s (StructType 6)) (v (StructType 14))))
                                             (function_returns (StructType 16))))
                                           (function_impl
                                            (Fn
                                             ((Return
                                               (Value
                                                (Struct
                                                 ((Value (Type (StructType 16)))
                                                  ((slice (Reference (s (StructType 6))))
                                                   (value (Reference (v (StructType 14)))))))))))))))
                                        ((StructField
                                          ((Reference (res_address (StructType 5))) slice
                                           (StructType 6)))
                                         (Value
                                          (Struct
                                           ((Value (Type (StructType 14)))
                                            ((len
                                              (StructField
                                               ((Reference (res_len (StructType 10)))
                                                value (StructType 9))))
                                             (workchain_id
                                              (StructField
                                               ((Reference
                                                 (res_workchain (StructType 12)))
                                                value (StructType 11))))
                                             (address
                                              (StructField
                                               ((Reference (res_address (StructType 5)))
                                                value IntegerType))))))))))))))
                                  (if_else ((Block ()))))))))))))))))))
                   (struct_id 14) (struct_base_id 13)))
                 (12
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (StructType 11))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (StructType 11))))
                         (function_returns (StructType 12))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 12)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (StructType 11))))))))))))))))
                   (struct_impls ()) (struct_id 12) (struct_base_id -500)))
                 (11
                  ((struct_fields ((value ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 11))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 11)))
                              ((value (Reference (i IntegerType)))))))))))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 11)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 11))) value IntegerType))
                              (Value (Integer 8)))))))))))
                     (deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 12))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((res
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 6))) (Value (Integer 8))))))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 12)))
                                ((slice
                                  (StructField
                                   ((Reference (res (StructType 5))) slice (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 11)))
                                     ((value
                                       (StructField
                                        ((Reference (res (StructType 5))) value
                                         IntegerType))))))))))))))))))))
                     (from
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 11))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 11)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (struct_impls
                    (((impl_interface -1)
                      (impl_methods
                       ((serialize
                         ((function_signature
                           ((function_params
                             ((self (StructType 11)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 11))) value IntegerType))
                                 (Value (Integer 8))))))))))))))
                     ((impl_interface -3)
                      (impl_methods
                       ((deserialize
                         ((function_signature
                           ((function_params ((s (StructType 6))))
                            (function_returns (StructType 12))))
                          (function_impl
                           (Fn
                            ((Block
                              ((Let
                                ((res
                                  (FunctionCall
                                   ((ResolvedReference (load_int <opaque>))
                                    ((Reference (s (StructType 6))) (Value (Integer 8))))))))
                               (Return
                                (Value
                                 (Struct
                                  ((Value (Type (StructType 12)))
                                   ((slice
                                     (StructField
                                      ((Reference (res (StructType 5))) slice
                                       (StructType 6))))
                                    (value
                                     (Value
                                      (Struct
                                       ((Value (Type (StructType 11)))
                                        ((value
                                          (StructField
                                           ((Reference (res (StructType 5))) value
                                            IntegerType)))))))))))))))))))))))
                     ((impl_interface 8)
                      (impl_methods
                       ((from
                         ((function_signature
                           ((function_params ((i IntegerType)))
                            (function_returns (StructType 11))))
                          (function_impl
                           (Fn
                            ((Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 11)))
                                 ((value (Reference (i IntegerType))))))))))))))))))
                   (struct_id 11) (struct_base_id 7)))
                 (10
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type (StructType 9))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v (StructType 9))))
                         (function_returns (StructType 10))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 10)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v (StructType 9))))))))))))))))
                   (struct_impls ()) (struct_id 10) (struct_base_id -500)))
                 (9
                  ((struct_fields ((value ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 9))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 9)))
                              ((value (Reference (i IntegerType)))))))))))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 9)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 9))) value IntegerType))
                              (Value (Integer 9)))))))))))
                     (deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 10))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((res
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 6))) (Value (Integer 9))))))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 10)))
                                ((slice
                                  (StructField
                                   ((Reference (res (StructType 5))) slice (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 9)))
                                     ((value
                                       (StructField
                                        ((Reference (res (StructType 5))) value
                                         IntegerType))))))))))))))))))))
                     (from
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 9))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 9)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (struct_impls
                    (((impl_interface -1)
                      (impl_methods
                       ((serialize
                         ((function_signature
                           ((function_params
                             ((self (StructType 9)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 9))) value IntegerType))
                                 (Value (Integer 9))))))))))))))
                     ((impl_interface -3)
                      (impl_methods
                       ((deserialize
                         ((function_signature
                           ((function_params ((s (StructType 6))))
                            (function_returns (StructType 10))))
                          (function_impl
                           (Fn
                            ((Block
                              ((Let
                                ((res
                                  (FunctionCall
                                   ((ResolvedReference (load_int <opaque>))
                                    ((Reference (s (StructType 6))) (Value (Integer 9))))))))
                               (Return
                                (Value
                                 (Struct
                                  ((Value (Type (StructType 10)))
                                   ((slice
                                     (StructField
                                      ((Reference (res (StructType 5))) slice
                                       (StructType 6))))
                                    (value
                                     (Value
                                      (Struct
                                       ((Value (Type (StructType 9)))
                                        ((value
                                          (StructField
                                           ((Reference (res (StructType 5))) value
                                            IntegerType)))))))))))))))))))))))
                     ((impl_interface 8)
                      (impl_methods
                       ((from
                         ((function_signature
                           ((function_params ((i IntegerType)))
                            (function_returns (StructType 9))))
                          (function_impl
                           (Fn
                            ((Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 9)))
                                 ((value (Reference (i IntegerType))))))))))))))))))
                   (struct_id 9) (struct_base_id 7)))
                 (6
                  ((struct_fields ((s ((field_type (BuiltinType Slice))))))
                   (struct_methods
                    ((parse
                      ((function_signature
                        ((function_params ((cell (StructType 1))))
                         (function_returns (StructType 6))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 6)))
                              ((s
                                (FunctionCall
                                 ((ResolvedReference (builtin_slice_begin_parse <opaque>))
                                  ((StructField
                                    ((Reference (cell (StructType 1))) c
                                     (BuiltinType Cell)))))))))))))))))
                     (load_int
                      ((function_signature
                        ((function_params ((self (StructType 6)) (bits IntegerType)))
                         (function_returns (StructType 5))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((output
                               (FunctionCall
                                ((ResolvedReference (builtin_slice_load_int <opaque>))
                                 ((StructField
                                   ((Reference (self (StructType 6))) s
                                    (BuiltinType Slice)))
                                  (Reference (bits IntegerType))))))))
                            (Let
                             ((slice
                               (Value
                                (Struct
                                 ((Value (Type (StructType 6)))
                                  ((s
                                    (StructField
                                     ((Reference (output (StructType -5))) value1
                                      (BuiltinType Slice)))))))))))
                            (Let
                             ((int
                               (StructField
                                ((Reference (output (StructType -5))) value2 IntegerType)))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 5)))
                                ((slice (Reference (slice (StructType 6))))
                                 (value (Reference (int IntegerType)))))))))))))))))
                   (struct_impls ()) (struct_id 6) (struct_base_id 4)))
                 (5
                  ((struct_fields
                    ((slice ((field_type (StructType 6))))
                     (value ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((s (StructType 6)) (v IntegerType)))
                         (function_returns (StructType 5))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 5)))
                              ((slice (Reference (s (StructType 6))))
                               (value (Reference (v IntegerType)))))))))))))))
                   (struct_impls ()) (struct_id 5) (struct_base_id -500)))
                 (3
                  ((struct_fields ((b ((field_type (BuiltinType Builder))))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ()) (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 3)))
                              ((b
                                (FunctionCall
                                 ((ResolvedReference (builtin_builder_new <opaque>)) ())))))))))))))
                     (build
                      ((function_signature
                        ((function_params ((self (StructType 3))))
                         (function_returns (StructType 1))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((c
                               (FunctionCall
                                ((ResolvedReference (builtin_builder_build <opaque>))
                                 ((StructField
                                   ((Reference (self (StructType 3))) b
                                    (BuiltinType Builder)))))))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 1)))
                                ((c (Reference (c (BuiltinType Cell))))))))))))))))
                     (serialize_int
                      ((function_signature
                        ((function_params
                          ((self (StructType 3)) (int IntegerType) (bits IntegerType)))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((b
                               (FunctionCall
                                ((ResolvedReference (builtin_builder_store_int <opaque>))
                                 ((StructField
                                   ((Reference (self (StructType 3))) b
                                    (BuiltinType Builder)))
                                  (Reference (int IntegerType))
                                  (Reference (bits IntegerType))))))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 3)))
                                ((b (Reference (b (BuiltinType Builder))))))))))))))))
                     (serialize_coins
                      ((function_signature
                        ((function_params ((self (StructType 3)) (c IntegerType)))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((b
                               (FunctionCall
                                ((ResolvedReference
                                  (builtin_builder_store_coins <opaque>))
                                 ((StructField
                                   ((Reference (self (StructType 3))) b
                                    (BuiltinType Builder)))
                                  (Reference (c IntegerType))))))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 3)))
                                ((b (Reference (b BoolType)))))))))))))))))
                   (struct_impls ()) (struct_id 3) (struct_base_id 2)))
                 (1
                  ((struct_fields ((c ((field_type (BuiltinType Cell))))))
                   (struct_methods ()) (struct_impls ()) (struct_id 1) (struct_base_id 0)))))
               (unions
                ((18
                  ((cases (((StructType 14) (Discriminator 0))))
                   (union_methods
                    ((deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 19))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Let
                             ((res_discr
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 6))) (Value (Integer 1))))))))
                            (Let
                             ((res_addr
                               (FunctionCall
                                ((ResolvedReference (deserialize <opaque>))
                                 ((StructField
                                   ((Reference (res_discr (StructType 5))) slice
                                    (StructType 6)))))))))
                            (Let
                             ((s
                               (StructField
                                ((Reference (res_addr (StructType 15))) slice
                                 (StructType 6)))))))))))))))
                   (union_impls
                    (((impl_interface -3)
                      (impl_methods
                       ((deserialize
                         ((function_signature
                           ((function_params ((s (StructType 6))))
                            (function_returns (StructType 19))))
                          (function_impl
                           (Fn
                            ((Block
                              ((Let
                                ((res_discr
                                  (FunctionCall
                                   ((ResolvedReference (load_int <opaque>))
                                    ((Reference (s (StructType 6))) (Value (Integer 1))))))))
                               (Let
                                ((res_addr
                                  (FunctionCall
                                   ((ResolvedReference (deserialize <opaque>))
                                    ((StructField
                                      ((Reference (res_discr (StructType 5))) slice
                                       (StructType 6)))))))))
                               (Let
                                ((s
                                  (StructField
                                   ((Reference (res_addr (StructType 15))) slice
                                    (StructType 6))))))))))))))))
                     ((impl_interface 20)
                      (impl_methods
                       ((from
                         ((function_signature
                           ((function_params ((v (StructType 14))))
                            (function_returns (UnionType 17))))
                          (function_impl
                           (Fn
                            ((Return
                              (MakeUnionVariant ((Reference (v (StructType 14))) 17)))))))))))))
                   (union_id 18) (union_base_id 17)))))
               (interfaces
                ((20
                  ((interface_methods
                    ((from
                      ((function_params ((from (StructType 14))))
                       (function_returns SelfType)))))))
                 (8
                  ((interface_methods
                    ((from
                      ((function_params ((from IntegerType))) (function_returns SelfType)))))))))
               (type_counter <opaque>) (memoized_fcalls <opaque>)
               (struct_signs
                (11
                 (((st_sig_fields
                    ((slice (Value (Type (StructType 6))))
                     (value (Value (Type (ExprType (Reference (Self (StructSig 6)))))))))
                   (st_sig_methods
                    ((new
                      ((function_params
                        ((s (StructType 6))
                         (v (ExprType (Reference (Self (StructSig 6)))))))
                       (function_returns
                        (ExprType
                         (FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params ((T (TypeN 0))))
                                (function_returns (StructSig 10))))
                              (function_impl (BuiltinFn (<fun> <opaque>))))))
                           ((Reference (Self (StructSig 6))))))))))))
                   (st_sig_base_id -500) (st_sig_id 11))
                  ((st_sig_fields
                    ((slice (Value (Type (StructType 6))))
                     (value (Value (Type (Dependent T (TypeN 0)))))))
                   (st_sig_methods
                    ((new
                      ((function_params ((s (StructType 6)) (v (Dependent T (TypeN 0)))))
                       (function_returns (StructSig 10))))))
                   (st_sig_base_id -500) (st_sig_id 10))
                  ((st_sig_fields
                    ((slice (Value (Type (StructType 6))))
                     (value (Value (Type (ExprType (Reference (Self (StructSig 6)))))))))
                   (st_sig_methods
                    ((new
                      ((function_params
                        ((s (StructType 6))
                         (v (ExprType (Reference (Self (StructSig 6)))))))
                       (function_returns
                        (ExprType
                         (FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params ((T (TypeN 0))))
                                (function_returns (StructSig 0))))
                              (function_impl (BuiltinFn (<fun> <opaque>))))))
                           ((Reference (Self (StructSig 6))))))))))))
                   (st_sig_base_id -500) (st_sig_id 9))
                  ((st_sig_fields
                    ((slice (Value (Type (StructType 6))))
                     (value (Value (Type (Dependent T (TypeN 0)))))))
                   (st_sig_methods
                    ((new
                      ((function_params ((s (StructType 6)) (v (Dependent T (TypeN 0)))))
                       (function_returns (StructSig 8))))))
                   (st_sig_base_id -500) (st_sig_id 8))
                  ((st_sig_fields
                    ((slice (Value (Type (StructType 6))))
                     (value (Value (Type (ExprType (Reference (Self (StructSig 6)))))))))
                   (st_sig_methods
                    ((new
                      ((function_params
                        ((s (StructType 6))
                         (v (ExprType (Reference (Self (StructSig 6)))))))
                       (function_returns
                        (ExprType
                         (FunctionCall
                          ((ResolvedReference (LoadResult <opaque>))
                           ((Reference (Self (StructSig 6))))))))))))
                   (st_sig_base_id -500) (st_sig_id 7))
                  ((st_sig_fields
                    ((len (Value (Type (StructType 9))))
                     (workchain_id (Value (Type (StructType 11))))
                     (address (Value (Type IntegerType)))))
                   (st_sig_methods
                    ((serialize
                      ((function_params
                        ((self
                          (ExprType
                           (Value (Type (ExprType (Reference (Self (StructSig 6))))))))
                         (b (StructType 3))))
                       (function_returns (StructType 3))))
                     (deserialize
                      ((function_params ((s (StructType 6))))
                       (function_returns
                        (ExprType
                         (FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params ((T (TypeN 0))))
                                (function_returns (StructSig 10))))
                              (function_impl (BuiltinFn (<fun> <opaque>))))))
                           ((Value (Type (ExprType (Reference (Self (StructSig 6)))))))))))))))
                   (st_sig_base_id 13) (st_sig_id 6))
                  ((st_sig_fields
                    ((slice (Value (Type (StructType 6))))
                     (value (Value (Type (ExprType (Reference (Self (StructSig 4)))))))))
                   (st_sig_methods
                    ((new
                      ((function_params
                        ((s (StructType 6))
                         (v (ExprType (Reference (Self (StructSig 4)))))))
                       (function_returns
                        (ExprType
                         (FunctionCall
                          ((ResolvedReference (LoadResult <opaque>))
                           ((Reference (Self (StructSig 4))))))))))))
                   (st_sig_base_id -500) (st_sig_id 5))
                  ((st_sig_fields ((value (ResolvedReference (Integer <opaque>)))))
                   (st_sig_methods
                    ((new
                      ((function_params ((i IntegerType)))
                       (function_returns
                        (ExprType
                         (Value (Type (ExprType (Reference (Self (StructSig 4))))))))))
                     (serialize
                      ((function_params
                        ((self
                          (ExprType
                           (Value (Type (ExprType (Reference (Self (StructSig 4))))))))
                         (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (deserialize
                      ((function_params ((s (StructType 6))))
                       (function_returns
                        (ExprType
                         (FunctionCall
                          ((ResolvedReference (LoadResult <opaque>))
                           ((Value (Type (ExprType (Reference (Self (StructSig 4)))))))))))))
                     (from
                      ((function_params ((i IntegerType)))
                       (function_returns
                        (ExprType
                         (Value (Type (ExprType (Reference (Self (StructSig 4))))))))))))
                   (st_sig_base_id 7) (st_sig_id 4))
                  ((st_sig_fields ((s (Value (Type (BuiltinType Slice))))))
                   (st_sig_methods
                    ((parse
                      ((function_params ((cell (StructType 1))))
                       (function_returns
                        (ExprType
                         (Value (Type (ExprType (Reference (Self (StructSig 3))))))))))
                     (load_int
                      ((function_params
                        ((self
                          (ExprType
                           (Value (Type (ExprType (Reference (Self (StructSig 3))))))))
                         (bits IntegerType)))
                       (function_returns (StructType 5))))))
                   (st_sig_base_id 4) (st_sig_id 3))
                  ((st_sig_fields ((b (Value (Type (BuiltinType Builder))))))
                   (st_sig_methods
                    ((new
                      ((function_params ())
                       (function_returns
                        (ExprType
                         (Value (Type (ExprType (Reference (Self (StructSig 2))))))))))
                     (build
                      ((function_params
                        ((self
                          (ExprType
                           (Value (Type (ExprType (Reference (Self (StructSig 2))))))))))
                       (function_returns (StructType 1))))
                     (serialize_int
                      ((function_params
                        ((self
                          (ExprType
                           (Value (Type (ExprType (Reference (Self (StructSig 2))))))))
                         (int IntegerType) (bits IntegerType)))
                       (function_returns
                        (ExprType
                         (Value (Type (ExprType (Reference (Self (StructSig 2))))))))))
                     (serialize_coins
                      ((function_params
                        ((self
                          (ExprType
                           (Value (Type (ExprType (Reference (Self (StructSig 2))))))))
                         (c IntegerType)))
                       (function_returns
                        (ExprType
                         (Value (Type (ExprType (Reference (Self (StructSig 2))))))))))))
                   (st_sig_base_id 2) (st_sig_id 2))
                  ((st_sig_fields ((c (Value (Type (BuiltinType Cell))))))
                   (st_sig_methods ()) (st_sig_base_id 0) (st_sig_id 1)))))
               (union_signs
                (1
                 (((un_sig_cases ((StructType 14))) (un_sig_methods ())
                   (un_sig_base_id 17))))))) |}] *)

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
                 ((Value (Type (StructType 25))) ((value (Value (Integer 513))))))))
              (i
               (Value
                (Struct
                 ((Value (Type (StructType 78))) ((value (Value (Integer 100))))))))))
            (structs
             ((79
               ((struct_fields
                 ((slice ((field_type (StructType 6))))
                  (value ((field_type (StructType 78))))))
                (struct_methods
                 ((new
                   ((function_signature
                     ((function_params ((s (StructType 6)) (v (StructType 78))))
                      (function_returns (StructType 79))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 79)))
                           ((slice (Reference (s (StructType 6))))
                            (value (Reference (v (StructType 78))))))))))))))))
                (struct_impls ()) (struct_id 79) (struct_base_id -500)))
              (78
               ((struct_fields ((value ((field_type IntegerType)))))
                (struct_methods
                 ((new
                   ((function_signature
                     ((function_params ((i IntegerType)))
                      (function_returns (StructType 78))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 78)))
                           ((value (Reference (i IntegerType)))))))))))))
                  (serialize
                   ((function_signature
                     ((function_params
                       ((self (StructType 78)) (builder (StructType 3))))
                      (function_returns (StructType 3))))
                    (function_impl
                     (Fn
                      ((Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 78))) value IntegerType))
                           (Value (Integer 257)))))))))))
                  (deserialize
                   ((function_signature
                     ((function_params ((s (StructType 6))))
                      (function_returns (StructType 79))))
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
                            ((Value (Type (StructType 79)))
                             ((slice
                               (StructField
                                ((Reference (res (StructType 5))) slice (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 78)))
                                  ((value
                                    (StructField
                                     ((Reference (res (StructType 5))) value
                                      IntegerType))))))))))))))))))))
                  (from
                   ((function_signature
                     ((function_params ((i IntegerType)))
                      (function_returns (StructType 78))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 78)))
                           ((value (Reference (i IntegerType)))))))))))))))
                (struct_impls
                 (((impl_interface -1)
                   (impl_methods
                    ((serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 78)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 78))) value IntegerType))
                              (Value (Integer 257))))))))))))))
                  ((impl_interface -3)
                   (impl_methods
                    ((deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 79))))
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
                               ((Value (Type (StructType 79)))
                                ((slice
                                  (StructField
                                   ((Reference (res (StructType 5))) slice
                                    (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 78)))
                                     ((value
                                       (StructField
                                        ((Reference (res (StructType 5))) value
                                         IntegerType)))))))))))))))))))))))
                  ((impl_interface 10)
                   (impl_methods
                    ((from
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 78))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 78)))
                              ((value (Reference (i IntegerType))))))))))))))))))
                (struct_id 78) (struct_base_id 9)))))
            (type_counter <opaque>) (memoized_fcalls <opaque>)
            (struct_signs ((current_id 1836) (items ())))
            (union_signs ((current_id 5) (items ()))))) |}]

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
                            ((Value (Type (StructType 49)))
                             ((value (Value (Integer 100))))))))))
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize <opaque>))
                          ((Reference (i (StructType 49)))
                           (Reference (b (StructType 3))))))))))))))))))
            (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
            (struct_signs ((current_id 1836) (items ())))
            (union_signs ((current_id 5) (items ()))))) |}]

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
                             ((Value (Type (StructType 81)))
                              ((a
                                (Value
                                 (Struct
                                  ((Value (Type (StructType 49)))
                                   ((value (Value (Integer 0))))))))
                               (b
                                (Value
                                 (Struct
                                  ((Value (Type (StructType 78)))
                                   ((value (Value (Integer 1))))))))))))
                           (Reference (b (StructType 3))))))))))))))))
              (T_serializer
               (Value
                (Function
                 ((function_signature
                   ((function_params ((self (StructType 81)) (b (StructType 3))))
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
                                  ((self (StructType 49)) (builder (StructType 3))))
                                 (function_returns (StructType 3))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (FunctionCall
                                    ((ResolvedReference (serialize_int <opaque>))
                                     ((Reference (builder (StructType 3)))
                                      (StructField
                                       ((Reference (self (StructType 49))) value
                                        IntegerType))
                                      (Value (Integer 32))))))))))))
                            ((StructField
                              ((Reference (self (StructType 81))) a (StructType 49)))
                             (Reference (b (StructType 3)))))))))
                       (Let
                        ((b
                          (FunctionCall
                           ((Value
                             (Function
                              ((function_signature
                                ((function_params
                                  ((self (StructType 78)) (builder (StructType 3))))
                                 (function_returns (StructType 3))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (FunctionCall
                                    ((ResolvedReference (serialize_int <opaque>))
                                     ((Reference (builder (StructType 3)))
                                      (StructField
                                       ((Reference (self (StructType 78))) value
                                        IntegerType))
                                      (Value (Integer 16))))))))))))
                            ((StructField
                              ((Reference (self (StructType 81))) b (StructType 78)))
                             (Reference (b (StructType 3)))))))))
                       (Return (Reference (b (StructType 3)))))))))))))
              (T (Value (Type (StructType 81))))))
            (structs
             ((81
               ((struct_fields
                 ((a ((field_type (StructType 49))))
                  (b ((field_type (StructType 78))))))
                (struct_methods ()) (struct_impls ()) (struct_id 81)
                (struct_base_id 80)))
              (79
               ((struct_fields
                 ((slice ((field_type (StructType 6))))
                  (value ((field_type (StructType 78))))))
                (struct_methods
                 ((new
                   ((function_signature
                     ((function_params ((s (StructType 6)) (v (StructType 78))))
                      (function_returns (StructType 79))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 79)))
                           ((slice (Reference (s (StructType 6))))
                            (value (Reference (v (StructType 78))))))))))))))))
                (struct_impls ()) (struct_id 79) (struct_base_id -500)))
              (78
               ((struct_fields ((value ((field_type IntegerType)))))
                (struct_methods
                 ((new
                   ((function_signature
                     ((function_params ((i IntegerType)))
                      (function_returns (StructType 78))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 78)))
                           ((value (Reference (i IntegerType)))))))))))))
                  (serialize
                   ((function_signature
                     ((function_params
                       ((self (StructType 78)) (builder (StructType 3))))
                      (function_returns (StructType 3))))
                    (function_impl
                     (Fn
                      ((Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 78))) value IntegerType))
                           (Value (Integer 16)))))))))))
                  (deserialize
                   ((function_signature
                     ((function_params ((s (StructType 6))))
                      (function_returns (StructType 79))))
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
                            ((Value (Type (StructType 79)))
                             ((slice
                               (StructField
                                ((Reference (res (StructType 5))) slice (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 78)))
                                  ((value
                                    (StructField
                                     ((Reference (res (StructType 5))) value
                                      IntegerType))))))))))))))))))))
                  (from
                   ((function_signature
                     ((function_params ((i IntegerType)))
                      (function_returns (StructType 78))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 78)))
                           ((value (Reference (i IntegerType)))))))))))))))
                (struct_impls
                 (((impl_interface -1)
                   (impl_methods
                    ((serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 78)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 78))) value IntegerType))
                              (Value (Integer 16))))))))))))))
                  ((impl_interface -3)
                   (impl_methods
                    ((deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 79))))
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
                               ((Value (Type (StructType 79)))
                                ((slice
                                  (StructField
                                   ((Reference (res (StructType 5))) slice
                                    (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 78)))
                                     ((value
                                       (StructField
                                        ((Reference (res (StructType 5))) value
                                         IntegerType)))))))))))))))))))))))
                  ((impl_interface 10)
                   (impl_methods
                    ((from
                      ((function_signature
                        ((function_params ((i IntegerType)))
                         (function_returns (StructType 78))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 78)))
                              ((value (Reference (i IntegerType))))))))))))))))))
                (struct_id 78) (struct_base_id 9)))))
            (type_counter <opaque>) (memoized_fcalls <opaque>)
            (struct_signs ((current_id 1837) (items ())))
            (union_signs ((current_id 5) (items ()))))) |}]

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
                (Struct ((Value (Type (StructType 79))) ((a (Value (Integer 10))))))))
              (check
               (Value
                (Function
                 ((function_signature
                   ((function_params ((y (StructType 79))))
                    (function_returns (StructType 79))))
                  (function_impl (Fn ((Return (Reference (y (StructType 79)))))))))))
              (Value (Value (Type (StructType 79))))))
            (structs
             ((79
               ((struct_fields ((a ((field_type IntegerType)))))
                (struct_methods
                 ((from
                   ((function_signature
                     ((function_params ((x IntegerType)))
                      (function_returns (StructType 79))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 79)))
                           ((a (Reference (x IntegerType)))))))))))))))
                (struct_impls
                 (((impl_interface 10)
                   (impl_methods
                    ((from
                      ((function_signature
                        ((function_params ((x IntegerType)))
                         (function_returns (StructType 79))))
                       (function_impl
                        (Fn
                         ((Return
                           (Value
                            (Struct
                             ((Value (Type (StructType 79)))
                              ((a (Reference (x IntegerType))))))))))))))))))
                (struct_id 79) (struct_base_id 78)))))
            (type_counter <opaque>) (memoized_fcalls <opaque>)
            (struct_signs ((current_id 1837) (items ())))
            (union_signs ((current_id 5) (items ()))))) |}]

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
            (struct_signs ((current_id 1836) (items ())))
            (union_signs ((current_id 5) (items ()))))) |}]

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
            (struct_signs ((current_id 1836) (items ())))
            (union_signs ((current_id 5) (items ()))))) |}]
(*
   module Config = struct
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

       struct Coins {
         val value: Integer

         fn new(c: Integer) -> Self {
           Self { value: c }
         }

         impl Serialize {
           fn serialize(self: Self, builder: Builder) -> Builder {
             builder.serialize_coins(self.value)
           }
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

       let I9 = Int(9);

       struct AddrExtern {
         val len: Int(9)
         impl Deserialize {
           fn deserialize(s: Slice) -> LoadResult(Self) {
             let res_len = Int(9).deserialize(s);
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
             ((AddrExtern (Value (Type (StructType 14))))
              (I9 (Value (Type (StructType 11))))
              (Int
               (Value
                (Function
                 ((function_signature
                   ((function_params ((bits IntegerType)))
                    (function_returns (StructSig 5))))
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
                                (ExprType (Reference (Self (StructSig 5)))))))
                             (function_impl
                              (Fn
                               ((Return
                                 (Value
                                  (Struct
                                   ((Reference (Self (StructSig 5)))
                                    ((value (Reference (i IntegerType))))))))))))))
                          (serialize
                           (MkFunction
                            ((function_signature
                              ((function_params
                                ((self (ExprType (Reference (Self (StructSig 5)))))
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
                                        (ExprType (Reference (Self (StructSig 5))))))
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
                                     (Type (ExprType (Reference (Self (StructSig 5)))))))))))))
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
                                        ((Reference (Self (StructSig 5))))))
                                      ((slice
                                        (StructField
                                         ((Reference (res (StructType 5))) slice
                                          (StructType 6))))
                                       (value
                                        (Value
                                         (Struct
                                          ((Reference (Self (StructSig 5)))
                                           ((value
                                             (StructField
                                              ((Reference (res (StructType 5))) value
                                               IntegerType)))))))))))))))))))))
                          (from
                           (MkFunction
                            ((function_signature
                              ((function_params ((i IntegerType)))
                               (function_returns
                                (ExprType (Reference (Self (StructSig 5)))))))
                             (function_impl
                              (Fn
                               ((Return
                                 (Value
                                  (Struct
                                   ((Reference (Self (StructSig 5)))
                                    ((value (Reference (i IntegerType))))))))))))))))
                        (mk_impls
                         (((mk_impl_interface (ResolvedReference (Serialize <opaque>)))
                           (mk_impl_methods
                            ((serialize
                              (MkFunction
                               ((function_signature
                                 ((function_params
                                   ((self (ExprType (Reference (Self (StructSig 5)))))
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
                                           (ExprType (Reference (Self (StructSig 5))))))
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
                                         (ExprType (Reference (Self (StructSig 5)))))))))))))
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
                                           ((Reference (Self (StructSig 5))))))
                                         ((slice
                                           (StructField
                                            ((Reference (res (StructType 5))) slice
                                             (StructType 6))))
                                          (value
                                           (Value
                                            (Struct
                                             ((Reference (Self (StructSig 5)))
                                              ((value
                                                (StructField
                                                 ((Reference (res (StructType 5)))
                                                  value IntegerType))))))))))))))))))))))))
                          ((mk_impl_interface (Value (Type (InterfaceType 10))))
                           (mk_impl_methods
                            ((from
                              (MkFunction
                               ((function_signature
                                 ((function_params ((i IntegerType)))
                                  (function_returns
                                   (ExprType (Reference (Self (StructSig 5)))))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Value
                                     (Struct
                                      ((Reference (Self (StructSig 5)))
                                       ((value (Reference (i IntegerType)))))))))))))))))))
                        (mk_struct_id 9) (mk_struct_sig 5)))))))))))
              (Coins (Value (Type (StructType 8))))
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
             ((15
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
               ((struct_fields ((len ((field_type (StructType 11))))))
                (struct_methods
                 ((deserialize
                   ((function_signature
                     ((function_params ((s (StructType 6))))
                      (function_returns (StructType 15))))
                    (function_impl
                     (Fn
                      ((Let
                        ((res_len
                          (FunctionCall
                           ((ResolvedReference (deserialize <opaque>))
                            ((Reference (s (StructType 6))))))))))))))))
                (struct_impls
                 (((impl_interface -3)
                   (impl_methods
                    ((deserialize
                      ((function_signature
                        ((function_params ((s (StructType 6))))
                         (function_returns (StructType 15))))
                       (function_impl
                        (Fn
                         ((Let
                           ((res_len
                             (FunctionCall
                              ((ResolvedReference (deserialize <opaque>))
                               ((Reference (s (StructType 6)))))))))))))))))))
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
                           (Value (Integer 9)))))))))))
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
                              ((Reference (s (StructType 6))) (Value (Integer 9))))))))
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
                              (Value (Integer 9))))))))))))))
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
                                 ((Reference (s (StructType 6))) (Value (Integer 9))))))))
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
                  ((impl_interface 10)
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
                (struct_id 11) (struct_base_id 9)))
              (8
               ((struct_fields ((value ((field_type IntegerType)))))
                (struct_methods
                 ((new
                   ((function_signature
                     ((function_params ((c IntegerType)))
                      (function_returns (StructType 8))))
                    (function_impl
                     (Fn
                      ((Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 8)))
                           ((value (Reference (c IntegerType)))))))))))))
                  (serialize
                   ((function_signature
                     ((function_params
                       ((self (StructType 8)) (builder (StructType 3))))
                      (function_returns (StructType 3))))
                    (function_impl
                     (Fn
                      ((Return
                        (FunctionCall
                         ((ResolvedReference (serialize_coins <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 8))) value IntegerType)))))))))))))
                (struct_impls
                 (((impl_interface -1)
                   (impl_methods
                    ((serialize
                      ((function_signature
                        ((function_params
                          ((self (StructType 8)) (builder (StructType 3))))
                         (function_returns (StructType 3))))
                       (function_impl
                        (Fn
                         ((Return
                           (FunctionCall
                            ((ResolvedReference (serialize_coins <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 8))) value IntegerType))))))))))))))))
                (struct_id 8) (struct_base_id 7)))
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
            (interfaces
             ((10
               ((interface_methods
                 ((from
                   ((function_params ((from IntegerType))) (function_returns SelfType)))))))))
            (type_counter <opaque>) (memoized_fcalls <opaque>)
            (struct_signs
             ((current_id 8)
              (items
               ((7
                 ((st_sig_fields ((len (Value (Type (StructType 11))))))
                  (st_sig_methods
                   ((deserialize
                     ((function_params ((s (StructType 6))))
                      (function_returns
                       (ExprType
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ((T (TypeN 0))))
                               (function_returns (StructSig 0))))
                             (function_impl (BuiltinFn (<fun> <opaque>))))))
                          ((Value (Type (ExprType (Reference (Self (StructSig 7)))))))))))))))
                  (st_sig_base_id 13)))
                (6
                 ((st_sig_fields
                   ((slice (Value (Type (StructType 6))))
                    (value (Value (Type (ExprType (Reference (Self (StructSig 5)))))))))
                  (st_sig_methods
                   ((new
                     ((function_params
                       ((s (StructType 6))
                        (v (ExprType (Reference (Self (StructSig 5)))))))
                      (function_returns (StructSig 6))))))
                  (st_sig_base_id -500)))
                (5
                 ((st_sig_fields ((value (ResolvedReference (Integer <opaque>)))))
                  (st_sig_methods
                   ((new
                     ((function_params ((i IntegerType)))
                      (function_returns
                       (ExprType
                        (Value (Type (ExprType (Reference (Self (StructSig 5))))))))))
                    (serialize
                     ((function_params
                       ((self
                         (ExprType
                          (Value (Type (ExprType (Reference (Self (StructSig 5))))))))
                        (builder (StructType 3))))
                      (function_returns (StructType 3))))
                    (deserialize
                     ((function_params ((s (StructType 6))))
                      (function_returns
                       (ExprType
                        (FunctionCall
                         ((ResolvedReference (LoadResult <opaque>))
                          ((Value (Type (ExprType (Reference (Self (StructSig 5)))))))))))))
                    (from
                     ((function_params ((i IntegerType)))
                      (function_returns
                       (ExprType
                        (Value (Type (ExprType (Reference (Self (StructSig 5))))))))))))
                  (st_sig_base_id 9)))
                (4
                 ((st_sig_fields ((value (Value (Type IntegerType)))))
                  (st_sig_methods
                   ((new
                     ((function_params ((c IntegerType)))
                      (function_returns
                       (ExprType
                        (Value (Type (ExprType (Reference (Self (StructSig 4))))))))))
                    (serialize
                     ((function_params
                       ((self
                         (ExprType
                          (Value (Type (ExprType (Reference (Self (StructSig 4))))))))
                        (builder (StructType 3))))
                      (function_returns (StructType 3))))))
                  (st_sig_base_id 7)))
                (3
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
                  (st_sig_base_id 4)))
                (2
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
                  (st_sig_base_id 2)))
                (1
                 ((st_sig_fields ((c (Value (Type (BuiltinType Cell))))))
                  (st_sig_methods ()) (st_sig_base_id 0)))))))
            (union_signs ((current_id 0) (items ()))))) |}] *)

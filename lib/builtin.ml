open Base
open Lang_types

let builder = Value (Type (BuiltinType "Builder"))

let cell = Value (Type (BuiltinType "Cell"))

let int_type =
  (* memoize struct ids for equality *)
  let struct_ids = Hashtbl.create (module Int)
  (* memoize constructor funs for equality *)
  and int_constructor_funs = Hashtbl.create (module Int)
  (* memoize serializator funs for equality *)
  and int_serializator_funs = Hashtbl.create (module Int) in
  (* int's newtype *)
  let rec int_type_s p bits =
    let struct_id =
      Hashtbl.find_or_add struct_ids bits ~default:(fun () ->
          let c = !struct_counter in
          struct_counter := c + 1 ;
          c )
    in
    let s =
      { struct_fields = [("integer", {field_type = Value (Type IntegerType)})];
        struct_id }
    in
    let methods =
      [ ("new", int_type_s_new s bits);
        ("serialize", int_type_s_serialize s bits) ]
    in
    if Option.is_none @@ List.Assoc.find p.methods ~equal:equal_value (Struct s)
    then p.methods <- (Struct s, methods) :: p.methods
    else () ;
    s
  and int_type_s_new self bits =
    let function_impl =
      Hashtbl.find_or_add int_constructor_funs bits ~default:(fun () ->
          builtin_fun @@ constructor_impl bits )
    in
    { function_signature =
        { function_params = [("integer", Value (Type IntegerType))];
          function_returns = Value (Struct self) };
      function_impl = BuiltinFn function_impl }
  and int_type_s_serialize self bits =
    let function_impl =
      Hashtbl.find_or_add int_serializator_funs bits ~default:(fun () ->
          builtin_fun @@ serialize_impl bits )
    in
    { function_signature =
        { function_params = [("self", Value (Struct self)); ("builder", builder)];
          function_returns = Value (Type VoidType) };
      function_impl = BuiltinFn function_impl }
  and constructor_impl bits p = function
    | [Value (Integer i)] ->
        let numbits = Zint.numbits i in
        let i =
          (* FIXME: or should we raise an error here? *)
          if numbits > bits then
            let extract =
              if Zint.(lt i Zint.zero) then Zint.signed_extract
              else Zint.extract
            in
            extract i 0 (numbits - bits)
          else i
        in
        Value (StructInstance (int_type_s p bits, [("integer", Integer i)]))
    | _ ->
        (* TODO: raise an error instead *)
        constructor_impl bits p [Value (Integer (Zint.of_int 0))]
  and serialize_impl bits _p = function
    | [self; b] ->
        (* TODO: we should extract this to a method in `Builder` but for demonstration's
         * sake we're keeping it here for now
         *)
        Asm ([StructField (self, "integer"); b], [PUSHINT bits; STIX])
    | _ ->
        (* TODO: raise an error instead *)
        Value Void
  and function_impl p = function
    | [Value (Integer bits)] ->
        Value (Struct (int_type_s p @@ Z.to_int bits))
    | _ ->
        (* TODO: raise an error instead *)
        Value Void
  in
  Value
    (Function
       { function_signature =
           { function_params = [("bits", Value (Type IntegerType))];
             function_returns = Value (Type TypeType) };
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let asm =
  let function_impl _p = function
    | [Value (String code)] ->
        let lexbuf = Lexing.from_string code in
        let code = Asm_parser.code Asm_lexer.token lexbuf in
        Asm ([], code)
    | _ ->
        Value Void
  in
  Value
    (Function
       { function_signature =
           { function_params = [("instructions", Value (Type StringType))];
             function_returns = Value (Type VoidType) };
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let serializer =
  let function_signature =
    { function_params = [("t", Value (Type TypeType))];
      function_returns =
        Value
          (Type
             (FunctionType
                { function_params =
                    [("t", Value (Type HoleType)); ("builder", builder)];
                  function_returns = Value (Type VoidType) } ) ) }
  in
  let serializer_f s p = function
    | [struct'; builder] ->
        let calls =
          List.filter_map s.struct_fields ~f:(function
            | name, {field_type = Value f} ->
                let methods =
                  List.Assoc.find_exn p.methods ~equal:equal_value f
                in
                List.Assoc.find methods ~equal:String.equal "serialize"
                |> Option.map ~f:(fun method_ ->
                       Expr
                         (FunctionCall
                            ( Value (Function method_),
                              StructField (struct', name) :: [builder] ) ) )
            | _ ->
                None )
        in
        Block calls
    | _ ->
        (* TODO: error *)
        Value Void
  in
  let function_impl _p = function
    | [Value (Struct s)] ->
        Value
          (Function
             { function_signature;
               function_impl = BuiltinFn (builtin_fun @@ serializer_f s) } )
    | _ ->
        Value
          (Function
             { function_signature;
               function_impl =
                 BuiltinFn (builtin_fun @@ fun _p _args -> Value Void) } )
  in
  Value
    (Function
       { function_signature;
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let asm_function args ret instr =
  Value
    (Function
       { function_signature = {function_params = args; function_returns = ret};
         function_impl = AsmFn instr } )

let asm_functions =
  [ ("new_builder", asm_function [] builder Asm.NEWC);
    ( "store_uint",
      asm_function
        [ ("builder", builder);
          ("value", Value (Type IntegerType));
          ("bits", Value (Type IntegerType)) ]
        builder Asm.NEWC );
    ("end_builder", asm_function [("builder", builder)] cell Asm.ENDC) ]

let default_bindings =
  [ ("asm", asm);
    ("Builder", builder);
    ("Integer", Value (Type IntegerType));
    ("Int", int_type);
    ("Bool", Value (Builtin "Bool"));
    ("Type", Value (Type TypeType));
    ("Void", Value Void);
    (* TODO: re-design the serialization API surface; this is more for demonstration
     * purposes
     *)
    ("serializer", serializer) ]
  @ asm_functions

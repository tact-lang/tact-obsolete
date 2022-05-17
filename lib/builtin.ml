open Base
open Lang_types

let int_type =
  (* memoize constructor funs for equality *)
  let int_constructor_funs = Hashtbl.create (module Int)
  and struct_counter' = !struct_counter in
  struct_counter := struct_counter' + 1 ;
  (* int's newtype *)
  let rec int_type_s bits =
    { struct_fields = [("integer", {field_type = Value (Type IntegerType)})];
      struct_methods = [("new", int_type_s_new bits)];
      struct_id = (bits, struct_counter') }
  and int_type_s_new bits =
    let function_impl =
      Hashtbl.find_or_add int_constructor_funs bits ~default:(fun () ->
          builtin_fun @@ constructor_impl bits )
    in
    { function_params = [("integer", Value (Type IntegerType))];
      (* TODO: figure out how to represent Self *)
      function_returns = Hole;
      function_impl = BuiltinFn function_impl }
  and constructor_impl bits = function
    | [Integer i] ->
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
        Value (StructInstance (int_type_s bits, [("integer", Integer i)]))
    | _ ->
        (* TODO: raise an error instead *)
        constructor_impl bits [Integer (Zint.of_int 0)]
  and function_impl = function
    | [Integer bits] ->
        Value (Struct (int_type_s @@ Z.to_int bits))
    | _ ->
        (* TODO: raise an error instead *)
        Value Void
  in
  Value
    (Function
       { function_params = [("bits", Value (Type IntegerType))];
         function_returns = Value (Struct (int_type_s 257));
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let asm =
  let function_impl = function
    | [String code] ->
        let lexbuf = Lexing.from_string code in
        let code = Asm_parser.code Asm_lexer.token lexbuf in
        Asm code
    | _ ->
        Value Void
  in
  Value
    (Function
       { function_params = [("instructions", Value (Type StringType))];
         function_returns = Value (Type VoidType);
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let default_bindings =
  [ ("asm", asm);
    ("Integer", Value (Type IntegerType));
    ("Int", int_type);
    ("Bool", Value (Builtin "Bool"));
    ("Type", Value (Builtin "Type"));
    ("Void", Value Void) ]

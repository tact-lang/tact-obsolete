open Lang_types

let int_type =
  let struct_counter' = !struct_counter in
  struct_counter := struct_counter' + 1 ;
  (* int's newtype *)
  let int_type_s =
    { struct_fields = [("integer", {field_type = Value (Type IntegerType)})];
      struct_methods = [];
      struct_id = struct_counter' }
  in
  let rec constructor_impl bits p = function
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
        StructInstance (int_type_s, [("integer", Integer i)])
    | _ ->
        (* TODO: raise an error instead *)
        constructor_impl bits p [Integer (Zint.of_int 0)]
  in
  let constructor bits =
    Function
      (BuiltinFn
         { function_params = [("integer", Value (Type IntegerType))];
           function_returns = Value (Struct int_type_s);
           function_impl = constructor_impl bits } )
  in
  let function_impl _p = function
    | [Integer bits] ->
        constructor @@ Zint.to_int bits
    | _ ->
        (* TODO: raise an error instead *)
        Void
  in
  Value
    (Function
       (BuiltinFn
          { function_params = [("bits", Value (Type IntegerType))];
            function_returns = Value (constructor 257);
            function_impl } ) )

let default_bindings =
  [ ("Integer", Value (Type IntegerType));
    ("Int", int_type);
    ("Bool", Value (Builtin "Bool"));
    ("Type", Value (Builtin "Type"));
    ("Void", Value Void) ]

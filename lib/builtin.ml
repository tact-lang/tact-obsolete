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
    BuiltinFn
      { function_params = [("integer", Value (Type IntegerType))];
        (* TODO: figure out how to represent Self *)
        function_returns = Hole;
        function_impl }
  and constructor_impl bits p = function
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
        StructInstance (int_type_s bits, [("integer", Integer i)])
    | _ ->
        (* TODO: raise an error instead *)
        constructor_impl bits p [Integer (Zint.of_int 0)]
  and function_impl _p = function
    | [Integer bits] ->
        Struct (int_type_s @@ Z.to_int bits)
    | _ ->
        (* TODO: raise an error instead *)
        Void
  in
  Value
    (Function
       (BuiltinFn
          { function_params = [("bits", Value (Type IntegerType))];
            function_returns = Value (Struct (int_type_s 257));
            function_impl = builtin_fun function_impl } ) )

let default_bindings =
  [ ("Integer", Value (Type IntegerType));
    ("Int", int_type);
    ("Bool", Value (Builtin "Bool"));
    ("Type", Value (Builtin "Type"));
    ("Void", Value Void) ]

open Base
open Lang_types

let builder = Value (Type (BuiltinType "Builder"))

let builder_methods =
  let new_ =
    { function_signature = {function_params = []; function_returns = builder};
      function_impl = Fn (Some (Return (Primitive EmptyBuilder))) }
  in
  [("new", new_)]

let cell = Value (Type (BuiltinType "Cell"))

let int_type =
  (* memoize struct ids for equality *)
  let struct_ids = Hashtbl.create (module Int)
  (* memoize constructor funs for equality *)
  and int_constructor_funs = Hashtbl.create (module Int) in
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
        ("serialize", int_type_s_serialize bits s) ]
    in
    if
      Option.is_none
      @@ List.Assoc.find p.methods ~equal:equal_value (Type (StructType s))
    then p.methods <- (Type (StructType s), methods) :: p.methods
    else () ;
    s
  and int_type_s_new self bits =
    let function_impl =
      Hashtbl.find_or_add int_constructor_funs bits ~default:(fun () ->
          builtin_fun @@ constructor_impl bits )
    in
    { function_signature =
        { function_params = [("integer", Value (Type IntegerType))];
          function_returns = Value (Type (StructType self)) };
      function_impl = BuiltinFn function_impl }
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
        Struct (int_type_s p bits, [("integer", Value (Integer i))])
    | _ ->
        (* TODO: raise an error instead *)
        constructor_impl bits p [Integer (Zint.of_int 0)]
  and int_type_s_serialize bits s =
    let self = Value (Type (StructType s)) in
    { function_signature =
        { function_params = [("self", self); ("b", builder)];
          function_returns = builder };
      function_impl =
        Fn
          (Some
             (Return
                (Primitive
                   (StoreInt
                      { builder = Reference ("b", builder);
                        length = Value (Integer (Z.of_int bits));
                        integer =
                          StructField
                            ( Reference ("self", Value (Type (StructType s))),
                              "integer" );
                        signed = true } ) ) ) ) }
  and function_impl p = function
    | [Integer bits] ->
        Type (StructType (int_type_s p @@ Z.to_int bits))
    | _ ->
        (* TODO: raise an error instead *)
        Void
  in
  Value
    (Function
       { function_signature =
           { function_params = [("bits", Value (Type IntegerType))];
             function_returns = Value (Type TypeType) };
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let serializer =
  let function_signature =
    { function_params = [("t", Value (Type TypeType))];
      function_returns =
        Value
          (Type
             (FunctionType
                { function_params =
                    [("t", Value (Type HoleType)); ("b", builder)];
                  function_returns = builder } ) ) }
  in
  let rec serializer_f s p =
    let calls =
      List.filter_map s.struct_fields ~f:(function
        | name, {field_type = Value f} ->
            let methods = List.Assoc.find_exn p.methods ~equal:equal_value f in
            let serialize_field =
              match List.Assoc.find methods ~equal:String.equal "serialize" with
              | Some m ->
                  Some m
              | None -> (
                match f with
                | Type (StructType t) ->
                    let m = serializer_f t p in
                    p.methods <-
                      List.map p.methods ~f:(fun (s, methods) ->
                          match equal_value s (Type (StructType t)) with
                          | true ->
                              (s, ("serializer", m) :: methods)
                          | false ->
                              (s, methods) ) ;
                    Some m
                | _ ->
                    None )
            in
            serialize_field
            |> Option.map ~f:(fun method_ ->
                   Let
                     [ ( "b",
                         FunctionCall
                           ( Value (Function method_),
                             StructField
                               ( Reference ("self", Value (Type (StructType s))),
                                 name )
                             :: [Reference ("b", builder)] ) ) ] )
        | _ ->
            None )
    in
    let body = Block (calls @ [Return (Reference ("b", builder))]) in
    { function_signature =
        { function_params =
            [("self", Value (Type (StructType s))); ("b", builder)];
          function_returns = builder };
      function_impl = Fn (Some body) }
  in
  let function_impl p = function
    | [Type (StructType s)] ->
        Function (serializer_f s p)
    | _ ->
        Void
  in
  Value
    (Function
       { function_signature;
         function_impl = BuiltinFn (builtin_fun function_impl) } )

(* Only for debug purposes *)
let bin_op_intf =
  Value
    (Type
       (InterfaceType
          { interface_methods =
              [ ( "op",
                  { function_params =
                      [ ("left", Value (Type IntegerType));
                        ("right", Value (Type IntegerType)) ];
                    function_returns = Value (Type IntegerType) } ) ] } ) )

let default_bindings =
  [ ("Builder", builder);
    ("Integer", Value (Type IntegerType));
    ("Int", int_type);
    ("Bool", Value (Type BoolType));
    ("Type", Value (Type TypeType));
    ("Void", Value Void);
    (* TODO: re-design the serialization API surface; this is more for demonstration
     * purposes
     *)
    ("serializer", serializer);
    ("BinOp", bin_op_intf) ]

let default_methods = [(Type (BuiltinType "Builder"), builder_methods)]

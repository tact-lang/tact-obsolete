open Base
open Lang_types

let builder = BuiltinType "Builder"

(* Builder is second struct in the std, so its ID will be 3 *)
let builder_struct = StructType 3

let cell = Value (Type (BuiltinType "Cell"))

let serializer =
  let function_signature =
    { function_params = [("t", type0)];
      function_returns =
        FunctionType
          { function_params = [("t", HoleType); ("b", builder_struct)];
            function_returns = builder_struct } }
  in
  let serializer_f s p =
    let s = List.Assoc.find_exn p.structs s ~equal:equal_int in
    let calls =
      List.filter_map s.struct_fields ~f:(function name, {field_type = f} ->
          let serialize_field =
            match
              List.Assoc.find (Program.methods_of p f) ~equal:String.equal
                "serialize"
            with
            | Some m ->
                Some m
            | None ->
                None
          in
          serialize_field
          |> Option.map ~f:(fun method_ ->
                 Let
                   [ ( "b",
                       FunctionCall
                         ( Value (Function method_),
                           StructField
                             ( Reference ("self", StructType s.struct_id),
                               name,
                               f )
                           :: [Reference ("b", builder_struct)] ) ) ] ) )
    in
    let body = Block (calls @ [Return (Reference ("b", builder_struct))]) in
    { function_signature =
        { function_params =
            [("self", StructType s.struct_id); ("b", builder_struct)];
          function_returns = builder_struct };
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

let from_intf =
  let function_signature =
    {function_params = [("T", type0)]; function_returns = HoleType}
  in
  let make_from t =
    Type
      (InterfaceType
         { interface_methods =
             [ ( "from",
                 {function_params = [("from", t)]; function_returns = SelfType}
               ) ] } )
  in
  let function_impl _p = function [Type t] -> make_from t | _ -> Void in
  Value
    (Function
       { function_signature;
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let make_builtin_fn params ret_ty primitive =
  Value
    (Function
       { function_signature =
           {function_params = params; function_returns = ret_ty};
         function_impl = Fn (Some (Return (Primitive primitive))) } )

let builtin_bindings =
  [ ("builtin_Builder", Value (Type (BuiltinType "Builder")));
    ("builtin_Cell", Value (Type (BuiltinType "Cell")));
    ( "builtin_builder_new",
      Value
        (Function
           { function_signature =
               {function_params = []; function_returns = BuiltinType "Builder"};
             function_impl = Fn (Some (Return (Primitive EmptyBuilder))) } ) );
    ( "builtin_builder_build",
      make_builtin_fn
        [("b", BuiltinType "Builder")]
        (BuiltinType "Cell")
        (BuildCell {builder = Reference ("b", BuiltinType "Builder")}) );
    ( "builtin_builder_store_int",
      make_builtin_fn
        [ ("b", BuiltinType "Builder");
          ("int", IntegerType);
          ("bits", IntegerType) ]
        (BuiltinType "Builder")
        (StoreInt
           { builder = Reference ("b", BuiltinType "Builder");
             length = Reference ("bits", IntegerType);
             integer = Reference ("int", IntegerType);
             signed = true } ) );
    ( "builtin_send_raw_msg",
      make_builtin_fn
        [("msg", BuiltinType "Cell"); ("flags", IntegerType)]
        VoidType
        (SendRawMsg
           { msg = Reference ("msg", BuiltinType "Cell");
             flags = Reference ("flags", IntegerType) } ) ) ]

let default_bindings () =
  [ ("Integer", Value (Type IntegerType));
    ("Bool", Value (Type BoolType));
    ("Type", Value (Type type0));
    ("Void", Value Void);
    ("VoidType", Value (Type VoidType));
    (* TODO: re-design the serialization API surface; this is more for demonstration
     * purposes
     *)
    ("serializer", serializer);
    ("From", from_intf) ]
  @ builtin_bindings

let default_structs = []

let std = [%blob "std/std.tact"]

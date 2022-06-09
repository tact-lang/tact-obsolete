open Base
open Lang_types

let builder = BuiltinType "Builder"

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
  let rec int_type_s bits =
    let struct_id =
      Hashtbl.find_or_add struct_ids bits ~default:(fun () ->
          let c = !struct_counter in
          struct_counter := c + 1 ;
          c )
    in
    let struct_methods =
      [("new", int_type_s_new bits); ("serialize", int_type_s_serialize bits)]
    and struct_impls = [] in
    { struct_fields = [("integer", {field_type = IntegerType})];
      struct_methods;
      struct_impls;
      struct_id }
  and int_type_s_new bits =
    let function_impl =
      Hashtbl.find_or_add int_constructor_funs bits ~default:(fun () ->
          builtin_fun @@ constructor_impl bits )
    in
    { function_signature =
        { function_params = [("integer", IntegerType)];
          function_returns = SelfType };
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
        Struct (int_type_s bits, [("integer", Value (Integer i))])
    | _ ->
        (* TODO: raise an error instead *)
        constructor_impl bits p [Integer (Zint.of_int 0)]
  and int_type_s_serialize bits =
    let self = SelfType in
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
                          StructField (Reference ("self", self), "integer");
                        signed = true } ) ) ) ) }
  and function_impl _p = function
    | [Integer bits] ->
        Type (StructType (int_type_s @@ Z.to_int bits))
    | _ ->
        (* TODO: raise an error instead *)
        Void
  in
  Value
    (Function
       { function_signature =
           {function_params = [("bits", IntegerType)]; function_returns = type0};
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let serializer =
  let function_signature =
    { function_params = [("t", type0)];
      function_returns =
        FunctionType
          { function_params = [("t", HoleType); ("b", builder)];
            function_returns = builder } }
  in
  let rec serializer_f s p =
    let calls =
      List.filter_map s.struct_fields ~f:(function name, {field_type = f} ->
          let methods = methods_of f in
          let serialize_field =
            match List.Assoc.find methods ~equal:String.equal "serialize" with
            | Some m ->
                Some m
            | None -> (
              match f with StructType t -> Some (serializer_f t p) | _ -> None )
          in
          serialize_field
          |> Option.map ~f:(fun method_ ->
                 Let
                   [ ( "b",
                       FunctionCall
                         ( Value (Function method_),
                           StructField (Reference ("self", StructType s), name)
                           :: [Reference ("b", builder)] ) ) ] ) )
    in
    let body = Block (calls @ [Return (Reference ("b", builder))]) in
    { function_signature =
        { function_params = [("self", StructType s); ("b", builder)];
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
                      [("left", IntegerType); ("right", IntegerType)];
                    function_returns = IntegerType } ) ] } ) )

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

let default_bindings =
  [ ("Builder", Value (Type builder));
    ("Integer", Value (Type IntegerType));
    ("Int", int_type);
    ("Bool", Value (Type BoolType));
    ("Type", Value (Type type0));
    ("Void", Value Void);
    (* TODO: re-design the serialization API surface; this is more for demonstration
     * purposes
     *)
    ("serializer", serializer);
    ("BinOp", bin_op_intf);
    ("From", from_intf) ]

let default_methods = [(Type (BuiltinType "Builder"), builder_methods)]

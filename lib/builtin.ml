open Base
open Lang_types

(* If you add new built-in struct, increase previous struct id and add it below, from 0 up to 99. *)
let builder_id = 0

let builder = BuiltinType "Builder"

let builder_struct_info =
  let id = builder_id in
  let builder_methods =
    let new_ =
      { function_signature =
          {function_params = []; function_returns = StructType id};
        function_impl =
          Fn
            (Some
               (Return
                  (Value (Struct (id, [("builder", Primitive EmptyBuilder)])))
               ) ) }
    in
    [("new", new_)]
  in
  let builder_struct =
    { struct_fields = [("builder", {field_type = builder})];
      struct_methods = builder_methods;
      struct_impls = [];
      struct_id = id }
  in
  builder_struct

let builder_struct = StructType builder_id

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
          let c = p.struct_counter in
          p.struct_counter <- c + 1 ;
          c )
    in
    let s_ty = struct_id in
    let methods =
      [ ("new", int_type_s_new s_ty bits);
        ("serialize", int_type_s_serialize bits s_ty) ]
    in
    let s =
      { struct_fields = [("integer", {field_type = IntegerType})];
        struct_methods = methods;
        struct_impls = [];
        struct_id }
    in
    if Option.is_none @@ List.Assoc.find p.structs ~equal:equal_int s_ty then
      p.structs <- (s_ty, s) :: p.structs
    else () ;
    s_ty
  and int_type_s_new self bits =
    let function_impl =
      Hashtbl.find_or_add int_constructor_funs bits ~default:(fun () ->
          builtin_fun @@ constructor_impl bits )
    in
    { function_signature =
        { function_params = [("integer", IntegerType)];
          function_returns = StructType self };
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
    let self = StructType s in
    { function_signature =
        { function_params = [("self", self); ("b", builder_struct)];
          function_returns = builder_struct };
      function_impl =
        Fn
          (Some
             (Return
                (Primitive
                   (StoreInt
                      { builder =
                          StructField
                            (Reference ("b", builder_struct), "builder");
                        length = Value (Integer (Z.of_int bits));
                        integer =
                          StructField
                            (Reference ("self", StructType s), "integer");
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
           {function_params = [("bits", IntegerType)]; function_returns = type0};
         function_impl = BuiltinFn (builtin_fun function_impl) } )

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
                             (Reference ("self", StructType s.struct_id), name)
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
  [ ("Builder", Value (Type builder_struct));
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

let default_structs = [(builder_id, builder_struct_info)]

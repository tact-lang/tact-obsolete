open Base
open Lang_types

let builder = BuiltinType "Builder"

(* Builder is second struct in the std, so its ID will be 3 *)
let builder_struct = StructType 3

let slice_struct = StructType 6

let cell = Value (Type (BuiltinType "Cell"))

let builtin_struct_next_id = ref 0

let next_builtin_struct_id () =
  let id = !builtin_struct_next_id - 1 in
  builtin_struct_next_id := id ;
  id

let make_load_result_with_id id t =
  { struct_fields =
      [("slice", {field_type = slice_struct}); ("value", {field_type = t})];
    struct_methods =
      [ ( "new",
          { function_signature =
              { function_params = [("s", slice_struct); ("v", t)];
                function_returns = StructType id };
            function_impl =
              Fn
                (Some
                   (Return
                      (Value
                         (Struct
                            ( Value (Type (StructType id)),
                              [ ("slice", Reference ("s", slice_struct));
                                ("value", Reference ("v", t)) ] ) ) ) ) ) } ) ];
    struct_impls = [];
    struct_id = id;
    tensor = false }

let load_result_func a =
  let function_signature =
    { function_params = [("T", type0)];
      function_returns =
        (let id, _ =
           Arena.with_id a ~f:(fun _ ->
               { st_sig_fields =
                   [ ("slice", Value (Type slice_struct));
                     ("value", Value (Type (Dependent ("T", type0)))) ] } )
         in
         StructSig id ) }
  in
  let make_load_result p t =
    let id = p.type_counter in
    p.type_counter <- p.type_counter + 1 ;
    let struct_ = make_load_result_with_id id t in
    let struct_ =
      Result.ok_exn @@ Program.with_struct p struct_ (fun _ -> Ok struct_)
    in
    Type (StructType struct_.struct_id)
  in
  let function_impl p = function
    | [Type t] ->
        make_load_result p t
    | _ ->
        Void
  in
  Value
    (Function
       { function_signature;
         function_impl = BuiltinFn (builtin_fun function_impl) } )

let serialize_intf =
  let intf =
    { interface_methods =
        [ ( "serialize",
            { function_params = [("self", SelfType); ("b", builder_struct)];
              function_returns = builder_struct } ) ] }
  in
  intf

let serialize_intf_id = next_builtin_struct_id ()

let deserialize_intf =
  let id = next_builtin_struct_id () in
  let intf =
    { interface_methods =
        [ ( "deserialize",
            { function_params = [("b", builder_struct)];
              function_returns = StructType id } ) ] }
  in
  (intf, id)

let deserialize_intf_id = next_builtin_struct_id ()

let serializer =
  let function_signature =
    { function_params = [("t", type0)];
      function_returns =
        FunctionType
          { function_params = [("t", HoleType); ("b", builder_struct)];
            function_returns = builder_struct } }
  in
  let rec int_required_bits = function
    | 0 ->
        0
    | x ->
        1 + (int_required_bits @@ Int.shift_right x 1)
  in
  let serialize_union_ty u p =
    let union = List.Assoc.find_exn p.unions u ~equal:equal_int in
    let discriminator_len =
      Value
        (Integer (Z.of_int (int_required_bits (List.length union.cases - 1))))
    in
    let branches =
      List.filter_map union.cases ~f:(fun (ty, Discriminator discr) ->
          let serialize_ty =
            match
              List.Assoc.find (Program.methods_of p ty) ~equal:String.equal
                "serialize"
            with
            | Some m ->
                Some m
            | None ->
                None
          in
          serialize_ty
          |> Option.map ~f:(fun method_ ->
                 { branch_ty = ty;
                   branch_var = "var";
                   branch_stmt =
                     Block
                       [ Let
                           [ ( "b",
                               Primitive
                                 (StoreInt
                                    { builder =
                                        StructField
                                          ( Reference ("b", builder_struct),
                                            "b",
                                            BuiltinType "Builder" );
                                      length = discriminator_len;
                                      integer = Value (Integer (Z.of_int discr));
                                      signed = false } ) ) ];
                         Let
                           [ ( "b",
                               FunctionCall
                                 ( Value (Function method_),
                                   [ Reference ("var", ty);
                                     Reference ("b", builder_struct) ] ) ) ];
                         Return (Reference ("b", builder_struct)) ] } ) )
    in
    let switch =
      {switch_condition = Reference ("self", UnionType union.union_id); branches}
    in
    let body = Switch switch in
    { function_signature =
        { function_params =
            [("self", UnionType union.union_id); ("b", builder_struct)];
          function_returns = builder_struct };
      function_impl = Fn (Some body) }
  in
  let serializer_struct_ty s p =
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
        Function (serializer_struct_ty s p)
    | [Type (UnionType u)] ->
        Function (serialize_union_ty u p)
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
  let make_from p t =
    let intf =
      { interface_methods =
          [ ( "from",
              {function_params = [("from", t)]; function_returns = SelfType} )
          ] }
    in
    let intf_ty = Program.insert_interface p intf in
    Type intf_ty
  in
  let function_impl p = function [Type t] -> make_from p t | _ -> Void in
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

let tensor2_hashtbl =
  Hashtbl.create
    ( module struct
      type t = Lang_types.type_ * Lang_types.type_
      [@@deriving hash, sexp_of, compare]
    end )

let tensor2 t1 t2 =
  Hashtbl.find_or_add tensor2_hashtbl (t1, t2) ~default:(fun () ->
      { struct_id = next_builtin_struct_id ();
        struct_fields =
          [("value1", {field_type = t1}); ("value2", {field_type = t2})];
        struct_methods = [];
        struct_impls = [];
        tensor = true } )

let builtin_bindings =
  [ ("builtin_Builder", Value (Type (BuiltinType "Builder")));
    ("builtin_Cell", Value (Type (BuiltinType "Cell")));
    ("builtin_Slice", Value (Type (BuiltinType "Slice")));
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
    ( "builtin_builder_store_coins",
      make_builtin_fn
        [("b", BuiltinType "Builder"); ("c", IntegerType)]
        BoolType
        (StoreCoins
           { builder = Reference ("b", BuiltinType "Builder");
             coins = Reference ("c", IntegerType) } ) );
    ( "builtin_slice_begin_parse",
      make_builtin_fn
        [("c", BuiltinType "Cell")]
        (BuiltinType "Slice")
        (ParseCell {cell = Reference ("c", BuiltinType "Cell")}) );
    ( "builtin_slice_end_parse",
      make_builtin_fn
        [("s", BuiltinType "Slice")]
        VoidType
        (SliceEndParse {slice = Reference ("s", BuiltinType "Slice")}) );
    ( "builtin_slice_load_int",
      make_builtin_fn
        [("s", BuiltinType "Slice"); ("bits", IntegerType)]
        (StructType (tensor2 (BuiltinType "Slice") IntegerType).struct_id)
        (SliceLoadInt
           { slice = Reference ("s", BuiltinType "Slice");
             bits = Reference ("bits", IntegerType) } ) );
    ( "builtin_divmod",
      make_builtin_fn
        [("x", IntegerType); ("y", IntegerType)]
        (StructType (tensor2 IntegerType IntegerType).struct_id)
        (Divmod
           {x = Reference ("x", IntegerType); y = Reference ("y", IntegerType)}
        ) );
    ( "builtin_send_raw_msg",
      make_builtin_fn
        [("msg", BuiltinType "Cell"); ("flags", IntegerType)]
        VoidType
        (SendRawMsg
           { msg = Reference ("msg", BuiltinType "Cell");
             flags = Reference ("flags", IntegerType) } ) );
    ( "builtin_equal",
      make_builtin_fn
        [("x", IntegerType); ("y", IntegerType)]
        BoolType
        (Equality
           {x = Reference ("x", IntegerType); y = Reference ("y", IntegerType)}
        ) ) ]

let default_bindings signs =
  [ ("Integer", Value (Type IntegerType));
    ("Bool", Value (Type BoolType));
    ("Type", Value (Type type0));
    ("Void", Value Void);
    ("VoidType", Value (Type VoidType));
    (* TODO: re-design the serialization API surface; this is more for demonstration
     * purposes
     *)
    ("serializer", serializer);
    ("Serialize", Value (Type (InterfaceType serialize_intf_id)));
    ("Deserialize", Value (Type (InterfaceType deserialize_intf_id)));
    ("LoadResult", load_result_func signs);
    ("From", from_intf) ]
  @ builtin_bindings

let default_structs =
  ( Hashtbl.map tensor2_hashtbl ~f:(fun struct_ -> (struct_.struct_id, struct_))
  |> Hashtbl.data )
  @ [ (let id = snd deserialize_intf in
       (id, make_load_result_with_id id SelfType) ) ]

let default_intfs =
  [ (serialize_intf_id, serialize_intf);
    (deserialize_intf_id, fst deserialize_intf) ]

let std = [%blob "std/std.tact"]

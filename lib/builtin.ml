open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    open Config

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

    let bl = builtin_located

    let make_load_result_with_id base_id id t =
      { struct_fields =
          [ (bl "slice", {field_type = slice_struct});
            (bl "value", {field_type = t}) ];
        struct_details =
          { uty_methods =
              [ ( bl "new",
                  bl
                    { function_signature =
                        bl
                          { function_params =
                              [(bl "s", slice_struct); (bl "v", t)];
                            function_returns = StructType id };
                      function_impl =
                        Fn
                          (bl
                             (Return
                                (bl
                                   (Value
                                      (Struct
                                         ( bl @@ Value (Type (StructType id)),
                                           [ ( "slice",
                                               bl
                                               @@ Reference
                                                    (bl "s", slice_struct) );
                                             ( "value",
                                               bl @@ Reference (bl "v", t) ) ]
                                         ) ) ) ) ) ) } ) ];
            uty_impls = [];
            uty_id = id;
            uty_base_id = base_id };
        tensor = false }

    let load_result_func a =
      let base_id = -500 in
      let function_signature =
        bl
          { function_params = [(bl "T", type0)];
            function_returns =
              (let id, _ =
                 Arena.with_id a ~f:(fun id ->
                     { st_sig_fields =
                         [ (bl "slice", bl @@ Value (Type slice_struct));
                           ( bl "value",
                             bl @@ Value (Type (Dependent (bl "T", type0))) ) ];
                       st_sig_methods =
                         [ ( bl "new",
                             bl
                               { function_params =
                                   [ (bl "s", slice_struct);
                                     (bl "v", Dependent (bl "T", type0)) ];
                                 function_returns = StructSig id } ) ];
                       st_sig_base_id = base_id;
                       st_sig_id = id } )
               in
               StructSig id ) }
      in
      let make_load_result p t =
        let id = p.type_counter in
        p.type_counter <- p.type_counter + 1 ;
        let struct_ = make_load_result_with_id base_id id t in
        let struct_ =
          Result.ok_exn @@ Program.with_struct p struct_ (fun _ -> Ok struct_)
        in
        Type (StructType struct_.struct_details.uty_id)
      in
      let function_impl p = function
        | [Type t] ->
            make_load_result p t
        | _ ->
            Void
      in
      bl
      @@ Value
           (Function
              (bl
                 { function_signature;
                   function_impl = BuiltinFn (builtin_fun function_impl) } ) )

    let serialize_intf =
      let intf =
        { interface_methods =
            [ ( "serialize",
                bl
                  { function_params =
                      [(bl "self", SelfType); (bl "b", builder_struct)];
                    function_returns = builder_struct } ) ] }
      in
      intf

    let serialize_intf_id = next_builtin_struct_id ()

    let deserialize_intf =
      let id = next_builtin_struct_id () in
      let intf =
        { interface_methods =
            [ ( "deserialize",
                bl
                  { function_params = [(bl "b", builder_struct)];
                    function_returns = StructType id } ) ] }
      in
      (intf, id)

    let deserialize_intf_id = next_builtin_struct_id ()

    let serializer =
      let function_signature =
        bl
          { function_params = [(bl "t", type0)];
            function_returns =
              FunctionType
                (bl
                   { function_params =
                       [(bl "t", HoleType); (bl "b", builder_struct)];
                     function_returns = builder_struct } ) }
      in
      let rec int_required_bits = function
        | 0 ->
            0
        | x ->
            1 + (int_required_bits @@ Int.shift_right x 1)
      in
      let serialize_union_ty u p =
        (* print_sexp (sexp_of_int u) ;
           print_sexp (sexp_of_program p) ; *)
        let union = List.Assoc.find_exn p.unions u ~equal:equal_int in
        let discriminator_len =
          Value
            (Integer
               (Z.of_int (int_required_bits (List.length union.cases - 1))) )
        in
        let branches =
          List.filter_map union.cases ~f:(fun (ty, Discriminator discr) ->
              let serialize_ty =
                match
                  List.Assoc.find (Program.methods_of p ty)
                    ~equal:(fun v1 v2 -> equal_string v1.value v2.value)
                    (bl "serialize")
                with
                | Some m ->
                    Some m
                | None ->
                    None
              in
              serialize_ty
              |> Option.map ~f:(fun method_ ->
                     { branch_ty = ty;
                       branch_var = bl "var";
                       branch_stmt =
                         bl
                         @@ Block
                              [ bl
                                @@ Let
                                     [ ( bl "b",
                                         bl
                                         @@ Primitive
                                              (StoreInt
                                                 { builder =
                                                     bl
                                                     @@ StructField
                                                          ( bl
                                                            @@ Reference
                                                                 ( bl "b",
                                                                   builder_struct
                                                                 ),
                                                            bl "b",
                                                            BuiltinType
                                                              "Builder" );
                                                   length = bl discriminator_len;
                                                   integer =
                                                     bl
                                                     @@ Value
                                                          (Integer
                                                             (Z.of_int discr) );
                                                   signed = false } ) ) ];
                                bl
                                @@ Let
                                     [ ( bl "b",
                                         bl
                                         @@ FunctionCall
                                              ( bl @@ Value (Function method_),
                                                [ bl @@ Reference (bl "var", ty);
                                                  bl
                                                  @@ Reference
                                                       (bl "b", builder_struct)
                                                ] ) ) ];
                                bl
                                @@ Return
                                     (bl @@ Reference (bl "b", builder_struct))
                              ] }
                     |> bl ) )
        in
        let switch =
          { switch_condition =
              bl @@ Reference (bl "self", UnionType union.union_details.uty_id);
            branches }
        in
        let body = Switch switch in
        { function_signature =
            bl
              { function_params =
                  [ (bl "self", UnionType union.union_details.uty_id);
                    (bl "b", builder_struct) ];
                function_returns = builder_struct };
          function_impl = Fn (bl body) }
      in
      let serializer_struct_ty s p =
        let s = List.Assoc.find_exn p.structs s ~equal:equal_int in
        let calls =
          List.filter_map s.struct_fields ~f:(function
              | name, {field_type = f} ->
              let serialize_field =
                match
                  List.Assoc.find (Program.methods_of p f)
                    ~equal:(fun v1 v2 -> equal_string v1.value v2.value)
                    (bl "serialize")
                with
                | Some m ->
                    Some m
                | None ->
                    None
              in
              serialize_field
              |> Option.map ~f:(fun method_ ->
                     Let
                       [ ( bl "b",
                           bl
                           @@ FunctionCall
                                ( bl @@ Value (Function method_),
                                  ( bl
                                  @@ StructField
                                       ( bl
                                         @@ Reference
                                              ( bl "self",
                                                StructType
                                                  s.struct_details.uty_id ),
                                         name,
                                         f ) )
                                  :: [bl @@ Reference (bl "b", builder_struct)]
                                ) ) ]
                     |> bl ) )
        in
        let body =
          Block
            (calls @ [bl @@ Return (bl @@ Reference (bl "b", builder_struct))])
        in
        { function_signature =
            bl
              { function_params =
                  [ (bl "self", StructType s.struct_details.uty_id);
                    (bl "b", builder_struct) ];
                function_returns = builder_struct };
          function_impl = Fn (bl body) }
      in
      let function_impl p = function
        | [Type (StructType s)] ->
            Function (bl @@ serializer_struct_ty s p)
        | [Type (UnionType u)] ->
            Function (bl @@ serialize_union_ty u p)
        | _ ->
            Void
      in
      bl
      @@ Value
           (Function
              (bl
                 { function_signature;
                   function_impl = BuiltinFn (builtin_fun function_impl) } ) )

    let from_intf =
      let function_signature =
        bl {function_params = [(bl "T", type0)]; function_returns = HoleType}
      in
      let make_from p t =
        let intf =
          { interface_methods =
              [ ( "from",
                  bl
                    { function_params = [(bl "from", t)];
                      function_returns = SelfType } ) ] }
        in
        let intf_ty = Program.insert_interface p intf in
        Type intf_ty
      in
      let function_impl p = function [Type t] -> make_from p t | _ -> Void in
      bl
      @@ Value
           (Function
              (bl
                 { function_signature;
                   function_impl = BuiltinFn (builtin_fun function_impl) } ) )

    let make_builtin_fn params ret_ty primitive =
      bl
      @@ Value
           (Function
              (bl
                 { function_signature =
                     bl {function_params = params; function_returns = ret_ty};
                   function_impl = Fn (bl (Return (bl @@ Primitive primitive)))
                 } ) )

    let tensor2_hashtbl =
      Hashtbl.create
        ( module struct
          type t = type_ * type_ [@@deriving hash, sexp_of, compare]
        end )

    let tensor2 t1 t2 =
      Hashtbl.find_or_add tensor2_hashtbl (t1, t2) ~default:(fun () ->
          { struct_fields =
              [ (bl "value1", {field_type = t1});
                (bl "value2", {field_type = t2}) ];
            struct_details =
              { uty_id = next_builtin_struct_id ();
                uty_methods = [];
                uty_impls = [];
                uty_base_id = -501 };
            tensor = true } )

    let builtin_bindings =
      [ (bl "builtin_Builder", bl @@ Value (Type (BuiltinType "Builder")));
        (bl "builtin_Cell", bl @@ Value (Type (BuiltinType "Cell")));
        (bl "builtin_Slice", bl @@ Value (Type (BuiltinType "Slice")));
        ( bl "builtin_builder_new",
          bl
          @@ Value
               (Function
                  (bl
                     { function_signature =
                         bl
                           { function_params = [];
                             function_returns = BuiltinType "Builder" };
                       function_impl =
                         Fn (bl (Return (bl @@ Primitive EmptyBuilder))) } ) )
        );
        ( bl "builtin_builder_build",
          make_builtin_fn
            [(bl "b", BuiltinType "Builder")]
            (BuiltinType "Cell")
            (BuildCell
               {builder = bl @@ Reference (bl "b", BuiltinType "Builder")} ) );
        ( bl "builtin_builder_store_int",
          make_builtin_fn
            [ (bl "b", BuiltinType "Builder");
              (bl "int", IntegerType);
              (bl "bits", IntegerType) ]
            (BuiltinType "Builder")
            (StoreInt
               { builder = bl @@ Reference (bl "b", BuiltinType "Builder");
                 length = bl @@ Reference (bl "bits", IntegerType);
                 integer = bl @@ Reference (bl "int", IntegerType);
                 signed = true } ) );
        ( bl "builtin_builder_store_coins",
          make_builtin_fn
            [(bl "b", BuiltinType "Builder"); (bl "c", IntegerType)]
            BoolType
            (StoreCoins
               { builder = bl @@ Reference (bl "b", BuiltinType "Builder");
                 coins = bl @@ Reference (bl "c", IntegerType) } ) );
        ( bl "builtin_slice_begin_parse",
          make_builtin_fn
            [(bl "c", BuiltinType "Cell")]
            (BuiltinType "Slice")
            (ParseCell {cell = bl @@ Reference (bl "c", BuiltinType "Cell")}) );
        ( bl "builtin_slice_end_parse",
          make_builtin_fn
            [(bl "s", BuiltinType "Slice")]
            VoidType
            (SliceEndParse
               {slice = bl @@ Reference (bl "s", BuiltinType "Slice")} ) );
        ( bl "builtin_slice_load_int",
          make_builtin_fn
            [(bl "s", BuiltinType "Slice"); (bl "bits", IntegerType)]
            (StructType
               (tensor2 (BuiltinType "Slice") IntegerType).struct_details.uty_id
            )
            (SliceLoadInt
               { slice = bl @@ Reference (bl "s", BuiltinType "Slice");
                 bits = bl @@ Reference (bl "bits", IntegerType) } ) );
        ( bl "builtin_divmod",
          make_builtin_fn
            [(bl "x", IntegerType); (bl "y", IntegerType)]
            (StructType (tensor2 IntegerType IntegerType).struct_details.uty_id)
            (Divmod
               { x = bl @@ Reference (bl "x", IntegerType);
                 y = bl @@ Reference (bl "y", IntegerType) } ) );
        ( bl "builtin_send_raw_msg",
          make_builtin_fn
            [(bl "msg", BuiltinType "Cell"); (bl "flags", IntegerType)]
            VoidType
            (SendRawMsg
               { msg = bl @@ Reference (bl "msg", BuiltinType "Cell");
                 flags = bl @@ Reference (bl "flags", IntegerType) } ) );
        ( bl "builtin_equal",
          make_builtin_fn
            [(bl "x", IntegerType); (bl "y", IntegerType)]
            BoolType
            (Equality
               { x = bl @@ Reference (bl "x", IntegerType);
                 y = bl @@ Reference (bl "y", IntegerType) } ) ) ]

    let default_bindings signs =
      [ (bl "Integer", bl @@ Value (Type IntegerType));
        (bl "Bool", bl @@ Value (Type BoolType));
        (bl "Type", bl @@ Value (Type type0));
        (bl "Void", bl @@ Value Void);
        (bl "VoidType", bl @@ Value (Type VoidType));
        (* TODO: re-design the serialization API surface; this is more for demonstration
         * purposes
         *)
        (bl "serializer", serializer);
        (bl "Serialize", bl @@ Value (Type (InterfaceType serialize_intf_id)));
        ( bl "Deserialize",
          bl @@ Value (Type (InterfaceType deserialize_intf_id)) );
        (bl "LoadResult", load_result_func signs);
        (bl "From", from_intf) ]
      @ builtin_bindings

    let default_structs =
      ( Hashtbl.map tensor2_hashtbl ~f:(fun struct_ ->
            (struct_.struct_details.uty_id, struct_) )
      |> Hashtbl.data )
      @ [ (let id = snd deserialize_intf in
           (id, make_load_result_with_id (-500) id SelfType) ) ]

    let default_intfs =
      [ (serialize_intf_id, serialize_intf);
        (deserialize_intf_id, fst deserialize_intf) ]

    let std = [%blob "std/std.tact"]
  end

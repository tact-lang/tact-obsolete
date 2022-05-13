open Base

module Make =
functor
  (Syntax : Syntax.T)
  ->
  struct
    open Errors
    include Lang_types
    open Interpreter

    type error =
      [`DuplicateField of string * struct_ | `UnresolvedIdentifier of string]
    [@@deriving equal, sexp_of]

    let default_bindings =
      [ ("Int257", Value (Builtin "Int257"));
        ("Bool", Value (Builtin "Bool"));
        ("Type", Value (Builtin "Type"));
        ("Void", Value Void) ]

    class ['s] constructor ((bindings, errors) : expr named_map * _ errors) =
      object (s : 's)
        inherit ['s] Syntax.visitor as super

        (* Bindings in scope *)
        val mutable current_bindings = [bindings]

        (* Bindings that will available only in runtime *)
        val mutable runtime_bindings : type_ named_map list = []

        (* Next structure definition will get this ID *)
        val mutable next_struct_id = 0

        (* Are we inside of a function body? How deep? *)
        val mutable functions = 0

        (* Program handle we pass to builtin functions *)
        val program = {bindings; stmts = []}

        method build_CodeBlock _env _code_block = Invalid

        method build_Enum _env _enum = InvalidExpr

        method build_FieldAccess _env _fieldaccess = InvalidExpr

        method build_Function _env fn = Value (Function (Fn fn))

        method build_FunctionCall _env fc = FunctionCall (fc, ref None)

        method build_Ident _env string_ = string_

        method build_If _env _if = Invalid

        method build_Int _env i = Value (Integer i)

        method build_Interface _env _iface = InvalidExpr

        method build_Let _env let_ =
          let name, expr = Syntax.value let_ in
          match is_immediate_expr expr with
          | true ->
              current_bindings <- [(name, expr)] :: current_bindings ;
              Let [(name, expr)]
          | false ->
              let ty = expr_to_type expr in
              runtime_bindings <- [(name, ty)] :: runtime_bindings ;
              Let [(name, expr)]

        method build_MutRef _env _mutref = InvalidExpr

        method build_Reference env ref =
          match find_in_scope ref current_bindings with
          | Some (Reference (ref', _)) ->
              s#build_Reference env ref'
          | Some (Value value) ->
              Value value
          | Some _ ->
              (* TODO: type_of *) Reference (ref, HoleType)
          | None -> (
            match find_in_scope ref runtime_bindings with
            | Some ty ->
                Reference (ref, ty)
            | None ->
                errors#report `Error (`UnresolvedIdentifier ref) () ;
                Reference (ref, HoleType) )

        method build_Return _env return = Return return

        method build_Break _env stmt = Break stmt

        method build_Struct _env s = Value (Struct s)

        method build_StructConstructor _env _sc = InvalidExpr

        method build_Union _env _union = InvalidExpr

        method build_Expr _env expr = Expr expr

        method! visit_expr env syntax_expr =
          let expr' = super#visit_expr env syntax_expr in
          match is_immediate_expr expr' && equal functions 0 with
          | true ->
              let inter = new interpreter (current_bindings, errors) in
              let value' = inter#interpret_expr expr' in
              Value value'
          | false ->
              expr'

        method build_binding _env name expr =
          (Syntax.value name, Syntax.value expr)

        method build_enum_definition _env _members _bindings = ()

        method build_enum_member _env _name _value = ()

        method build_field_access _env _expr _field = ()

        method build_function_call _env fn args =
          (Syntax.value fn, s#of_located_list args)

        method! visit_function_definition env f =
          (* prepare parameter bindings *)
          let param_bindings =
            s#of_located_list f.params
            |> List.map ~f:(fun (ident, expr) ->
                   ( s#visit_ident env @@ Syntax.value ident,
                     s#visit_expr env @@ Syntax.value expr ) )
            |> List.map ~f:(fun (id, expr) -> (id, expr_to_type expr))
          in
          let bindings' = runtime_bindings in
          (* inject them into current bindings *)
          runtime_bindings <- param_bindings :: runtime_bindings ;
          (* process the function definition *)
          let result = super#visit_function_definition env f in
          (* restore bindings as before entering the function *)
          runtime_bindings <- bindings' ;
          result

        method! visit_function_body env body =
          (* save the function enclosure count *)
          let functions' = functions in
          (* increment function counter *)
          functions <- functions + 1 ;
          (* process the body *)
          let result = super#visit_function_body env body in
          (* restore function enclosure count *)
          functions <- functions' ;
          result

        method build_function_body _env stmts = s#of_located_list stmts

        method build_function_definition _env _name params returns body =
          let function_params =
            s#of_located_list params
            |> List.map ~f:(fun (name, type_) ->
                   (Syntax.value name, Syntax.value type_) )
          and function_returns =
            returns
            |> Option.map ~f:(fun x -> Syntax.value x)
            |> Option.value ~default:Hole
          and function_impl = body in
          {function_params; function_returns; function_impl}

        method build_if_ _env _condition _then _else = ()

        method build_interface_definition _env _members = ()

        method build_program _env stmts =
          { stmts = s#of_located_list stmts;
            bindings = List.concat current_bindings }

        method build_struct_constructor _env _id _fields = ()

        method build_struct_definition _env struct_fields _bindings =
          let fields = s#of_located_list struct_fields in
          let s' =
            { struct_fields = fields;
              struct_methods = [];
              (* TODO: methods *)
              struct_id = next_struct_id }
          in
          (* Check for duplicate fields *)
          ( match
              List.find_a_dup fields ~compare:(fun (name1, _) (name2, _) ->
                  String.compare name1 name2 )
            with
          | Some (name, _) ->
              errors#report `Error (`DuplicateField (name, s')) ()
          | None ->
              () ) ;
          (* Increment next struct's ID *)
          next_struct_id <- next_struct_id + 1 ;
          s'

        method build_struct_field _env field_name field_type =
          (Syntax.value field_name, {field_type = Syntax.value field_type})

        method build_union_definition _env _members _bindings = ()

        method private of_located_list : 'a. 'a Syntax.located list -> 'a list =
          List.map ~f:Syntax.value

        method private resolve ref =
          List.find_map current_bindings ~f:(fun bindings ->
              List.Assoc.find bindings ~equal:String.equal ref )
      end
  end

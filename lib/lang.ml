open Base

module Make =
functor
  (Syntax : Syntax.T)
  ->
  struct
    open Errors

    class ['s] base_map =
      object (_ : 's)
        inherit ['s] Zint.map
      end

    type 'a named_map = (string * 'a) list

    and program = {stmts : stmt list; [@sexp.list] bindings : expr named_map}

    and expr =
      | FunctionCall of (function_call * (expr option[@sexp.option]) ref)
        (* expr option is cached result *)
      | Type of type_
      | Reference of (string * type_)
      | Value of value
      | Hole
      | InvalidExpr

    and value =
      | Void
      | Struct of struct_
      | Function of function_
      | Integer of (Zint.t[@visitors.name "z"])
      | Builtin of builtin

    and stmt =
      | Let of expr named_map
      | Return of expr
      | Break of stmt
      | Expr of expr
      | Invalid

    and builtin = string

    and type_ =
      | TypeType
      | IntegerType
      | VoidType
      | BuiltinType of builtin
      | StructType of struct_
      | FunctionType of function_
      | HoleType
      | InvalidType

    and struct_ =
      { struct_fields : struct_field named_map;
        struct_methods : function_ named_map;
        struct_id : (int[@sexp.opaque]) }

    and struct_field = {field_type : expr}

    and 'a typed_fn =
      { function_params : expr named_map;
        function_returns : expr;
        function_impl : 'a }

    and function_body = (stmt list option[@sexp.option])

    and fn = function_body typed_fn

    and native_function =
      (program -> expr list -> expr[@visitors.opaque] [@equal.ignore])

    and builtin_fn = native_function typed_fn

    and function_ = Fn of fn | BuiltinFn of builtin_fn | InvalidFn

    and function_call = expr * expr list
    [@@deriving
      equal,
        sexp_of,
        visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]}]

    let rec expr_to_type = function
      | Type _ ->
          TypeType
      | Value (Struct struct_) ->
          StructType struct_
      | Value (Function function_) ->
          FunctionType function_
      | Value (Builtin builtin) ->
          BuiltinType builtin
      | Value (Integer _) ->
          IntegerType
      | Value Void ->
          VoidType
      | Hole ->
          HoleType
      | FunctionCall ((Value (Function (Fn {function_returns; _})), _), _)
      | FunctionCall ((Value (Function (BuiltinFn {function_returns; _})), _), _)
        ->
          expr_to_type function_returns
      | Reference (_, t) ->
          t
      | _ ->
          InvalidType

    let rec is_immediate_expr = function
      | Value _ ->
          true
      | FunctionCall ((_, args), _) ->
          are_immediate_arguments args
      | Hole ->
          false
      | Reference _ ->
          false
      | InvalidExpr ->
          false
      | Type _ ->
          false

    and are_immediate_arguments args =
      Option.is_none (List.find args ~f:(fun a -> not (is_immediate_expr a)))

    type error =
      [ `DuplicateField of string * struct_
      | `UnresolvedIdentifier of string
      | `UnexpectedExpr of expr
      | `Unsupported ]
    [@@deriving equal, sexp_of]

    let default_bindings =
      [ ("Int257", Value (Builtin "Int257"));
        ("Bool", Value (Builtin "Bool"));
        ("Type", Value (Builtin "Type"));
        ("Void", Value Void) ]

    class ['s] of_syntax_converter
      ((bindings, errors) : expr named_map * _ errors) =
      object (s : 's)
        inherit ['s] Syntax.visitor as super

        (* Bindings in scope *)
        val mutable current_bindings = [bindings]

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

        method build_FunctionCall env fc =
          s#handle_FunctionCall env (fc, ref None)

        method build_Ident _env string_ = string_

        method build_If _env _if = Invalid

        method build_Int _env i = Value (Integer i)

        method build_Interface _env _iface = InvalidExpr

        method build_Let _env let_ =
          let name, expr = Syntax.value let_ in
          current_bindings <- [(name, expr)] :: current_bindings ;
          Let [(name, expr)]

        method build_MutRef _env _mutref = InvalidExpr

        method build_Reference _env ref =
          match s#resolve ref with
          | Some v ->
              if equal functions 0 then v else Reference (ref, expr_to_type v)
          | None ->
              errors#report `Error (`UnresolvedIdentifier ref) () ;
              InvalidExpr

        method build_Return _env return = Return return

        method build_Break _env stmt = Break stmt

        method build_Struct _env s = Value (Struct s)

        method build_StructConstructor _env _sc = InvalidExpr

        method build_Union _env _union = InvalidExpr

        method build_Expr _env expr = Expr expr

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
          in
          let bindings' = current_bindings in
          (* inject them into current bindings *)
          current_bindings <- param_bindings :: current_bindings ;
          (* process the function definition *)
          let result = super#visit_function_definition env f in
          (* restore bindings as before entering the function *)
          current_bindings <- bindings' ;
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

        method private handle_FunctionCall env ((fn, args), result) =
          match !result with
          | Some t ->
              t
          | None -> (
            match fn with
            | Value (Function (BuiltinFn {function_impl; _}))
              when are_immediate_arguments args ->
                function_impl program args
            | Value
                (Function (Fn {function_params; function_impl = Some impl; _}))
              when are_immediate_arguments args
                   && equal (List.length function_params) (List.length args)
              -> (
                let bindings =
                  List.zip_exn function_params args
                  |> List.map ~f:(fun ((name, _), expr) -> (name, expr))
                in
                let rec interpret ?(return = InvalidExpr) bindings stmts =
                  let resolver =
                    object
                      inherit [_] map

                      method! visit_Reference _env (ref, _typ) =
                        let rec resolve ref =
                          match
                            List.Assoc.find bindings ~equal:String.equal ref
                          with
                          | Some (Reference (ref', _)) ->
                              resolve ref'
                          | Some t ->
                              t
                          | None ->
                              errors#report `Error (`UnresolvedIdentifier ref)
                                () ;
                              InvalidExpr
                        in
                        resolve ref
                    end
                  in
                  match stmts with
                  | [] ->
                      `Stop return
                  | Invalid :: _ ->
                      `Stop InvalidExpr
                  | Expr t :: rest ->
                      interpret bindings rest ~return:(resolver#visit_expr () t)
                  | Break t :: _ ->
                      interpret bindings [t]
                  | Return t :: _ ->
                      `Stop t
                  | Let let_bindings :: rest ->
                      interpret (let_bindings @ bindings) rest
                in
                match
                  interpret (bindings @ List.concat current_bindings) impl
                with
                | `Stop expr ->
                    result := Some expr ;
                    expr
                | _ ->
                    InvalidExpr )
            | Reference (_, FunctionType fn) ->
                s#handle_FunctionCall env ((Value (Function fn), args), result)
            | _ ->
                FunctionCall ((fn, args), result) )
      end
  end

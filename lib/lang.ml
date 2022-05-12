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
      | Reference of string
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
      | IntegerType
      | VoidType
      | BuiltinType of builtin
      | StructType of struct_
      | FunctionType of function_
      | HoleType
      | ReferenceType of string
      | InvalidType

    and struct_ =
      { struct_fields : struct_field named_map;
        struct_methods : function_ named_map;
        struct_id : (int[@sexp.opaque]) }

    and struct_field = {field_type : type_}

    and 'a typed_fn =
      { function_params : type_ named_map;
        function_returns : type_;
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

    let expr_to_type value =
      match value with
      | Value (Struct struct_) ->
          StructType struct_
      | Value (Function function_) ->
          FunctionType function_
      | Value (Builtin builtin) ->
          BuiltinType builtin
      | Value Void ->
          VoidType
      | Hole ->
          HoleType
      | FunctionCall ((Value (Function (Fn {function_returns; _})), _), _)
      | FunctionCall ((Value (Function (BuiltinFn {function_returns; _})), _), _)
        ->
          function_returns
      | Reference ref ->
          ReferenceType ref
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
        inherit ['s] Syntax.visitor

        val mutable next_struct_id = 0

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
          Let [(name, expr)]

        method build_MutRef _env _mutref = InvalidExpr

        method build_Reference _env ref = Reference ref

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

        method build_function_definition _env _name params returns body =
          { function_params =
              s#of_located_list params
              |> List.map ~f:(fun (name, type_) ->
                     (Syntax.value name, Syntax.value type_ |> expr_to_type) );
            function_returns =
              returns
              |> Option.map ~f:(fun x -> Syntax.value x |> expr_to_type)
              |> Option.value ~default:HoleType;
            function_impl = Option.map body ~f:s#of_located_list }

        method build_if_ _env _condition _then _else = ()

        method build_interface_definition _env _members = ()

        method build_program _env stmts =
          {stmts = s#of_located_list stmts; bindings}

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
          ( Syntax.value field_name,
            {field_type = Syntax.value field_type |> expr_to_type} )

        method build_union_definition _env _members _bindings = ()

        method private of_located_list : 'a. 'a Syntax.located list -> 'a list =
          List.map ~f:Syntax.value
      end

    class ['s] reference_resolver
      ((bindings, errors) : expr named_map * _ errors) =
      object (s : 's)
        inherit ['s] map as super

        val mutable current_bindings = [bindings]

        method! visit_Reference _env ref =
          match s#resolve ref with
          | Some t ->
              t
          | None ->
              errors#report `Error (`UnresolvedIdentifier ref) () ;
              Reference ref

        method! visit_ReferenceType _env ref =
          match s#resolve ref with
          | Some t ->
              expr_to_type t
          | None ->
              errors#report `Error (`UnresolvedIdentifier ref) () ;
              ReferenceType ref

        method! visit_Let env bindings =
          current_bindings <- bindings :: current_bindings ;
          super#visit_Let env bindings

        (* Do not cross the function boundary *)
        method! visit_function_body _env b = b

        method! visit_program env program =
          (* process statements first *)
          let program = super#visit_program env program in
          (* process new bindings *)
          let program' =
            super#visit_program env
              {stmts = []; bindings = List.concat current_bindings}
          in
          {(super#visit_program env program') with stmts = program.stmts}

        method private resolve ref =
          match
            List.find_map current_bindings ~f:(fun bindings ->
                List.Assoc.find bindings ~equal:String.equal ref )
          with
          | Some (Reference ref') ->
              s#resolve ref'
          | other ->
              other
      end
  end

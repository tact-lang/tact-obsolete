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
      | FunctionCall of (function_call * (value option[@sexp.option]) ref)
        (* expr option is cached result *)
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
      | Type of type_

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
      (program -> value list -> value[@visitors.opaque] [@equal.ignore])

    and builtin_fn = native_function typed_fn

    and function_ = Fn of fn | BuiltinFn of builtin_fn | InvalidFn

    and function_call = expr * expr list
    [@@deriving
      equal,
        sexp_of,
        visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]}]

    let rec expr_to_type = function
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
      | Value (Type type_) ->
          type_
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

    and are_immediate_arguments args =
      Option.is_none (List.find args ~f:(fun a -> not (is_immediate_expr a)))

    type error =
      [ `DuplicateField of string * struct_
      | `UnresolvedIdentifier of string
      | `UnexpectedExpr of expr
      | `Unsupported
      | `CannotInterpret of stmt
      | `NotEnoughArgument ]
    [@@deriving equal, sexp_of]

    let default_bindings =
      [ ("Int257", Value (Builtin "Int257"));
        ("Bool", Value (Builtin "Bool"));
        ("Type", Value (Builtin "Type"));
        ("Void", Value Void) ]

    let find_in_scope : 'a. string -> 'a named_map list -> 'a option =
     fun ref scope ->
      List.find_map scope ~f:(fun bindings ->
          Option.map
            (List.find bindings ~f:(fun (s, _) -> String.equal ref s))
            ~f:(fun (_name, a) -> a) )

    (*TODO: type checks for arguments*)
    class interpreter ((bindings, errors) : expr named_map list * _ errors) =
      object (self)
        val global_bindings = bindings

        val mutable vars_scope : value named_map list = []

        val mutable return = Void

        method interpret_stmt_list : stmt list -> value =
          fun stmts ->
            match stmts with
            | [] ->
                return
            | stmt :: rest ->
                self#interpret_stmt stmt rest

        method interpret_stmt stmt rest =
          match stmt with
          | Let binds -> (
              let values =
                List.map binds ~f:(fun (_, arg) -> self#interpret_expr arg)
              in
              match
                List.zip (List.map binds ~f:(fun (name, _) -> name)) values
              with
              | Ok args_scope ->
                  let prev_scope = vars_scope in
                  vars_scope <- args_scope :: vars_scope ;
                  let output = self#interpret_stmt_list rest in
                  vars_scope <- prev_scope ;
                  output
              | _ ->
                  errors#report `Error `NotEnoughArgument () ;
                  Void )
          | Break stmt ->
              self#interpret_stmt stmt []
          | Return expr ->
              self#interpret_expr expr
          | Expr expr ->
              let expr' = self#interpret_expr expr in
              return <- expr' ;
              self#interpret_stmt_list rest
          | Invalid ->
              Void

        method interpret_expr : expr -> value =
          fun expr ->
            match expr with
            | FunctionCall (fc, result) -> (
              match !result with
              | Some t ->
                  t
              | _ ->
                  let value = self#interpret_fc fc in
                  result := Some value ;
                  value )
            | Reference (name, _) -> (
              match self#find_ref name with
              | Some expr' ->
                  self#interpret_expr expr'
              | None ->
                  errors#report `Error (`UnresolvedIdentifier name) () ;
                  Void )
            | Value value ->
                self#interpret_value value
            | InvalidExpr | Hole ->
                errors#report `Error (`CannotInterpret (Expr expr)) () ;
                Void

        method interpret_value : value -> value =
          fun value ->
            match value with
            | Struct {struct_fields; struct_methods; struct_id} ->
                let struct_fields =
                  List.map struct_fields ~f:(fun (name, {field_type}) ->
                      ( name,
                        {field_type = Value (self#interpret_expr field_type)} ) )
                in
                Struct {struct_fields; struct_methods; struct_id}
            | value ->
                value

        method interpret_function : function_ -> function_ = fun f -> f

        method interpret_fc : function_call -> value =
          fun (func, args) ->
            let mk_err = Expr (FunctionCall ((func, args), ref None)) in
            let args' = List.map args ~f:(fun arg -> self#interpret_expr arg) in
            let args_to_list params values =
              match
                List.zip (List.map params ~f:(fun (name, _) -> name)) values
              with
              | Ok scope ->
                  Ok scope
              | _ ->
                  Error mk_err
            in
            match self#interpret_expr func with
            | Function f -> (
              match f with
              | Fn {function_params; function_impl; _} -> (
                  let args_scope = args_to_list function_params args' in
                  match args_scope with
                  | Ok args_scope -> (
                    match function_impl with
                    | Some body ->
                        let prev_scope = vars_scope in
                        vars_scope <- args_scope :: vars_scope ;
                        let output = self#interpret_stmt_list body in
                        vars_scope <- prev_scope ;
                        output
                    | None ->
                        Void )
                  | Error _ ->
                      Void )
              | BuiltinFn {function_impl; _} ->
                  let output =
                    function_impl
                      { stmts = [];
                        bindings =
                          Option.value (List.hd global_bindings) ~default:[] }
                      args'
                  in
                  output
              | _ ->
                  Void )
            | _ ->
                Void

        method private find_ref : string -> expr option =
          fun ref ->
            match find_in_scope ref vars_scope with
            | Some e ->
                Some (Value e)
            | None ->
                self#find_in_global_scope ref

        method private find_in_global_scope : string -> expr option =
          fun ref ->
            match find_in_scope ref global_bindings with
            | Some (Reference (ref', _)) ->
                self#find_in_global_scope ref'
            | not_ref ->
                not_ref
      end

    class ['s] of_syntax_converter
      ((bindings, errors) : expr named_map * _ errors) =
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

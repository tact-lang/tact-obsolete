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
      [ `DuplicateField of string * struct_
      | `UnresolvedIdentifier of string
      | `MethodNotFound of expr * string
      | `UnexpectedType of expr ]
    [@@deriving equal, sexp_of]

    include Builtin

    class ['s] constructor ((bindings, errors) : (string * expr) list * _ errors)
      =
      object (s : 's)
        inherit ['s] Syntax.visitor as super

        (* Bindings in scope *)
        val mutable current_bindings = [bindings]

        (* Bindings that will available only in runtime *)
        val mutable runtime_bindings = []

        (* Are we inside of a function body? How deep? *)
        val mutable functions = 0

        (* Program handle we pass to builtin functions *)
        val program = {bindings; stmts = []; methods = []}

        method build_CodeBlock _env _code_block = Invalid

        method build_Enum _env _enum = InvalidExpr

        method build_FieldAccess _env _fieldaccess = InvalidExpr

        method build_Function _env fn = Value (Function fn)

        method build_FunctionCall _env (f, args) =
          let fc = (f, args) in
          if is_immediate_expr (FunctionCall (f, args)) then
            let inter =
              new interpreter (program, current_bindings, errors, functions)
            in
            inter#interpret_fc fc
          else FunctionCall fc

        method build_MethodCall env mc = s#build_FunctionCall env mc

        method build_Ident _env string_ = string_

        method build_If _env _if = Invalid

        method build_Int _env i = Value (Integer i)

        method build_String _env s = Value (String s)

        method build_Interface _env _iface = InvalidExpr

        method build_Let _env let_ =
          let amend_bindings binding = function
            | [] ->
                [[binding]]
            | bindings :: rest ->
                (binding :: bindings) :: rest
          in
          let name, expr = Syntax.value let_ in
          match is_immediate_expr expr with
          | true ->
              current_bindings <- amend_bindings (name, expr) current_bindings ;
              Let [(name, expr)]
          | false ->
              let ty = expr_to_type expr in
              runtime_bindings <- amend_bindings (name, ty) runtime_bindings ;
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

        method build_StructConstructor _env sc = Value (StructInstance sc)

        method build_Union _env _union = InvalidExpr

        method build_Expr _env expr = Expr expr

        method! visit_expr env syntax_expr =
          let expr' = super#visit_expr env syntax_expr in
          match is_immediate_expr expr' && equal functions 0 with
          | true ->
              let inter =
                new interpreter (program, current_bindings, errors, functions)
              in
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

        method build_method_call _env receiver fn args =
          let receiver = Syntax.value receiver in
          let fn = Syntax.value fn
          and dummy : expr * expr list =
            ( Value
                (Function
                   { function_params = [];
                     function_returns = Value (Type VoidType);
                     function_impl =
                       BuiltinFn (builtin_fun (fun _ _ -> Value Void)) } ),
              [] )
          in
          (* TODO: check method signatures *)
          match receiver with
          | Value (Struct struct') -> (
              let methods =
                List.Assoc.find program.methods ~equal:equal_value
                  (Struct struct')
              in
              match
                Option.bind methods ~f:(fun methods ->
                    List.Assoc.find methods ~equal:String.equal fn )
              with
              | Some fn' ->
                  (Value (Function fn'), s#of_located_list args)
              | None ->
                  errors#report `Error (`MethodNotFound (receiver, fn)) () ;
                  dummy )
          | Value (StructInstance (struct', _)) -> (
              let methods =
                List.Assoc.find program.methods ~equal:equal_value
                  (Struct struct')
              in
              match
                Option.bind methods ~f:(fun methods ->
                    List.Assoc.find methods ~equal:String.equal fn )
              with
              | Some fn' ->
                  (Value (Function fn'), receiver :: s#of_located_list args)
              | None ->
                  errors#report `Error (`MethodNotFound (receiver, fn)) () ;
                  dummy )
          | receiver ->
              errors#report `Error (`UnexpectedType receiver) () ;
              dummy

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
          (* new binding scope *)
          let current_bindings' = current_bindings
          and runtime_bindings' = runtime_bindings in
          current_bindings <- [] :: current_bindings ;
          runtime_bindings <- [] :: runtime_bindings ;
          (* process the body *)
          let result = super#visit_function_body env body in
          (* drop binding scope *)
          current_bindings <- current_bindings' ;
          runtime_bindings <- runtime_bindings' ;
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
          {function_params; function_returns; function_impl = Fn function_impl}

        method build_if_ _env _condition _then _else = ()

        method build_interface_definition _env _members = ()

        method build_program _env stmts =
          { program with
            stmts = s#of_located_list stmts;
            bindings = List.concat current_bindings }

        method build_struct_constructor _env id _fields =
          match Syntax.value id with
          | Value (Struct struct') ->
              (struct', []) (* TODO: handle fields *)
          | e ->
              errors#report `Error (`UnexpectedType e) () ;
              ({struct_fields = []; struct_id = 0}, [])

        method build_struct_definition _env struct_fields _bindings =
          let struct_fields = s#of_located_list struct_fields in
          let s' = {struct_fields; struct_id = !struct_counter} in
          (* Check for duplicate fields *)
          ( match
              List.find_a_dup struct_fields
                ~compare:(fun (name1, _) (name2, _) ->
                  String.compare name1 name2 )
            with
          | Some (name, _) ->
              errors#report `Error (`DuplicateField (name, s')) ()
          | None ->
              () ) ;
          (* Increment next struct's ID *)
          struct_counter := !struct_counter + 1 ;
          s'

        method! visit_struct_definition env _visitors_this =
          let _visitors_r0 =
            s#visit_list
              (s#visit_located s#visit_struct_field)
              env _visitors_this.fields
          in
          let struct_ = s#build_struct_definition env _visitors_r0 []
          and current_bindings' = current_bindings in
          current_bindings <-
            [("Self", Value (Struct struct_))] :: current_bindings' ;
          let bindings =
            s#visit_list
              (s#visit_located s#visit_binding)
              env _visitors_this.struct_bindings
          in
          current_bindings <- current_bindings' ;
          let struct_methods =
            List.filter_map bindings ~f:(fun binding ->
                let name, expr = Syntax.value binding in
                match expr with
                | Value (Function f) ->
                    Some (name, f)
                | _ ->
                    None )
          in
          program.methods <- (Struct struct_, struct_methods) :: program.methods ;
          struct_

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

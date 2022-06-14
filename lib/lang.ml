open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Errors
    include Lang_types
    open Interpreter
    open Type_check
    module Syntax = Syntax.Make (Config)

    type error =
      [ `DuplicateField of string * mk_struct
      | `UnresolvedIdentifier of string
      | `MethodNotFound of expr * string
      | `UnexpectedType of type_
      | `TypeError of type_ * type_
      | `ExpectedFunction of type_
      | `UnallowedStmt of stmt
      | `OnlyFunctionIsAllowed ]
    [@@deriving equal, sexp_of]

    include Builtin

    (* If we have signature fn(X: Type) -> X, so
       we need to change it to fn(X: Type) -> Dependent(X) *)
    class ['s] make_dependent_types (_errors : _) =
      object (self : 's)
        inherit ['s] Lang_types.map as super

        val mutable previous_arguments = []

        method! visit_Reference _ (ref, ty) =
          let concrete_ty =
            List.find_map previous_arguments ~f:(fun (name, x) ->
                if equal_string name ref then Some x else None )
          in
          match concrete_ty with
          | Some ty' ->
              Value (Type (Dependent (ref, ty')))
          | None ->
              Value (Type (ExprType (Reference (ref, ty))))

        (* Unwrap ExprType(Value(Type(ty))) -> ty *)
        method! visit_type_ env type_ =
          let new_type = super#visit_type_ env type_ in
          match new_type with ExprType (Value (Type t)) -> t | t -> t

        method! visit_function_signature env sign =
          let prev = previous_arguments in
          let function_params =
            List.map
              ~f:(fun (name, ty) ->
                let ty' = self#visit_type_ env ty in
                let arg = (name, ty') in
                previous_arguments <- arg :: previous_arguments ;
                arg )
              sign.function_params
          in
          let function_returns = self#visit_type_ env sign.function_returns in
          previous_arguments <- prev ;
          {function_params; function_returns}
      end

    class ['s] constructor (bindings : (string * expr) list)
      (structs : (int * struct_) list) (unions : (int * union) list)
      (errors : _ errors) =
      object (s : 's)
        inherit ['s] Syntax.visitor as super

        (* Bindings in scope *)
        val mutable current_bindings = [List.map bindings ~f:make_comptime]

        val type_checker = new type_checker errors 0

        (* Are we inside of a function body? How deep? *)
        val mutable functions = 0

        (* TODO: can we remove duplicating bindings here and the above? *)
        (* Program handle we pass to builtin functions. *)
        (* IDs from 0 to 99 inlusevily is reserved for built-in structs. *)
        val mutable program =
          {bindings; structs; unions; struct_counter = 100; memoized_fcalls = []}

        method build_CodeBlock _env code_block =
          Block (s#of_located_list code_block)

        method! visit_CodeBlock env block =
          s#with_bindings [] (fun _ -> super#visit_CodeBlock env block)

        method build_Enum _env _enum = InvalidExpr

        method build_FieldAccess _env _fieldaccess = InvalidExpr

        method build_Function _env fn = Value (Function fn)

        method build_FunctionCall _env (f, args) =
          match type_of f with
          | FunctionType sign -> (
              let no_errors = ref true in
              let types_satisfying =
                List.map2 sign.function_params args
                  ~f:(fun (_, expected) expr ->
                    match s#check_type ~expected (type_of expr) with
                    | Ok _ ->
                        expr
                    | Error (NeedFromCall func) ->
                        let s = FunctionCall (func, [expr]) in
                        s
                    | _ ->
                        errors#report `Error
                          (`TypeError (expected, type_of expr))
                          () ;
                        no_errors := false ;
                        Value Void )
              in
              match types_satisfying with
              | Ok args' when !no_errors ->
                  let fc = (f, args') in
                  if is_immediate_expr (FunctionCall (f, args')) then
                    let inter =
                      new interpreter
                        (program, current_bindings, errors, functions)
                    in
                    let fc = inter#interpret_fc fc in
                    Value fc
                  else FunctionCall fc
              | _ ->
                  Value Void )
          | ty ->
              errors#report `Error (`ExpectedFunction ty) () ;
              Value Void

        method build_MethodCall env mc = s#build_FunctionCall env mc

        method build_Ident _env string_ = string_

        method build_If _env if_ = If if_

        method build_Int _env i = Value (Integer i)

        method build_Bool _env b = Value (Bool b)

        method build_String _env s = Value (String s)

        method build_Interface _env iface = Value (Type (InterfaceType iface))

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
              current_bindings <-
                amend_bindings (make_comptime (name, expr)) current_bindings ;
              Let [(name, expr)]
          | false ->
              let ty = type_of expr in
              current_bindings <-
                amend_bindings (make_runtime (name, ty)) current_bindings ;
              Let [(name, expr)]

        method build_MutRef _env _mutref = InvalidExpr

        method build_Reference env ref =
          match find_in_scope ref current_bindings with
          | Some (Runtime ty) ->
              Reference (ref, ty)
          | Some (Comptime (Reference (ref', _))) ->
              s#build_Reference env ref'
          | Some (Comptime (Value value)) ->
              ResolvedReference (ref, Value value)
          | Some (Comptime ex) ->
              Reference (ref, type_of ex)
          | None ->
              errors#report `Error (`UnresolvedIdentifier ref) () ;
              Reference (ref, HoleType)

        method build_Return _env return =
          match functions with
          | 0 ->
              errors#report `Error (`UnallowedStmt (Return return)) () ;
              Return return
          | _ -> (
            match
              type_checker#check_return_type (type_of return) ~program
                ~current_bindings
            with
            | Ok _ ->
                Return return
            | Error (NeedFromCall func) ->
                Break (Expr (FunctionCall (func, [return])))
            | Error (TypeError fn_returns) ->
                errors#report `Error
                  (`TypeError (fn_returns, type_of return))
                  () ;
                Return return )

        method build_Break _env stmt =
          match stmt with
          | Expr ex -> (
            match functions with
            | 0 ->
                raise InternalCompilerError
            | _ -> (
              match
                type_checker#check_return_type (type_of ex) ~program
                  ~current_bindings
              with
              | Ok _ ->
                  Break stmt
              | Error (NeedFromCall func) ->
                  Break (Expr (FunctionCall (func, [ex])))
              | Error (TypeError fn_returns) ->
                  errors#report `Error (`TypeError (fn_returns, type_of ex)) () ;
                  Break stmt ) )
          | stmt ->
              Break stmt

        method build_Struct _env s = MkStructDef s

        method build_StructConstructor _env sc = Value (Struct sc)

        method build_Union _env union = MkUnionDef union

        method build_Expr _env expr = Expr expr

        method build_impl _env intf bindings =
          { impl_interface = Syntax.value intf;
            impl_methods = s#of_located_list bindings }

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
                   { function_signature =
                       {function_params = []; function_returns = VoidType};
                     function_impl = BuiltinFn (builtin_fun (fun _ _ -> Void))
                   } ),
              [] )
          in
          let make_call receiver ~mk_args =
            let receiver' = Value (Type (StructType receiver)) in
            match
              (Program.get_struct program receiver).struct_methods
              |> fun ms -> List.Assoc.find ms fn ~equal:String.equal
            with
            | Some fn' ->
                ( ResolvedReference (fn, Value (Function fn')),
                  mk_args (s#of_located_list args) )
            | None ->
                errors#report `Error (`MethodNotFound (receiver', fn)) () ;
                dummy
          in
          (* TODO: check method signatures *)
          match receiver with
          | ResolvedReference (_, Value (Type (StructType st)))
          | Value (Type (StructType st)) ->
              make_call st ~mk_args:(fun x -> x)
          | ResolvedReference (_, Value (Struct (st, _)))
          | Value (Struct (st, _)) ->
              make_call st ~mk_args:(fun args -> receiver :: args)
          | receiver' ->
              errors#report `Error (`UnexpectedType (type_of receiver')) () ;
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
          let function_returns =
            s#with_bindings
              (List.map param_bindings ~f:(fun (name, ty) ->
                   (name, Comptime (Value (Type (Dependent (name, ty))))) ) )
              (fun _ ->
                f.returns
                |> Option.map ~f:(fun x ->
                       expr_to_type (s#visit_expr env (Syntax.value x)) )
                |> Option.value ~default:HoleType )
          in
          let body, fn_returns =
            s#with_bindings (List.map param_bindings ~f:make_runtime) (fun _ ->
                type_checker#with_fn_returns env function_returns (fun env' ->
                    s#visit_option s#visit_function_body env' f.function_body ) )
          in
          let function_signature =
            let sign =
              {function_params = param_bindings; function_returns = fn_returns}
            in
            let sig_maker = new make_dependent_types errors in
            sig_maker#visit_function_signature () sign
          in
          {function_signature; function_impl = Fn body}

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

        method build_function_body _env stmt = stmt

        method build_function_definition _ _ _ _ _ = raise InternalCompilerError

        method build_if_ _env if_condition if_then if_else =
          { if_condition = Syntax.value if_condition;
            if_then = Syntax.value if_then;
            if_else = Option.map if_else ~f:Syntax.value }

        method build_interface_definition _env members =
          let signatures =
            List.filter_map (s#of_located_list members) ~f:(fun (name, x) ->
                match x with
                | Value (Function f) ->
                    Some (name, f.function_signature)
                | _ ->
                    errors#report `Error `OnlyFunctionIsAllowed () ;
                    None )
          in
          {interface_methods = signatures}

        method! visit_interface_definition env def =
          let value =
            s#with_bindings
              [make_comptime ("Self", Value (Type SelfType))]
              (fun _ -> super#visit_interface_definition env def)
          in
          value

        method build_program _env _ =
          { program with
            bindings = extract_comptime_bindings (List.concat current_bindings)
          }

        method build_struct_constructor _env id fields =
          match Syntax.value id with
          | ResolvedReference (_, Value (Type (StructType struct')))
          | Value (Type (StructType struct')) ->
              ( struct',
                List.map fields ~f:(fun (name, expr) ->
                    (Syntax.value name, Syntax.value expr) ) )
          | e ->
              errors#report `Error (`UnexpectedType (type_of e)) () ;
              (-1, [])

        method build_struct_definition _env struct_fields bindings impls =
          let mk_struct_fields = s#of_located_list struct_fields in
          let mk_methods =
            List.filter_map bindings ~f:(fun binding ->
                let name, expr = Syntax.value binding in
                match expr with
                | Value (Function f) ->
                    Some (name, f)
                | _ ->
                    None )
          in
          let impl_methods =
            List.concat
              (List.map impls ~f:(fun impl ->
                   List.filter_map impl.impl_methods ~f:(fun (name, ex) ->
                       match ex with
                       | Value (Function f) ->
                           Some (name, f)
                       | _ ->
                           None ) ) )
          in
          let s' =
            { mk_struct_fields;
              mk_methods = mk_methods @ impl_methods;
              mk_impls = impls;
              mk_struct_id = -1 }
          in
          (* Check for duplicate fields *)
          ( match
              List.find_a_dup mk_struct_fields
                ~compare:(fun (name1, _) (name2, _) ->
                  String.compare name1 name2 )
            with
          | Some (name, _) ->
              errors#report `Error (`DuplicateField (name, s')) ()
          | None ->
              () ) ;
          s'

        method! visit_struct_definition env syn_struct_def =
          let fields =
            s#visit_list
              (s#visit_located s#visit_struct_field)
              env syn_struct_def.fields
          in
          let struct_ =
            { struct_fields =
                List.map (s#of_located_list fields) ~f:(fun (name, expr) ->
                    (name, {field_type = ExprType expr}) );
              struct_methods = [];
              struct_impls = [];
              struct_id = program.struct_counter }
          in
          program.struct_counter <- program.struct_counter + 1 ;
          let mk_struct =
            Program.with_struct program struct_ (fun _ ->
                let methods =
                  s#with_bindings
                    [ make_comptime
                        ("Self", Value (Type (StructType struct_.struct_id))) ]
                    (fun _ ->
                      s#visit_list
                        (s#visit_located s#visit_binding)
                        env syn_struct_def.struct_bindings )
                in
                let impls =
                  s#with_bindings
                    [ make_comptime
                        ("Self", Value (Type (StructType struct_.struct_id))) ]
                    (fun _ -> s#visit_list s#visit_impl env syn_struct_def.impls)
                in
                let mk_struct =
                  s#build_struct_definition env fields methods impls
                in
                Error {mk_struct with mk_struct_id = struct_.struct_id} )
          in
          let mk_struct =
            match mk_struct with
            | Error mk ->
                mk
            | Ok _ ->
                raise InternalCompilerError
          in
          mk_struct

        method build_struct_field : _ -> _ -> _ -> string * expr =
          fun _env field_name field_type ->
            (Syntax.value field_name, Syntax.value field_type)

        method build_union_definition _ _ _ = raise InternalCompilerError

        method! visit_union_definition env def =
          let members =
            s#visit_list (s#visit_located s#visit_expr) env def.union_members
          in
          let cases = s#of_located_list members in
          Program.with_union_id program
            (fun id ->
              { cases =
                  List.map cases ~f:(fun x -> (expr_to_type x, Discriminator 0));
                union_methods = [];
                union_impls = [];
                union_id = id } )
            (fun u_base ->
              let methods =
                s#with_bindings
                  [ make_comptime
                      ("Self", Value (Type (UnionType u_base.union_id))) ]
                  (fun _ ->
                    s#visit_list
                      (s#visit_located s#visit_binding)
                      env def.union_bindings )
                |> s#of_located_list
                |> List.map ~f:(fun (name, e) ->
                       match e with
                       | Value (Function f) ->
                           (name, f)
                       | _ ->
                           raise InternalCompilerError )
              in
              let convert_impls = s#make_from_impls cases u_base.union_id in
              Error
                { mk_cases = cases;
                  mk_union_id = u_base.union_id;
                  mk_union_impls = convert_impls;
                  mk_union_methods = methods } )
          |> Result.error |> Option.value_exn

        method private of_located_list : 'a. 'a Syntax.located list -> 'a list =
          List.map ~f:Syntax.value

        method private check_type ~expected actual =
          type_checker#check_type ~program ~current_bindings ~expected actual

        method private with_bindings : 'a. tbinding list -> (unit -> 'a) -> 'a =
          fun added_bindings f ->
            let current_bindings' = current_bindings in
            current_bindings <- added_bindings :: current_bindings ;
            let result = f () in
            current_bindings <- current_bindings' ;
            result

        method private make_from_impls : expr list -> int -> impl list =
          fun cases union ->
            List.map cases ~f:(fun case ->
                let from_intf_ =
                  FunctionCall (from_intf, [Value (Type (ExprType case))])
                in
                { impl_interface = from_intf_;
                  impl_methods = [("from", s#make_from_impl_fn case union)] } )

        method private make_from_impl_fn case union =
          Value
            (Function
               { function_signature =
                   { function_params = [("v", ExprType case)];
                     function_returns = UnionType union };
                 function_impl =
                   Fn
                     (Some
                        (Return
                           (MakeUnionVariant
                              (Reference ("v", ExprType case), union) ) ) ) } )
      end
  end

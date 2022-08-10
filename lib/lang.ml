open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Errors
    open Config
    module Builtin = Builtin.Make (Config)
    module Lang_types = Lang_types.Make (Config)
    include Lang_types

    open Interpreter.Make (Config)

    open Type_check.Make (Config)

    module Syntax = Syntax.Make (Config)

    open Partial_evaluator.Make (Config)

    type error =
      [ `DuplicateField of string located * mk_struct
      | `UnresolvedIdentifier of string located
      | `MethodNotFound of expr * string located
      | `IsNotStruct of expr
      | `IsNotUnion of expr
      | `CannotHaveMethods of expr * type_
      | `TypeError of type_ * type_ * (span[@equal.ignore] [@sexp.opaque])
      | `ExpectedFunction of type_ * (span[@equal.ignore] [@sexp.opaque])
      | `OnlyFunctionIsAllowed of (span[@equal.ignore] [@sexp.opaque])
      | `MissingField of
        type_ * string located * (span[@equal.ignore] [@sexp.opaque])
      | `FieldNotFoundF of string located
      | `FieldNotFound of expr * string located
      | `CaseNotFound of (span[@equal.ignore] [@sexp.opaque])
      | `ArgumentNumberMismatch of
        int * int * (span[@equal.ignore] [@sexp.opaque])
      | `ExpectedTypeFunction of bool * (span[@equal.ignore] [@sexp.opaque]) ]
    [@@deriving equal, sexp_of]

    include Builtin

    class ['s] self_type_updater (new_ty : type_) =
      object (_ : 's)
        inherit ['s] map

        method! visit_SelfType _ = new_ty
      end

    (*  *)
    class ['s] self_ref_updater (new_expr : expr_kind) =
      object (self : 's)
        inherit ['s] map as super

        method! visit_Reference env (name, ty) =
          if String.equal name.value "Self" then new_expr
          else Reference (name, self#visit_type_ env ty)

        method! visit_type_ env =
          function
          | ExprType {value = Value (Type t); _} ->
              self#visit_type_ env t
          | ty ->
              super#visit_type_ env ty
      end

    (* Span that is passed from the `located` type. *)
    type constr_ctx =
      {expr_span : (span[@sexp.opaque]) option; is_inside_condition_block : bool}
    [@@deriving sexp_of]

    let default_ctx = {expr_span = None; is_inside_condition_block = false}

    (* Skip statements that can be invalid because of reported errors. *)
    exception Skip

    class ['s] constructor ?(program = default_program ()) (errors : _ errors) =
      object (s : 's)
        inherit ['s] Syntax.visitor as super

        method get_errors = errors

        (* Bindings in scope *)
        val current_bindings = ref [List.map program.bindings ~f:make_comptime]

        (* method get_bindings = current_bindings *)

        val type_checker = new type_checker errors 0

        (* Are we inside of a function body? How deep? *)
        val mutable functions = 0

        (* TODO: can we remove duplicating bindings here and the above? *)
        (* Program handle we pass to builtin functions. *)
        val mutable program = program

        method build_attribute _env attribute_ident attribute_exprs =
          {attribute_ident; attribute_exprs}

        method build_CodeBlock _env code_block = Block code_block

        method! visit_CodeBlock env block =
          s#with_bindings [] (fun _ -> super#visit_CodeBlock env block)

        method build_Enum _env _enum = InvalidExpr

        method build_FieldAccess _env fieldaccess = StructField fieldaccess

        method! visit_FieldAccess env fa =
          s#visit_field_access env fa |> fun f -> s#build_FieldAccess env f

        method build_Function _env fn = MkFunction fn

        method build_FunctionCall _env (f, args, is_type_fn) =
          let span =
            merge_spans f.span (merge_spans_list @@ List.map args ~f:span)
          in
          match type_of program f with
          | FunctionType sign -> (
              if not @@ Bool.equal is_type_fn sign.value.function_is_type then (
                errors#report `Error
                  (`ExpectedTypeFunction (sign.value.function_is_type, span))
                  () ;
                Value Void )
              else
                let no_errors = ref true in
                let types_satisfying =
                  List.map2 sign.value.function_params args
                    ~f:(fun (_, expected) expr ->
                      match s#check_type ~expected expr with
                      | Ok _ ->
                          expr
                      | Error (NeedFromCall func) ->
                          let s = FunctionCall (func, [expr], false) in
                          {value = s; span = expr.span}
                      | _ ->
                          errors#report `Error
                            (`TypeError
                              (expected, type_of program expr, expr.span) )
                            () ;
                          no_errors := false ;
                          {value = Value Void; span = expr.span} )
                in
                match types_satisfying with
                | Ok args' when !no_errors ->
                    let fc = (f, args', is_type_fn) in
                    if
                      is_immediate_expr !current_bindings program
                        {value = FunctionCall (f, args', is_type_fn); span}
                    then
                      let fc =
                        let inter = s#make_interpreter span in
                        let fc = inter#interpret_fc fc in
                        fc
                      in
                      Value fc
                    else FunctionCall fc
                | _ ->
                    let expected = List.length sign.value.function_params in
                    let actual = List.length args in
                    s#report `Error
                      (`ArgumentNumberMismatch (expected, actual, f.span)) )
          | ty ->
              s#report `Error (`ExpectedFunction (ty, f.span))

        method build_MethodCall _env mc = mc

        method build_Ident _env string_ = string_

        method build_Int _env i = Value (Integer i)

        method build_Bool _env b = Value (Bool b)

        method build_String _env s = Value (String s)

        method build_Interface _env intf = MkInterfaceDef intf

        method build_Let : _ -> (string located * expr) located -> _ =
          fun _env let_ ->
            let amend_bindings binding = function
              | [] ->
                  [[binding]]
              | bindings :: rest ->
                  (binding :: bindings) :: rest
            in
            let name, expr = Syntax.value let_ in
            match is_immediate_expr !current_bindings program expr with
            | true ->
                current_bindings :=
                  amend_bindings (make_comptime (name, expr)) !current_bindings ;
                Let [(name, expr)]
            | false ->
                let ty = type_of program expr in
                current_bindings :=
                  amend_bindings (make_runtime (name, ty)) !current_bindings ;
                Let [(name, expr)]

        method build_DestructuringLet _env let_ =
          let amend_bindings binding = function
            | [] ->
                [[binding]]
            | bindings :: rest ->
                (binding :: bindings) :: rest
          in
          let initial_span = let_.span in
          let let_ = Syntax.value let_ in
          let st_ty = type_of program let_.destructuring_let_expr in
          let fields =
            match st_ty with
            | StructType id ->
                (Program.get_struct program id).struct_fields
                |> List.map ~f:(fun (n, ty) -> (n, ty.field_type))
            | ExprType ex -> (
              match type_of program ex with
              | StructSig id ->
                  (Arena.get program.struct_signs id).st_sig_fields
                  |> List.map ~f:(fun (n, exty) ->
                         (n, expr_to_type program exty) )
              | _ ->
                  s#report `Error (`IsNotStruct let_.destructuring_let_expr) )
            | _ ->
                s#report `Error (`IsNotStruct let_.destructuring_let_expr)
          in
          (* Check if field names are correct *)
          List.iter let_.destructuring_let ~f:(fun (name, name2) ->
              match
                List.find fields ~f:(fun (n, _) ->
                    String.equal n.value name.value )
              with
              | Some (_, ty) ->
                  current_bindings :=
                    amend_bindings (make_runtime (name2, ty)) !current_bindings
              | _ ->
                  errors#report `Error
                    (`FieldNotFound
                      ({value = Value (Type st_ty); span = name2.span}, name) )
                    () ) ;
          (* If rest of fields are not ignored, check for completeness *)
          if let_.destructuring_let_rest then ()
          else
            List.iter fields ~f:(fun (name, _) ->
                if
                  List.Assoc.find let_.destructuring_let
                    ~equal:(equal_located String.equal)
                    name
                  |> Option.is_some
                then ()
                else
                  errors#report `Error
                    (`MissingField (st_ty, name, initial_span))
                    () ) ;
          DestructuringLet let_

        method build_Assignment env assignment =
          let {assignment_ident; assignment_expr; _} = assignment in
          let make name expr =
            match
              is_immediate_expr !current_bindings program expr
              && not env.is_inside_condition_block
            with
            | true ->
                make_comptime (name, expr)
            | false ->
                make_runtime (name, type_of program expr)
          in
          (* Update individual bindings *)
          let rec update' = function
            | [] ->
                None
            | (name, _) :: rest
              when equal_located String.equal name assignment_ident ->
                Some (make name assignment_expr :: rest)
            | binding :: rest -> (
              match update' rest with
              | Some updated ->
                  Some (binding :: updated)
              | None ->
                  None )
          in
          (* Update binding sets *)
          let rec update = function
            | [] ->
                errors#report `Error (`UnresolvedIdentifier assignment_ident) () ;
                []
            | binding_set :: bindings -> (
              match update' binding_set with
              | Some binding_set' ->
                  binding_set' :: bindings
              | None ->
                  binding_set :: update bindings )
          in
          let right =
            match find_in_scope assignment_ident.value !current_bindings with
            | Some bind -> (
                let expected =
                  match bind with
                  | Comptime ex ->
                      type_of program ex
                  | Runtime rt ->
                      rt
                in
                match s#check_type ~expected assignment_expr with
                | Ok _ ->
                    assignment_expr
                | Error (NeedFromCall func) ->
                    let s = FunctionCall (func, [assignment_expr], false) in
                    {value = s; span = assignment_expr.span}
                | _ ->
                    errors#report `Error
                      (`TypeError
                        ( expected,
                          type_of program assignment_expr,
                          assignment_expr.span ) )
                      () ;
                    {value = Value Void; span = assignment_expr.span} )
            | None ->
                assignment_expr
          in
          current_bindings := update !current_bindings ;
          Assignment {assignment_ident; assignment_expr = right}

        method build_MutRef _env _mutref = InvalidExpr

        method build_Reference : _ -> string located -> _ =
          fun _ ref ->
            match find_in_scope ref.value !current_bindings with
            | Some (Runtime ty) ->
                Reference (ref, ty)
            | Some (Comptime ex) ->
                ResolvedReference (ref, ex)
            | None ->
                s#report `Error (`UnresolvedIdentifier ref)

        method build_Return _env return =
          let typecheck =
            if functions = 0 then Ok VoidType
            else
              type_checker#check_return_type return ~program ~current_bindings
          in
          match typecheck with
          | Ok _ ->
              Return return
          | Error (NeedFromCall func) ->
              Return
                { value = FunctionCall (func, [return], false);
                  span = return.span }
          | Error (TypeError fn_returns) ->
              errors#report `Error
                (`TypeError (fn_returns, type_of program return, return.span))
                () ;
              Return return

        method build_Break _env stmt =
          match stmt.value with
          | Expr ex -> (
            match functions with
            | 0 ->
                unreachable ()
            | _ -> (
              match
                type_checker#check_return_type ex ~program ~current_bindings
              with
              | Ok _ ->
                  Break stmt
              | Error (NeedFromCall func) ->
                  Break
                    { value =
                        Expr
                          { value = FunctionCall (func, [ex], false);
                            span = stmt.span };
                      span = stmt.span }
              | Error (TypeError fn_returns) ->
                  errors#report `Error
                    (`TypeError (fn_returns, type_of program ex, ex.span))
                    () ;
                  Break stmt ) )
          | _ ->
              Break stmt

        method build_Switch _ s = Switch s

        method build_switch_branch _env _ _ _ = unreachable ()

        method! visit_switch_branch env b =
          let ty =
            expr_to_type program @@ s#visit_located s#visit_expr env b.ty
          in
          let ref = s#visit_located s#visit_ident env b.var in
          let stmt =
            s#with_bindings
              [make_runtime (ref, ty)]
              (fun _ ->
                let stmt =
                  s#visit_located s#visit_stmt
                    {env with is_inside_condition_block = true}
                    b.stmt
                in
                stmt )
          in
          {branch_ty = ty; branch_var = ref; branch_stmt = stmt}

        method build_switch _env cond branches _default =
          let cond_type = type_of program cond in
          match Program.get_union_cases program cond_type with
          | Some cases ->
              List.iter branches ~f:(fun b ->
                  let actual_ty = b.value.branch_ty in
                  if not @@ List.exists cases ~f:(equal_type_ actual_ty) then
                    errors#report `Error (`CaseNotFound b.value.branch_var.span)
                      () ) ;
              {switch_condition = cond; branches}
          | _ ->
              errors#report `Error (`IsNotUnion cond) () ;
              {switch_condition = cond; branches}

        method! visit_switch env switch =
          let cond = s#visit_located s#visit_expr env switch.switch_condition in
          let branches =
            s#visit_list_filter_skipped
              (s#visit_located s#visit_switch_branch)
              env switch.branches
          in
          let default = s#visit_option s#visit_stmt env switch.default in
          s#build_switch env cond branches default

        method build_Struct _env s = MkStructDef s

        method build_StructConstructor _env sc = Value (Struct sc)

        method build_Union _env union = MkUnionDef union

        method build_Expr _env expr = Expr expr

        method build_impl _env attributes intf bindings =
          { mk_impl_attributes = attributes;
            mk_impl_interface = intf;
            mk_impl_methods = s#of_located_list bindings }

        method! visit_located
            : 'a 'b.
              (constr_ctx -> 'a -> 'b) -> constr_ctx -> 'a located -> 'b located
            =
          fun f env {value; span} ->
            {value = f {env with expr_span = Some span} value; span}

        method! visit_expr env syntax_expr =
          let expr' = super#visit_expr env syntax_expr in
          let expr_dummy =
            {value = expr'; span = env.expr_span |> Option.value_exn}
          in
          match
            is_immediate_expr !current_bindings program expr_dummy
            && equal functions 0
          with
          | true ->
              let inter =
                s#make_interpreter (env.expr_span |> Option.value_exn)
              in
              let value' = inter#interpret_expr expr_dummy in
              Value value'
          | false ->
              expr'

        method build_binding _env name expr = (name, expr)

        method build_destructuring_binding _env destructuring_binding
            destructuring_let_expr destructuring_let_rest =
          { destructuring_let = Syntax.value destructuring_binding;
            destructuring_let_expr;
            destructuring_let_rest }

        method build_assignment _env assignment_ident assignment_expr =
          {assignment_ident; assignment_expr}

        method build_enum_definition _env _ _ _ = ()

        method build_enum_member _env _name _value = ()

        method build_field_access _env expr field =
          let mk_err () = s#report `Error (`FieldNotFoundF field) in
          match type_of program expr with
          | StructType s -> (
              let struct_ = Program.get_struct program s in
              match
                List.Assoc.find struct_.struct_fields field
                  ~equal:(equal_located equal_string)
              with
              | Some {field_type} ->
                  (expr, field, field_type)
              | None ->
                  mk_err () )
          | ExprType ex -> (
            match type_of program ex with
            | StructSig s -> (
                let s = Arena.get program.struct_signs s in
                match
                  List.Assoc.find s.st_sig_fields field
                    ~equal:(equal_located equal_string)
                with
                | Some ty ->
                    (expr, field, expr_to_type program ty)
                | None ->
                    mk_err () )
            | _ ->
                mk_err () )
          | _ ->
              mk_err ()

        method build_function_call _env fn args is_ty = (fn, args, is_ty)

        method build_method_call _env in_receiver fn args =
          let make_call receiver ~mk_args =
            match
              Program.methods_of program receiver
              |> fun ms ->
              List.find_map ms ~f:(fun (name, f) ->
                  if equal_string name.value fn.value then Some (name.span, f)
                  else None )
            with
            | Some (span, fn') ->
                FunctionCall
                  ( { value =
                        ResolvedReference
                          (fn, {value = Value (Function fn'); span});
                      span = fn.span },
                    mk_args args,
                    false (* FIXME: add is_type for methods *) )
            | None ->
                s#report `Error (`MethodNotFound (in_receiver, fn))
          in
          (* TODO: check method signatures *)
          match type_of program in_receiver with
          | TypeN 0 | Type0 _ ->
              make_call (expr_to_type program in_receiver) ~mk_args:(fun x -> x)
          | StructSig sign_id -> (
              let sign = Arena.get program.struct_signs sign_id in
              match
                List.Assoc.find sign.st_sig_methods fn
                  ~equal:(equal_located String.equal)
              with
              | Some m ->
                  StructSigMethodCall
                    { st_sig_call_instance = in_receiver;
                      st_sig_call_def = sign_id;
                      st_sig_call_method = (fn.value, m);
                      st_sig_call_args = args;
                      st_sig_call_kind = StructSigKind;
                      st_sig_call_span = fn.span }
              | None ->
                  s#report `Error (`MethodNotFound (in_receiver, fn)) )
          | UnionSig sign_id -> (
              let sign = Arena.get program.union_signs sign_id in
              match
                List.Assoc.find sign.un_sig_methods fn
                  ~equal:(equal_located String.equal)
              with
              | Some m ->
                  StructSigMethodCall
                    { st_sig_call_instance = in_receiver;
                      st_sig_call_def = sign_id;
                      st_sig_call_method = (fn.value, m);
                      st_sig_call_args = args;
                      st_sig_call_kind = UnionSigKind;
                      st_sig_call_span = fn.span }
              | None ->
                  s#report `Error (`MethodNotFound (in_receiver, fn)) )
          | InterfaceType intf_id -> (
              let intf = Program.get_intf program intf_id in
              match
                List.Assoc.find intf.interface_methods fn.value
                  ~equal:String.equal
              with
              | Some m ->
                  (*
                     Interface function can have signature with `SelfType` type
                     which is unknown at the interface definition point, but it should be
                     updated to the actual type when interface method was called.

                     Example:
                     ```
                      interface Intf { fn make() -> Self }
                      fn test(I: Intf) -> I {
                        let obj = I.make(); // Interface method has output type `SelfType`
                                            // which should be update to the `I` type.
                      }
                     ```
                  *)
                  let sign =
                    (new self_type_updater (ExprType in_receiver))
                      #visit_function_signature () m
                  in
                  IntfMethodCall
                    { intf_instance = in_receiver;
                      intf_def = intf_id;
                      intf_method = (fn.value, sign);
                      intf_args = args;
                      intf_loc = fn.span }
              | None ->
                  s#report `Error (`MethodNotFound (in_receiver, fn)) )
          | StructType st ->
              make_call (StructType st) ~mk_args:(fun args ->
                  in_receiver :: args )
          | UnionType ut ->
              make_call (UnionType ut) ~mk_args:(fun args ->
                  in_receiver :: args )
          | ExprType ex -> (
            (* If receiver has expr type that have type Interface, that means that
               value should implement interface, so we accept this case to allow
               such constructions:
               ```
                 fn foo(X: Intf) {
                  fn(arg: X) -> { arg.intf_method() }
                 }
               ```
               where
               type_of(arg) = ExprType(Reference("X"))
               type_of(Reference("X")) = Intf
            *)
            match type_of program ex with
            | InterfaceType intf_id -> (
                let intf = Program.get_intf program intf_id in
                match
                  List.Assoc.find intf.interface_methods fn.value
                    ~equal:String.equal
                with
                | Some m ->
                    IntfMethodCall
                      { intf_instance = ex;
                        intf_def = intf_id;
                        intf_method = (fn.value, m);
                        intf_args = in_receiver :: args;
                        intf_loc = fn.span }
                | None ->
                    s#report `Error (`MethodNotFound (in_receiver, fn)) )
            | StructSig sign_id -> (
                let sign = Arena.get program.struct_signs sign_id in
                match
                  List.Assoc.find sign.st_sig_methods fn
                    ~equal:(equal_located String.equal)
                with
                | Some m ->
                    let m =
                      (new self_ref_updater ex.value)#visit_function_signature
                        () m
                    in
                    StructSigMethodCall
                      { st_sig_call_instance = ex;
                        st_sig_call_def = sign_id;
                        st_sig_call_method = (fn.value, m);
                        st_sig_call_args = in_receiver :: args;
                        st_sig_call_kind = StructSigKind;
                        st_sig_call_span = fn.span }
                | None ->
                    s#report `Error (`MethodNotFound (in_receiver, fn)) )
            | UnionSig sign_id -> (
                let sign = Arena.get program.union_signs sign_id in
                match
                  List.Assoc.find sign.un_sig_methods fn
                    ~equal:(equal_located String.equal)
                with
                | Some m ->
                    StructSigMethodCall
                      { st_sig_call_instance = ex;
                        st_sig_call_def = sign_id;
                        st_sig_call_method = (fn.value, m);
                        st_sig_call_args = in_receiver :: args;
                        st_sig_call_kind = UnionSigKind;
                        st_sig_call_span = fn.span }
                | None ->
                    s#report `Error (`MethodNotFound (in_receiver, fn)) )
            | _ ->
                s#report `Error (`CannotHaveMethods (in_receiver, ExprType ex))
            )
          | receiver_ty ->
              s#report `Error (`CannotHaveMethods (in_receiver, receiver_ty))

        method! visit_function_definition env f =
          (* prepare parameter bindings *)
          let param_bindings : (string located * type_) list =
            s#of_located_list f.params
            |> List.map ~f:(fun (ident, expr) ->
                   ( s#visit_located s#visit_ident env ident,
                     s#visit_located s#visit_expr env expr ) )
            |> List.map ~f:(fun (id, expr) -> (id, expr_to_type program expr))
          in
          let function_returns =
            s#with_bindings
              (List.map param_bindings ~f:(fun (name, ty) ->
                   ( name,
                     Runtime
                       (ExprType {value = Reference (name, ty); span = name.span}
                       ) ) ) )
              (fun _ ->
                f.returns
                |> Option.map ~f:(fun x ->
                       expr_to_type program (s#visit_located s#visit_expr env x) )
                |> Option.value ~default:HoleType )
          in
          let body, fn_returns =
            s#with_bindings (List.map param_bindings ~f:make_runtime) (fun _ ->
                type_checker#with_fn_returns env function_returns (fun env' ->
                    s#visit_option s#visit_function_body env' f.function_body ) )
          in
          let function_attributes =
            s#visit_list s#visit_attribute env f.function_attributes
          in
          let function_signature =
            let sign =
              { value =
                  { function_attributes;
                    function_is_type = f.is_type_function;
                    function_params = param_bindings;
                    function_returns = fn_returns };
                span = f.function_def_span }
            in
            sign
          in
          { value =
              { function_signature;
                function_impl =
                  Fn
                    { value = Option.value body ~default:(Block []);
                      span = f.function_def_span } };
            span = f.function_def_span }

        method! visit_function_body env body =
          (* save the function enclosure count *)
          let functions' = functions in
          (* increment function counter *)
          functions <- functions + 1 ;
          let body =
            match body.function_stmt.value with
            | Expr ex ->
                {value = Syntax.Return ex; span = body.function_stmt.span}
            | _ ->
                body.function_stmt
          in
          (* process the body *)
          let result = super#visit_function_body env {function_stmt = body} in
          (* Convert implicit returns accomplished with an implicit last break *)
          let rec handle_returning_break stmt =
            match stmt.value with
            | Block block -> (
              match List.rev block with
              | [] ->
                  {value = Block []; span = stmt.span}
              | hd :: tl -> (
                match List.rev @@ (handle_returning_break hd :: tl) with
                | [stmt] ->
                    stmt
                | stmts ->
                    {value = Block stmts; span = stmt.span} ) )
            | Break {value = Expr expr; span} ->
                {value = Return expr; span}
            | Expr ex ->
                {value = Return ex; span = ex.span}
            | _ ->
                stmt
          in
          let result =
            handle_returning_break {value = result; span = body.span}
          in
          (* restore function enclosure count *)
          functions <- functions' ;
          result.value

        method build_function_body _env stmt = stmt.value

        method build_function_definition _ _ _ _ _ = unreachable ()

        method! visit_if_ env x =
          let new_env = {env with is_inside_condition_block = true} in
          let cond = s#visit_located s#visit_expr env x.condition in
          let body = s#visit_located s#visit_stmt new_env x.body in
          let else_ =
            s#visit_option (s#visit_located s#visit_stmt) new_env x.else_
          in
          s#build_if_ new_env cond body else_

        method build_if_ _env if_condition if_then if_else =
          match type_of program if_condition with
          | BoolType ->
              {if_condition; if_then; if_else}
          | _ ->
              errors#report `Error
                (`TypeError
                  (BoolType, type_of program if_condition, if_condition.span) )
                () ;
              {if_condition; if_then; if_else}

        method! visit_If env x =
          let visited = s#visit_if_ env x in
          s#build_If env visited

        method build_If _env if_ = If if_

        method build_while_loop _env while_cond while_body =
          match type_of program while_cond with
          | BoolType ->
              {while_cond; while_body}
          | _ ->
              errors#report `Error
                (`TypeError
                  (BoolType, type_of program while_cond, while_cond.span) )
                () ;
              {while_cond; while_body}

        method build_WhileLoop _env w = WhileLoop w

        method build_interface_definition _env attributes members =
          let signatures =
            List.filter_map (s#of_located_list members) ~f:(fun (name, x) ->
                match x.value with
                | Value (Function f) | MkFunction f ->
                    Some (name.value, f.value.function_signature)
                | _ ->
                    errors#report `Error (`OnlyFunctionIsAllowed name.span) () ;
                    None )
          in
          { mk_interface_attributes = attributes;
            mk_interface_methods = signatures }

        method! visit_interface_definition env def =
          let value =
            s#with_bindings
              [ make_comptime
                  ( builtin_located "Self",
                    builtin_located @@ Value (Type SelfType) ) ]
              (fun _ -> super#visit_interface_definition env def)
          in
          value

        method build_program _env _stmts = {program with bindings = s#bindings}

        method! visit_program env prog =
          let stmts =
            List.filter_map prog.stmts ~f:(fun stmt ->
                try Some (s#visit_located s#visit_stmt env stmt)
                with Skip -> None )
          in
          s#build_program env stmts

        method build_struct_constructor _env id fields =
          let check_fields st_ty fields_expected fields_actual =
            List.filter_map fields_expected
              ~f:(fun (n1, {field_type = expected_type}) ->
                match
                  List.find fields_actual ~f:(fun (n2, _) ->
                      String.equal n1.value n2.value )
                with
                | Some (_, actual_expr) -> (
                  match s#check_type ~expected:expected_type actual_expr with
                  | Ok _ ->
                      Some (n1, actual_expr)
                  | Error (NeedFromCall func) ->
                      let s = FunctionCall (func, [actual_expr], false) in
                      Some (n1, {value = s; span = actual_expr.span})
                  | _ ->
                      errors#report `Error
                        (`TypeError
                          ( expected_type,
                            type_of program actual_expr,
                            actual_expr.span ) )
                        () ;
                      None )
                | None ->
                    errors#report `Error (`MissingField (st_ty, n1, n1.span)) () ;
                    None )
          in
          match id.value with
          | ResolvedReference
              (_, ({value = Value (Type (StructType s as st_ty)); _} as ty)) ->
              let struct_ = Program.get_struct program s in
              let fields =
                check_fields st_ty struct_.struct_fields fields
                |> List.map ~f:(fun (name, expr) -> (name.value, expr))
              in
              (ty, fields)
          | Value (Type (StructType s as st_ty)) ->
              let struct_ = Program.get_struct program s in
              let fields =
                check_fields st_ty struct_.struct_fields fields
                |> List.map ~f:(fun (name, expr) -> (name.value, expr))
              in
              (id, fields)
          | _ -> (
            match type_of program id with
            | StructSig sign ->
                let struct_sign = Arena.get program.struct_signs sign in
                let expected_fields =
                  List.Assoc.map struct_sign.st_sig_fields ~f:(fun x ->
                      {field_type = expr_to_type program x} )
                in
                let fields =
                  check_fields (ExprType id) expected_fields fields
                  |> List.map ~f:(fun (name, expr) -> (name.value, expr))
                in
                (id, fields)
            | _ ->
                s#report `Error (`IsNotStruct id) )

        method build_struct_definition _ _ _ _ = unreachable ()

        method make_struct_definition
            : attribute list ->
              (string located * expr) list ->
              (string located * expr) list ->
              _ =
          fun attributes struct_fields bindings impls mk_struct_id sign_id span ->
            let mk_struct_fields = struct_fields in
            let mk_methods =
              List.filter_map bindings ~f:(fun binding ->
                  let name, expr = binding in
                  match expr.value with
                  | Value (Function _) | MkFunction _ ->
                      Some (name, expr)
                  | _ ->
                      None )
            in
            let impl_methods =
              List.concat
                (List.map impls ~f:(fun impl ->
                     List.filter_map impl.mk_impl_methods ~f:(fun (name, ex) ->
                         match ex.value with
                         | Value (Function _) | MkFunction _ ->
                             Some (name, ex)
                         | _ ->
                             None ) ) )
            in
            let _ =
              Arena.update program.struct_signs sign_id ~f:(fun sign ->
                  { st_sig_attributes = attributes;
                    st_sig_fields = mk_struct_fields;
                    st_sig_methods =
                      List.Assoc.map (mk_methods @ impl_methods) ~f:(fun f ->
                          match f.value with
                          | Value (Function f) | MkFunction f ->
                              f.value.function_signature
                          | _ ->
                              unreachable () );
                    st_sig_base_id = mk_struct_id;
                    st_sig_id = sign.st_sig_id } )
            in
            let s' =
              { mk_struct_attributes = attributes;
                mk_struct_fields;
                mk_struct_details =
                  { mk_methods = mk_methods @ impl_methods;
                    mk_impls = impls;
                    mk_id = mk_struct_id;
                    mk_sig = sign_id;
                    mk_span = span } }
            in
            (* Check for duplicate fields *)
            ( match
                List.find_a_dup mk_struct_fields
                  ~compare:(fun (name1, _) (name2, _) ->
                    String.compare name1.value name2.value )
              with
            | Some (name, _) ->
                errors#report `Error (`DuplicateField (name, s')) ()
            | None ->
                () ) ;
            s'

        method! visit_struct_definition env syn_struct_def =
          let prev_functions = functions in
          functions <- functions + 1 ;
          let fields =
            s#visit_list
              (s#visit_located s#visit_struct_field)
              env syn_struct_def.fields
            |> s#of_located_list
          in
          let attributes =
            s#visit_list s#visit_attribute env syn_struct_def.struct_attributes
          in
          let sign_id, _ =
            Arena.with_id program.struct_signs ~f:(fun id ->
                { st_sig_attributes = attributes;
                  st_sig_fields = fields;
                  st_sig_methods = [];
                  st_sig_base_id = program.type_counter;
                  st_sig_id = id } )
          in
          let mk_id = program.type_counter in
          program.type_counter <- program.type_counter + 1 ;
          let mk_struct =
            let self_name =
              {value = "Self"; span = syn_struct_def.struct_span}
            in
            let methods =
              s#with_bindings
                [make_runtime (self_name, StructSig sign_id)]
                (fun _ ->
                  List.fold syn_struct_def.struct_bindings ~init:[]
                    ~f:(fun methods item ->
                      let name, func = s#visit_binding env item.value in
                      let func' =
                        match func.value with
                        | Value (Function f) | MkFunction f ->
                            f
                        | _ ->
                            ice "Non-function when function expected"
                      in
                      let _ =
                        Arena.update program.struct_signs sign_id
                          ~f:(fun item ->
                            { item with
                              st_sig_methods =
                                (name, func'.value.function_signature)
                                :: item.st_sig_methods } )
                      in
                      (name, func) :: methods ) )
            in
            let self_ty =
              ExprType
                { value = Reference (self_name, StructSig sign_id);
                  span = syn_struct_def.struct_span }
            in
            let impls =
              s#with_bindings
                [make_runtime (self_name, StructSig sign_id)]
                (fun _ -> s#visit_list s#visit_impl env syn_struct_def.impls)
              |> List.map ~f:(s#execute_impl_attrs self_ty)
            in
            let mk_struct =
              s#make_struct_definition attributes fields methods impls mk_id
                sign_id syn_struct_def.struct_span
            in
            { mk_struct with
              mk_struct_details =
                { mk_struct.mk_struct_details with
                  mk_id = mk_struct.mk_struct_details.mk_id } }
          in
          functions <- prev_functions ;
          mk_struct

        method build_struct_field : _ -> _ -> _ -> _ -> string located * expr =
          fun _env _attributes field_name field_type -> (field_name, field_type)

        method build_union_definition _ _ _ = unreachable ()

        method! visit_union_definition env def =
          let prev_functions = functions in
          functions <- functions + 1 ;
          let cases =
            s#visit_list (s#visit_located s#visit_expr) env def.union_members
          in
          let attributes =
            s#visit_list s#visit_attribute env def.union_attributes
          in
          let union_base_id = program.type_counter in
          program.type_counter <- program.type_counter + 1 ;
          let sign_id, _ =
            Arena.with_id program.union_signs ~f:(fun _ ->
                { un_sig_attributes = attributes;
                  un_sig_cases = List.map cases ~f:(expr_to_type program);
                  un_sig_methods = [];
                  un_sig_base_id = union_base_id } )
          in
          let self_name = {value = "Self"; span = def.union_span} in
          let methods =
            s#with_bindings
              [make_runtime (self_name, UnionSig sign_id)]
              (fun _ ->
                List.fold def.union_bindings ~init:[] ~f:(fun methods item ->
                    let name, func = s#visit_binding env item.value in
                    let func' =
                      match func.value with
                      | Value (Function f) | MkFunction f ->
                          f
                      | _ ->
                          ice "Non-function when function expected"
                    in
                    let _ =
                      Arena.update program.union_signs sign_id ~f:(fun item ->
                          { item with
                            un_sig_methods =
                              (name, func'.value.function_signature)
                              :: item.un_sig_methods } )
                    in
                    (name, func) :: methods ) )
            |> List.map ~f:(fun (name, e) ->
                   match e.value with
                   | Value (Function _) | MkFunction _ ->
                       (name, e)
                   | _ ->
                       unreachable () )
          in
          let self_ty =
            ExprType
              { value = Reference (self_name, UnionSig sign_id);
                span = def.union_span }
          in
          let impls =
            s#with_bindings
              [make_runtime (self_name, UnionSig sign_id)]
              (fun _ -> s#visit_list s#visit_impl env def.union_impls)
            |> List.map ~f:(s#execute_impl_attrs self_ty)
          in
          let impl_methods =
            List.concat
              (List.map impls ~f:(fun impl ->
                   List.filter_map impl.mk_impl_methods ~f:(fun (name, ex) ->
                       match ex.value with
                       | Value (Function _) | MkFunction _ ->
                           Some (name, ex)
                       | _ ->
                           None ) ) )
          in
          let convert_impls = s#make_from_impls cases union_base_id in
          let mk_union =
            { mk_union_attributes = attributes;
              mk_cases = cases;
              mk_union_details =
                { mk_id = union_base_id;
                  mk_impls = impls @ convert_impls;
                  mk_methods = methods @ impl_methods;
                  mk_sig = sign_id;
                  mk_span = def.union_span } }
          in
          functions <- prev_functions ;
          mk_union

        method private execute_impl_attrs self_ty impl =
          let remove_attr attr impl =
            let new_attrs =
              List.filter impl.mk_impl_attributes ~f:(fun attr2 ->
                  not
                    (String.equal attr.attribute_ident.value
                       attr2.attribute_ident.value ) )
            in
            {impl with mk_impl_attributes = new_attrs}
          in
          List.fold impl.mk_impl_attributes ~init:impl ~f:(fun impl attr ->
              let impl_after_attr =
                match
                  List.Assoc.find program.attr_executors
                    attr.attribute_ident.value ~equal:equal_string
                with
                | Some executor -> (
                    let x =
                      executor program !current_bindings attr.attribute_exprs
                        (ImplAttrTarget {impl; self_ty})
                    in
                    match x with
                    | ImplAttrTarget {impl; _} ->
                        impl |> remove_attr attr )
                | None ->
                    impl
              in
              impl_after_attr )

        method bindings =
          extract_comptime_bindings (List.concat !current_bindings)

        method make_interpreter call_span =
          new interpreter
            (make_ctx program current_bindings functions)
            errors call_span s#partial_evaluate_fn

        method private of_located_list : 'a. 'a Syntax.located list -> 'a list =
          List.map ~f:Syntax.value

        method private check_type ~expected actual =
          type_checker#check_type ~program ~current_bindings ~expected actual

        method private with_bindings : 'a. tbinding list -> (unit -> 'a) -> 'a =
          fun added_bindings f ->
            current_bindings := added_bindings :: !current_bindings ;
            let result = f () in
            (* We can mutate bindings so we should just pop last scope *)
            current_bindings := List.tl_exn !current_bindings ;
            result

        method private make_from_impls : expr list -> int -> mk_impl list =
          fun cases union ->
            List.map cases ~f:(fun case ->
                let from_intf_ =
                  { value =
                      FunctionCall
                        ( from_intf,
                          [ { value = Value (Type (ExprType case));
                              span = case.span } ],
                          false );
                    span = case.span }
                in
                { mk_impl_attributes = [];
                  mk_impl_interface = from_intf_;
                  mk_impl_methods =
                    [ ( {value = "from"; span = case.span},
                        s#make_from_impl_fn case union ) ] } )

        method private make_from_impl_fn case union =
          { value =
              Value
                (Function
                   { value =
                       { function_signature =
                           { value =
                               { function_attributes = [];
                                 function_is_type = false;
                                 function_params =
                                   [ ( builtin_located "v",
                                       expr_to_type program case ) ];
                                 function_returns = UnionType union };
                             span = case.span };
                         function_impl =
                           Fn
                             (builtin_located
                                (Return
                                   ( builtin_located
                                   @@ MakeUnionVariant
                                        ( builtin_located
                                          @@ Reference
                                               ( builtin_located "v",
                                                 expr_to_type program case ),
                                          union ) ) ) ) };
                     span = case.span } );
            span = case.span }

        method private partial_evaluate_fn ctx f =
          let partial_evaluator = new partial_evaluator ctx errors in
          partial_evaluator#visit_function_ () f

        method visit_list_filter_skipped
            : 'a 'env 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
          fun f env xs ->
            List.filter_map xs ~f:(fun x ->
                try Some (f env x) with Skip -> None )

        method private report : 'a. _ -> _ -> 'a =
          fun severity ty ->
            errors#report severity ty () ;
            raise Skip
      end
  end

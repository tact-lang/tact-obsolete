open Base
open Errors

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Config

    open Interpreter.Make (Config)

    open Lang_types.Make (Config)

    class ['s] partial_evaluator (ctx : ctx) (errors : _) =
      object (self : 's)
        inherit [_] map as super

        (* Make a deep copy of the ctx. Local values should not flow into
           code that call partial_evaluator. *)
        val ctx = {ctx with program = ctx.program}

        method! visit_InvalidType _ ex =
          print_sexp (sexp_of_string "invalid type") ;
          print_sexp (sexp_of_expr ex) ;
          raise InternalCompilerError

        method! visit_Reference env (ref, ty) =
          match find_in_scope ref.value ctx.scope with
          | Some (Comptime ex) ->
              Value
                (self#with_interpreter env (fun inter ->
                     inter#interpret_expr ex ) )
          | Some (Runtime _) ->
              Reference (ref, self#visit_type_ env ty)
          | None ->
              print_sexp (sexp_of_string ref.value) ;
              print_sexp
                (sexp_of_list (sexp_of_list sexp_of_tbinding) ctx.scope) ;
              raise InternalCompilerError

        method! visit_type_ env ty =
          let ty = super#visit_type_ env ty in
          if
            is_immediate_expr ctx.scope ctx.program
              (builtin_located @@ Value (Type ty))
          then self#with_interpreter env (fun inter -> inter#interpret_type ty)
          else self#unwrap_expr_types ty

        method private unwrap_expr_types =
          function
          | ExprType {value = Value (Type t); _}
          | ExprType
              {value = ResolvedReference (_, {value = Value (Type t); _}); _} ->
              self#unwrap_expr_types t
          | t ->
              t

        method! visit_Block env b =
          self#with_vars [] (fun _ -> super#visit_Block env b)

        method! visit_Let env vars =
          (* TODO: this won't work if `vars` will be actually a list. *)
          let vars' =
            self#visit_list
              (fun env (name, ex) -> (name, self#visit_expr env ex))
              env vars
          in
          let vars_scope =
            List.map vars' ~f:(fun (name, ex) ->
                (name, Runtime (type_of ctx.program ex)) )
          in
          ctx.scope <- vars_scope :: ctx.scope ;
          Let vars'

        method! visit_DestructuringLet _env let_ =
          match type_of ctx.program let_.destructuring_let_expr with
          | StructType id ->
              let struct_ = Program.get_struct ctx.program id in
              (* Check if field names are correct *)
              List.iter let_.destructuring_let ~f:(fun (name, name2) ->
                  if
                    List.Assoc.find struct_.struct_fields
                      ~equal:(equal_located equal_string)
                      name
                    |> Option.is_some
                  then ()
                  else
                    errors#report `Error
                      (`FieldNotFound
                        ( { value = Value (Type (StructType id));
                            span = name2.span },
                          name ) )
                      () ) ;
              (* If rest of fields are not ignored, check for completeness *)
              if let_.destructuring_let_rest then ()
              else
                List.iter struct_.struct_fields ~f:(fun (name, _) ->
                    if
                      List.Assoc.find let_.destructuring_let
                        ~equal:(equal_located String.equal)
                        name
                      |> Option.is_some
                    then ()
                    else errors#report `Error (`MissingField (id, name)) () ) ;
              let vars =
                List.map let_.destructuring_let ~f:(fun (name, new_name) ->
                    List.Assoc.find struct_.struct_fields
                      ~equal:(equal_located String.equal)
                      name
                    |> Option.value_exn
                    |> fun {field_type} -> (new_name, Runtime field_type) )
              in
              ctx.scope <- vars :: ctx.scope ;
              DestructuringLet let_
          | _ ->
              raise InternalCompilerError

        method! visit_switch env switch =
          let cond = self#visit_expr env switch.switch_condition in
          let branches =
            List.map switch.branches
              ~f:(fun {value = {branch_var; branch_ty; branch_stmt}; span} ->
                let stmt =
                  self#with_vars
                    [(branch_var, Runtime branch_ty)]
                    (fun _ -> self#visit_stmt env branch_stmt)
                in
                {value = {branch_var; branch_ty; branch_stmt = stmt}; span} )
          in
          {switch_condition = cond; branches}

        method! visit_function_ env f =
          let sign =
            self#visit_function_signature env f.value.function_signature
          in
          ctx.functions <- ctx.functions + 1 ;
          let args =
            List.map sign.value.function_params ~f:(fun (name, ty) ->
                (name, Runtime ty) )
          in
          let out =
            self#with_vars args (fun _ ->
                let body = self#visit_function_impl env f.value.function_impl in
                {function_signature = sign; function_impl = body} )
          in
          ctx.functions <- ctx.functions - 1 ;
          {value = out; span = f.span}

        method! visit_IntfMethodCall env call =
          let intf_instance = self#visit_expr env call.intf_instance in
          let args = self#visit_list self#visit_expr env call.intf_args in
          let is_dependent = function
            | Value (Type (Dependent _)) ->
                true
            | _ ->
                false
          in
          match
            is_immediate_expr ctx.scope ctx.program intf_instance
            && not (is_dependent intf_instance.value)
          with
          | true -> (
              let intf_ty =
                match
                  self#with_interpreter env (fun inter ->
                      inter#interpret_expr intf_instance )
                with
                | Type t ->
                    t
                | _ ->
                    raise InternalCompilerError
              in
              match
                Program.find_impl_intf ctx.program call.intf_def intf_ty
              with
              | Some impl ->
                  let method_ =
                    List.find_map_exn impl.impl_methods ~f:(fun (name, impl) ->
                        let method_name, _ = call.intf_method in
                        if equal_string name.value method_name then Some impl
                        else None )
                  in
                  FunctionCall
                    ( {value = Value (Function method_); span = call.intf_loc},
                      args )
              | None ->
                  raise InternalCompilerError )
          | false ->
              IntfMethodCall {call with intf_instance; intf_args = args}

        val mutable visited_signs : (int * int) list = []

        method! visit_StructSig _ sign_id =
          match List.Assoc.find ctx.updated_items sign_id ~equal:equal_int with
          | Some new_id ->
              StructType new_id
          | None ->
              StructSig sign_id

        method! visit_mk_struct env mk =
          let mk =
            self#with_vars
              [ make_runtime
                  ( {value = "Self"; span = mk.mk_struct_details.mk_span},
                    StructSig mk.mk_struct_details.mk_sig ) ]
              (fun _ -> super#visit_mk_struct env mk)
          in
          mk

        method! visit_UnionSig _ sign_id =
          match List.Assoc.find ctx.updated_unions sign_id ~equal:equal_int with
          | Some new_id ->
              UnionType new_id
          | None ->
              UnionSig sign_id

        method! visit_mk_union env mk =
          let mk =
            self#with_vars
              [ make_runtime
                  ( {value = "Self"; span = mk.mk_union_details.mk_span},
                    UnionSig mk.mk_union_details.mk_sig ) ]
              (fun _ -> super#visit_mk_union env mk)
          in
          mk

        method! visit_MakeUnionVariant env (expr, uid) =
          let expr = self#visit_expr env expr in
          let new_id =
            match List.Assoc.find ctx.updated_items uid ~equal:equal_int with
            | Some new_id ->
                new_id
            | None ->
                uid
          in
          MakeUnionVariant (expr, new_id)

        method! visit_UnionVariant env (expr, uid) =
          let expr = self#visit_value env expr in
          let new_id =
            match List.Assoc.find ctx.updated_items uid ~equal:equal_int with
            | Some new_id ->
                new_id
            | None ->
                uid
          in
          UnionVariant (expr, new_id)

        method! visit_FunctionCall env (f, args) =
          let f = self#visit_expr env f in
          let args = self#visit_list self#visit_expr env args in
          if
            is_immediate_expr ctx.scope ctx.program
              {value = FunctionCall (f, args); span = f.span}
          then
            Value
              (self#with_interpreter env (fun inter ->
                   inter#interpret_fc (f, args) ) )
          else FunctionCall (f, args)

        method! visit_StructSigMethodCall env call =
          let st_sig_instance = self#visit_expr env call.st_sig_call_instance in
          let args =
            self#visit_list self#visit_expr env call.st_sig_call_args
          in
          let method_name, m_temp = call.st_sig_call_method in
          let visited_method_ = self#visit_function_signature env m_temp in
          let is_dependent = function
            | Value (Type (Dependent _)) ->
                true
            | _ ->
                false
          in
          match
            is_immediate_expr ctx.scope ctx.program st_sig_instance
            && not (is_dependent st_sig_instance.value)
          with
          | true ->
              let st_sig_ty =
                match
                  self#with_interpreter env (fun inter ->
                      inter#interpret_expr st_sig_instance )
                with
                | Type t ->
                    t
                | _ ->
                    raise InternalCompilerError
              in
              let methods = Program.methods_of ctx.program st_sig_ty in
              let method_ =
                List.find_map_exn methods ~f:(fun (name, fn) ->
                    if equal_string name.value method_name then Some fn
                    else None )
              in
              FunctionCall
                ( { value = Value (Function method_);
                    span = call.st_sig_call_span },
                  args )
          | false ->
              StructSigMethodCall
                { call with
                  st_sig_call_instance = st_sig_instance;
                  st_sig_call_args = args;
                  st_sig_call_method = (method_name, visited_method_) }

        method private with_vars : 'a. _ -> (unit -> 'a) -> 'a =
          fun vars f ->
            let prev_vars = ctx.scope in
            ctx.scope <- vars :: ctx.scope ;
            let out = f () in
            ctx.scope <- prev_vars ;
            out

        (* FIXME: This function should create new instance of the partial_evaluator
           and call new_instance#visit_function_ but there is some problems with
           generics I can not solve yet. *)
        method private with_interpreter
            : 'env 'a. 'env -> (interpreter -> 'a) -> 'a =
          fun _env f ->
            let inter = new interpreter ctx errors (fun _ f -> f) in
            f inter
      end
  end

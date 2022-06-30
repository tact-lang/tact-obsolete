open Base
open Lang_types
open Errors

type error =
  [ `UnresolvedIdentifier of string
  | `UninterpretableStatement of stmt
  | `UnexpectedType of type_
  | `FieldNotFound of int * string
  | `MissingField of int * string
  | `ArgumentNumberMismatch
  | `DuplicateVariant of type_ ]
[@@deriving equal, sexp_of]

class ['s] struct_updater (program : program) (old : int) (new_s : int) =
  object (self : 's)
    inherit ['s] map

    val mutable visited_structs = []

    method! visit_StructType env s =
      if equal_int s old then StructType new_s
      else if not (List.exists visited_structs ~f:(fun s' -> equal_int s' s))
      then
        let _ = self#update_struct_ids_if_needed env s in
        StructType s
      else StructType s

    (* This will update struct ids in the fields and functions of another struct.
       Suppose, you have following declaration:
       ```tact
       struct Wrap(X: Type) {val x: X}
       struct /* 3 */ Test {
        fn wrap(self: Self) -> /* 2 */ Wrap( /* 1 */ Self) {
          Wrap(Self) { x: self }
        }
       }
       ```
       If code will evaluated without function below, output code has IDs as in comments above.
       As you can see, `Wrap(Struct(1))` will be created with field `val x: Struct(1)`, but it
       is expected that after `Test` was constructed, `val x` field should have struct id `3`.
       So, to update such ids there is a function below.
    *)
    method private update_struct_ids_if_needed : 'env. 'env -> _ -> _ =
      fun env sid ->
        visited_structs <- sid :: visited_structs ;
        Program.update_struct program sid ~f:(fun st ->
            self#visit_struct_ env st )

    method! visit_UnionType env u =
      if not (List.exists visited_structs ~f:(fun s' -> equal_int s' u)) then (
        visited_structs <- u :: visited_structs ;
        let _ =
          Program.update_union program u ~f:(fun st -> self#visit_union env st)
        in
        UnionType u )
      else UnionType u

    method! visit_Struct env (s, fields) =
      let fields' = self#visit_list self#visit_binding env fields in
      if equal_int s old then Struct (new_s, fields') else Struct (s, fields')
  end

class ['s] union_updater (program : program) (old : int) (new_s : int) =
  object (self : 's)
    inherit ['s] map

    val mutable visited_structs = []

    method! visit_UnionType env s =
      if equal_int s old then UnionType new_s else self#update_unions env s

    method update_unions : 'env. 'env -> _ -> _ =
      fun env u ->
        if not (List.exists visited_structs ~f:(fun s' -> equal_int s' u)) then (
          visited_structs <- u :: visited_structs ;
          let _ =
            Program.update_union program u ~f:(fun st ->
                self#visit_union env st )
          in
          () ) ;
        UnionType u

    method! visit_StructType env s =
      if not (List.exists visited_structs ~f:(fun s' -> equal_int s' s)) then (
        visited_structs <- s :: visited_structs ;
        let _ =
          Program.update_struct program s ~f:(fun st ->
              self#visit_struct_ env st )
        in
        () ) ;
      StructType s

    method! visit_UnionVariant _ (expr, s) =
      if equal_int s old then UnionVariant (expr, new_s)
      else UnionVariant (expr, s)

    method! visit_MakeUnionVariant _ (expr, s) =
      if equal_int s old then MakeUnionVariant (expr, new_s)
      else MakeUnionVariant (expr, s)
  end

let get_memoized_or_execute p (f, args) ~execute =
  match
    List.Assoc.find p.memoized_fcalls (f, args)
      ~equal:(fun (f1, args1) (f2, args2) ->
        equal_value f1 f2 && equal_list equal_value args1 args2 )
  with
  | Some v ->
      v
  | None ->
      let res = execute f args in
      p.memoized_fcalls <- ((f, args), res) :: p.memoized_fcalls ;
      res

(*TODO: type checks for arguments*)
class interpreter
  ((program, bindings, errors, _functions) :
    program * tbinding list list * _ errors * int ) =
  object (self)
    val global_bindings = bindings

    val mutable vars_scope = []

    val mutable return = Void

    val mutable adding_structs = []

    val mutable errors = errors

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
          match List.zip (List.map binds ~f:(fun (name, _) -> name)) values with
          | Ok args_scope ->
              let prev_scope = vars_scope in
              vars_scope <- args_scope :: vars_scope ;
              let output = self#interpret_stmt_list rest in
              vars_scope <- prev_scope ;
              output
          | _ ->
              errors#report `Error `ArgumentNumberMismatch () ;
              Void )
      | DestructuringLet let_ ->
          let struct_expr = self#interpret_expr let_.destructuring_let_expr in
          let args_scope =
            List.map let_.destructuring_let ~f:(fun (name, new_name) ->
                let expr = StructField (Value struct_expr, name, HoleType) in
                (new_name, self#interpret_expr expr) )
          in
          let prev_scope = vars_scope in
          vars_scope <- args_scope :: vars_scope ;
          let output = self#interpret_stmt_list rest in
          vars_scope <- prev_scope ;
          output
      | Break stmt ->
          self#interpret_stmt stmt []
      | Return expr ->
          self#interpret_expr expr
      | Expr expr ->
          let expr' = self#interpret_expr expr in
          return <- expr' ;
          self#interpret_stmt_list rest
      | Block stmts ->
          self#interpret_stmt_list stmts
      | If {if_condition; if_then; if_else} -> (
          let result = self#interpret_expr if_condition in
          match result with
          | Bool true ->
              self#interpret_stmt if_then []
          | Bool false ->
              Option.map if_else ~f:(fun stmt -> self#interpret_stmt stmt [])
              |> Option.value ~default:Void
          | value ->
              errors#report `Error (`UnexpectedType (type_of (Value value))) () ;
              Void )
      | Switch {switch_condition; branches} -> (
          let cond = self#interpret_expr switch_condition in
          match cond with
          | UnionVariant (v, _) -> (
              let v_ty = type_of (Value v) in
              let correct_branch =
                List.find branches ~f:(fun b -> equal_type_ b.branch_ty v_ty)
              in
              match correct_branch with
              | Some branch ->
                  let prev_scope = vars_scope in
                  vars_scope <- [(branch.branch_var, v)] :: vars_scope ;
                  let res = self#interpret_stmt branch.branch_stmt [] in
                  vars_scope <- prev_scope ;
                  res
              | None ->
                  Void )
          | _ ->
              Void )
      | Invalid ->
          Void

    method interpret_expr : expr -> value =
      fun expr ->
        match expr with
        | FunctionCall fc ->
            self#interpret_fc fc
        | IntfMethodCall
            {intf_instance; intf_def; intf_method = method_name, _ty; intf_args}
          -> (
            let ty =
              match self#interpret_expr intf_instance with
              | Type t ->
                  t
              | _ ->
                  raise InternalCompilerError
            in
            match Program.find_impl_intf program intf_def ty with
            | Some impl ->
                let method_ =
                  List.find_map_exn impl.impl_methods ~f:(fun (name, impl) ->
                      if equal_string name method_name then Some impl else None )
                in
                self#interpret_fc (method_, intf_args)
            | None ->
                raise InternalCompilerError )
        | ResolvedReference (_, expr') ->
            self#interpret_expr expr'
        | Reference (name, _) -> (
          match self#find_ref name with
          | Some expr' ->
              self#interpret_expr expr'
          | None ->
              errors#report `Error (`UnresolvedIdentifier name) () ;
              Void )
        | StructField (struct_, field, _) -> (
          match self#interpret_expr struct_ with
          | Struct (struct_, struct') -> (
            match List.Assoc.find struct' ~equal:String.equal field with
            | Some field ->
                self#interpret_expr field
            | None ->
                errors#report `Error (`FieldNotFound (struct_, field)) () ;
                Void )
          | other ->
              errors#report `Error (`UnexpectedType (type_of (Value other))) () ;
              Void )
        | Value value ->
            self#interpret_value value
        | MakeUnionVariant (expr, union) ->
            (* We assume that input of interpreter is always type-checked,
               so we do not need to check if union can be built from the expr. *)
            let data = self#interpret_expr expr in
            UnionVariant (data, union)
        | MkStructDef {mk_struct_fields; mk_methods; mk_impls; mk_struct_id} ->
            let struct_fields =
              List.map mk_struct_fields ~f:(fun (name, field_type) ->
                  ( name,
                    { field_type =
                        expr_to_type (Value (self#interpret_expr field_type)) }
                  ) )
            in
            let struct_id = program.type_counter in
            program.type_counter <- program.type_counter + 1 ;
            let struct_ty = struct_id in
            let struct_ =
              { struct_fields;
                struct_methods = [];
                struct_impls = [];
                struct_id;
                tensor = false }
            in
            let struct_updater =
              new struct_updater program mk_struct_id struct_id
            in
            let s =
              Program.with_struct program struct_ (fun _ ->
                  let struct_methods =
                    List.map mk_methods ~f:(fun (name, fn) ->
                        let prev_scope = vars_scope in
                        vars_scope <-
                          [("Self", Type (StructType struct_ty))] :: vars_scope ;
                        let output =
                          match
                            self#interpret_expr
                              (struct_updater#visit_expr () fn)
                          with
                          | Function f ->
                              f
                          | _ ->
                              raise InternalCompilerError
                        in
                        vars_scope <- prev_scope ;
                        (name, output) )
                  in
                  let struct_impls =
                    List.map mk_impls ~f:(fun impl ->
                        { impl_interface =
                            Value (self#interpret_expr impl.impl_interface);
                          impl_methods =
                            List.map impl.impl_methods ~f:(fun (n, x) ->
                                ( n,
                                  Value
                                    (self#interpret_expr
                                       (struct_updater#visit_expr () x) ) ) ) } )
                  in
                  Ok
                    { struct_fields;
                      struct_methods;
                      struct_impls;
                      struct_id;
                      tensor = false } )
            in
            let _ =
              match s with Ok t -> t | Error _ -> raise InternalCompilerError
            in
            Type (StructType struct_ty)
        | MkUnionDef mk_union ->
            let compose f g x = g (f x) in
            let cases =
              List.map mk_union.mk_cases
                ~f:
                  (compose self#interpret_expr (fun x -> expr_to_type (Value x)))
              |> self#check_unions_for_doubled_types
            in
            let union =
              Program.with_union_id program
                (fun id ->
                  { cases =
                      Discriminator.LocalDiscriminators.choose_discriminators ()
                        id cases;
                    union_methods = [];
                    union_impls = [];
                    union_id = id } )
                (fun u_base ->
                  let union_updater =
                    new union_updater
                      program mk_union.mk_union_id u_base.union_id
                  in
                  let union_methods =
                    List.map mk_union.mk_union_methods ~f:(fun (name, fn) ->
                        let prev_scope = vars_scope in
                        vars_scope <-
                          [("Self", Type (UnionType u_base.union_id))]
                          :: vars_scope ;
                        let output =
                          match
                            self#interpret_expr (union_updater#visit_expr () fn)
                          with
                          | Function f ->
                              f
                          | _ ->
                              raise InternalCompilerError
                        in
                        vars_scope <- prev_scope ;
                        (name, output) )
                  in
                  let union_impls =
                    List.map mk_union.mk_union_impls ~f:(fun impl ->
                        { impl_interface =
                            Value (self#interpret_expr impl.impl_interface);
                          impl_methods =
                            List.map impl.impl_methods ~f:(fun (n, x) ->
                                ( n,
                                  Value
                                    (self#interpret_expr
                                       (union_updater#visit_expr () x) ) ) ) } )
                  in
                  Ok
                    { cases = u_base.cases;
                      union_methods;
                      union_impls;
                      union_id = u_base.union_id } )
              |> Result.ok_exn
            in
            Type (UnionType union.union_id)
        | MkInterfaceDef {mk_interface_methods} ->
            let intf =
              { interface_methods =
                  List.map mk_interface_methods ~f:(fun (name, sign) ->
                      ( name,
                        { function_params =
                            List.map sign.function_params ~f:(fun (pname, ty) ->
                                (pname, self#interpret_type ty) );
                          function_returns =
                            self#interpret_type sign.function_returns } ) ) }
            in
            let intf_ty = Program.insert_interface program intf in
            Type intf_ty
        | MkFunction f ->
            Function (self#interpret_function f)
        | Primitive _ | InvalidExpr | Hole ->
            errors#report `Error (`UninterpretableStatement (Expr expr)) () ;
            Void

    method interpret_type : type_ -> type_ =
      function
      | ExprType ex -> (
        match self#interpret_expr ex with
        | Type t ->
            t
        | _ ->
            raise InternalCompilerError )
      | ty ->
          ty

    (* TBD: previously we defined value as "atom" which cannot be interpreted, but below
       we interpret values. Should we move `Type`, `Struct` and `Function` to the `expr` type?*)
    method interpret_value : value -> value =
      fun value ->
        match value with
        | Type ty ->
            Type (self#interpret_type ty)
        | Struct (s, fields) ->
            Struct
              ( s,
                List.map fields ~f:(fun (n, f) ->
                    (n, Value (self#interpret_expr f)) ) )
        | value ->
            value

    method interpret_function : function_ -> function_ =
      fun f -> self#partial_evaluate_function f

    method interpret_fc : function_call -> value =
      fun (func, args) ->
        let f = self#interpret_expr func in
        let args' = List.map args ~f:(fun arg -> self#interpret_expr arg) in
        let mk_err = Expr (FunctionCall (func, args)) in
        let args_to_list params values =
          match
            List.zip (List.map params ~f:(fun (name, _) -> name)) values
          with
          | Ok scope ->
              Ok scope
          | _ ->
              Error mk_err
        in
        get_memoized_or_execute program (f, args') ~execute:(fun f args' ->
            match f with
            | Function f -> (
              match f with
              | { function_signature = {function_params; _};
                  function_impl = Fn function_impl;
                  _ } -> (
                  let args_scope = args_to_list function_params args' in
                  match args_scope with
                  | Ok args_scope -> (
                    match function_impl with
                    | Some body ->
                        let prev_scope = vars_scope in
                        vars_scope <- args_scope :: vars_scope ;
                        let output = self#interpret_stmt body [] in
                        vars_scope <- prev_scope ;
                        output
                    | None ->
                        Void )
                  | Error _ ->
                      Void )
              | {function_impl = BuiltinFn (function_impl, _); _} ->
                  let value = function_impl program args' in
                  value
              | _ ->
                  Void )
            | _ ->
                Void )

    method private find_ref : string -> expr option =
      fun ref ->
        match find_in_runtime_scope ref vars_scope with
        | Some e ->
            Some (Value e)
        | None ->
            self#find_in_global_scope ref

    method private find_in_global_scope : string -> expr option =
      fun ref ->
        match find_comptime ref global_bindings with
        | Some (Ok (Reference (ref', _))) ->
            self#find_in_global_scope ref'
        | Some (Ok (ResolvedReference (_, e))) ->
            Some e
        | Some (Error ()) ->
            Sexplib.Sexp.pp_hum Caml.Format.std_formatter (sexp_of_string ref) ;
            Sexplib.Sexp.pp_hum Caml.Format.std_formatter
              (sexp_of_list
                 (sexp_of_list
                    (Stdppx.sexp_of_pair sexp_of_string sexp_of_value) )
                 vars_scope ) ;
            Sexplib.Sexp.pp_hum Caml.Format.std_formatter
              (sexp_of_list (sexp_of_list sexp_of_tbinding) global_bindings) ;
            raise Errors.InternalCompilerError
        | Some (Ok v) ->
            Some v
        | None ->
            None

    method private check_unions_for_doubled_types : type_ list -> type_ list =
      fun xs ->
        List.fold xs ~init:[] ~f:(fun acc x ->
            match List.exists acc ~f:(equal_type_ x) with
            | true ->
                errors#report `Error (`DuplicateVariant x) () ;
                acc
            | false ->
                x :: acc )

    (* This method resolves references that can be resolved at this point of execution. *)
    method private partial_evaluate_function : function_ -> function_ =
      fun fn ->
        (* TODO: This should evaluate everything that can. *)
        let evaluator =
          object (self_eval)
            inherit [_] Lang_types.map as super

            val mutable local_vars = []

            method! visit_expr env =
              function
              | Reference (ref, ty) -> (
                match
                  List.find_map local_vars ~f:(fun binds ->
                      List.find binds ~f:(fun x -> equal_string x ref) )
                with
                | Some ref ->
                    let new_ty = self_eval#visit_type_ env ty in
                    Reference (ref, new_ty)
                | None -> (
                  match self#find_ref ref with
                  | Some ex ->
                      ex
                  | None ->
                      let new_ty = self_eval#visit_type_ env ty in
                      Reference (ref, new_ty) ) )
              | ex ->
                  super#visit_expr env ex

            method! visit_type_ env ty =
              self_eval#unwrap_expr_types (super#visit_type_ env ty)

            method private unwrap_expr_types =
              function
              | ExprType (Value (Type t)) ->
                  self_eval#unwrap_expr_types t
              | ExprType (ResolvedReference (_, Value (Type t))) ->
                  self_eval#unwrap_expr_types t
              | t ->
                  t

            method! visit_Block env b =
              self_eval#with_vars [] (fun _ -> super#visit_Block env b)

            method! visit_Let env vars =
              (* TODO: this does not work if `vars` will be actually a list. *)
              let vars' = super#visit_Let env vars in
              let vars_names = List.map vars ~f:(fun (name, _) -> name) in
              local_vars <- vars_names :: local_vars ;
              vars'

            method! visit_DestructuringLet _env let_ =
              match let_.destructuring_let_expr with
              | Reference (_, StructType id) | Value (Struct (id, _)) ->
                  let struct_ = Program.get_struct program id in
                  (* Check if field names are correct *)
                  List.iter let_.destructuring_let ~f:(fun (name, _) ->
                      if
                        List.Assoc.find struct_.struct_fields
                          ~equal:String.equal name
                        |> Option.is_some
                      then ()
                      else errors#report `Error (`FieldNotFound (id, name)) () ) ;
                  (* If rest of fields are not ignored, check for completeness *)
                  if let_.destructuring_let_rest then ()
                  else
                    List.iter struct_.struct_fields ~f:(fun (name, _) ->
                        if
                          List.Assoc.find let_.destructuring_let
                            ~equal:String.equal name
                          |> Option.is_some
                        then ()
                        else errors#report `Error (`MissingField (id, name)) () ) ;
                  local_vars <-
                    List.map let_.destructuring_let ~f:snd :: local_vars ;
                  DestructuringLet let_
              | expr ->
                  errors#report `Error (`UninterpretableStatement (Expr expr))
                    () ;
                  Invalid

            method! visit_function_ env f =
              let sign =
                self_eval#visit_function_signature env f.function_signature
              in
              let args =
                List.map sign.function_params ~f:(fun (name, _) -> name)
              in
              self_eval#with_vars args (fun _ ->
                  let body =
                    self_eval#visit_function_impl env f.function_impl
                  in
                  {function_signature = sign; function_impl = body} )

            method! visit_IntfMethodCall env call =
              let intf_instance = self_eval#visit_expr env call.intf_instance in
              let args =
                self_eval#visit_list self_eval#visit_expr env call.intf_args
              in
              let is_dependent = function
                | Value (Type (Dependent _)) ->
                    true
                | _ ->
                    false
              in
              match
                is_immediate_expr intf_instance
                && not (is_dependent intf_instance)
              with
              | true -> (
                  let intf_ty =
                    match self#interpret_expr intf_instance with
                    | Type t ->
                        t
                    | _ ->
                        raise InternalCompilerError
                  in
                  match
                    Program.find_impl_intf program call.intf_def intf_ty
                  with
                  | Some impl ->
                      let method_ =
                        List.find_map_exn impl.impl_methods
                          ~f:(fun (name, impl) ->
                            let method_name, _ = call.intf_method in
                            if equal_string name method_name then Some impl
                            else None )
                      in
                      FunctionCall (method_, args)
                  | None ->
                      print_sexp (sexp_of_type_ intf_ty) ;
                      print_sexp (sexp_of_intf_method_call call) ;
                      raise InternalCompilerError )
              | false ->
                  IntfMethodCall {call with intf_instance; intf_args = args}

            method private with_vars : 'a. _ -> (unit -> 'a) -> 'a =
              fun vars f ->
                let prev_vars = local_vars in
                local_vars <- vars :: local_vars ;
                let out = f () in
                local_vars <- prev_vars ;
                out
          end
        in
        let new_fn = evaluator#visit_function_ () fn in
        new_fn
  end

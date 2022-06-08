open Base
open Lang_types
open Errors

type error =
  [ `UnresolvedIdentifier of string
  | `UninterpretableStatement of stmt
  | `UnexpectedType of type_
  | `FieldNotFound of struct_ * string
  | `ArgumentNumberMismatch ]
[@@deriving equal, sexp_of]

(*TODO: type checks for arguments*)
class interpreter
  ((program, bindings, errors, _functions) :
    program * tbinding list list * _ errors * int ) =
  object (self)
    val global_bindings = bindings

    val mutable vars_scope = []

    val mutable return = Void

    val mutable adding_structs = []

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
      | Invalid ->
          Void

    method interpret_expr : expr -> value =
      fun expr ->
        match expr with
        | FunctionCall fc ->
            self#interpret_fc fc
        | ResolvedReference (_, expr') ->
            self#interpret_expr expr'
        | Reference (name, _) -> (
          match self#find_ref name with
          | Some expr' ->
              self#interpret_expr expr'
          | None ->
              errors#report `Error (`UnresolvedIdentifier name) () ;
              Sexplib.Sexp.pp_hum Caml.Format.std_formatter
                (sexp_of_list
                   (sexp_of_list
                      (Stdppx.sexp_of_pair sexp_of_string sexp_of_value) )
                   vars_scope ) ;
              raise InternalCompilerError )
        | StructField (struct_, field) -> (
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
        | Primitive _ | InvalidExpr | Hole ->
            errors#report `Error (`UninterpretableStatement (Expr expr)) () ;
            Void

    method interpret_type : type_ -> type_ =
      function
      | ExprType ex ->
          expr_to_type (Value (self#interpret_expr ex))
      | StructType {struct_fields; struct_id} ->
          let struct_fields =
            List.map struct_fields ~f:(fun (name, {field_type}) ->
                (name, {field_type = self#interpret_type field_type}) )
          in
          let s = {struct_fields; struct_id} in
          if not (List.exists adding_structs ~f:(equal_struct_ s)) then
            if is_immediate_expr (Value (Type (StructType s))) then
              self#add_struct_to_program s ;
          StructType {struct_fields; struct_id}
      | UnionType u ->
          UnionType {cases = List.map u.cases ~f:self#interpret_type}
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
        | Function f ->
            Function (self#interpret_function f)
        | value ->
            value

    method interpret_function : function_ -> function_ =
      fun f ->
        { function_signature =
            { function_params =
                List.map f.function_signature.function_params
                  ~f:(fun (name, x) -> (name, self#interpret_type x));
              function_returns =
                self#interpret_type f.function_signature.function_returns };
          function_impl = (match f.function_impl with Fn b -> Fn b | x -> x) }

    method interpret_fc : function_call -> value =
      fun (func, args) ->
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
        match self#interpret_expr func with
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
              let program' =
                { program with
                  bindings =
                    extract_comptime_bindings
                      (Option.value (List.hd global_bindings) ~default:[]) }
              in
              let value = function_impl program' args' in
              program.methods <- program'.methods ;
              value
          | _ ->
              Void )
        | _ ->
            Void

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

    method private add_struct_to_program struct_ =
      let prev_structs = adding_structs in
      adding_structs <- struct_ :: adding_structs ;
      let add_methods () =
        if
          not
            (List.exists program.methods ~f:(fun (s, _) ->
                 equal_value s (Type (StructType struct_)) ) )
        then
          match
            List.find_map program.methods_defs ~f:(fun (v, methods) ->
                match v with
                | Type (StructType s) ->
                    if equal_struct_ s struct_ then Some methods else None
                | _ ->
                    None )
          with
          | Some methods_defs ->
              let methods_monomorphed =
                List.map methods_defs ~f:(fun (name, f) ->
                    (name, self#interpret_function f) )
              in
              program.methods <-
                (Type (StructType struct_), methods_monomorphed)
                :: program.methods
          | None ->
              ()
      in
      let add_impls () =
        if
          not
            (List.exists program.methods ~f:(fun (s, _) ->
                 equal_value s (Type (StructType struct_)) ) )
        then
          match
            List.find_map program.impls_defs ~f:(fun (v, impls) ->
                match v with
                | Type (StructType s) ->
                    if equal_struct_ s struct_ then Some impls else None
                | _ ->
                    None )
          with
          | Some impls_defs ->
              let impls_monomorphed =
                List.map impls_defs ~f:(fun impl ->
                    { impl_interface =
                        Value (self#interpret_expr impl.impl_interface);
                      impl_methods =
                        List.map impl.impl_methods ~f:(fun (n, x) ->
                            (n, Value (self#interpret_expr x)) ) } )
              in
              program.impls <-
                (Type (StructType struct_), impls_monomorphed) :: program.impls
          | None ->
              ()
      in
      add_methods () ;
      add_impls () ;
      adding_structs <- prev_structs ;
      ()
  end

open Base

module Make =
functor
  (Syntax : Syntax.T)
  ->
  struct
    (* Error management *)
    type ('w, 'e) error_list =
      {mutable warnings : 'w list ref; mutable errors : 'e list ref}
    [@@deriving make]

    let new_warn warn errors = errors.warnings := !(errors.warnings) @ [warn]

    let new_error error errors = errors.errors := !(errors.errors) @ [error]

    (* *)

    class ['s] base_map =
      object (_ : 's)
        method visit_z : 'env. 'env -> Z.t -> Z.t = fun _env z -> z
      end

    (* Z wrapper to enable show derivation *)
    module Z' = struct
      type t = [%import: Z.t]

      let pp = Z.pp_print
    end

    type z = Z'.t [@@deriving show {with_path = false}]

    let sexp_of_z z = Sexplib.Sexp.of_string (Z.to_string z)

    type 'a named_map = ((string * 'a) list[@sexp.list])

    and term =
      | Void
      | Type of type_
      | Function of function_
      | FunctionCall of term * term list
      | Integer of (z[@visitors.name "z"])
      | Reference of string
      | ResolvedReference of string * term
      | Builtin of builtin
      | Invalid

    and builtin = string

    and kind =
      | ResolvedReferenceKind of string * kind
      | BuiltinKind of builtin
      | TypeKind of type_
      | FunctionKind of function_
      | ReferenceKind of string
      | UnsupportedKind of term

    and fn =
      { function_params : kind named_map;
        function_returns : kind;
        function_body : stmt list option [@sexp.option] }

    and builtin_fn = (env -> term list -> term[@visitors.opaque])

    and function_ = Fn of fn | BuiltinFn of builtin_fn

    and stmt = Return of term | Term of term

    and type_ =
      {type_fields : type_field named_map; type_methods : function_ named_map}

    and type_field = {field_type : kind}

    and env = {scope : scope}

    and scope = term named_map
    [@@deriving
      sexp_of,
        visitors
          { variety = "map";
            polymorphic = true;
            concrete = true;
            ancestors = ["base_map"] }]

    let as_amap (l : 'a named_map) f =
      Map.to_alist (f (Map.of_alist_exn (module String) l))

    let in_amap (l : 'a named_map) f = f (Map.of_alist_exn (module String) l)

    let term_to_kind value =
      match value with
      | Reference value ->
          ReferenceKind value
      | Type type_ ->
          TypeKind type_
      | Function function_ ->
          FunctionKind function_
      | Builtin builtin ->
          BuiltinKind builtin
      | _ ->
          UnsupportedKind value
      [@@deriving sexp_of]

    type error =
      | Duplicate_Identifier of string * term
      | Duplicate_Field of string * type_
      | Duplicate_Param of string * function_
      | Invalid_Param_Kind of string * function_
      | Invalid_Return_Kind of function_
      | Unresolved of string
      | Recursive_Reference of string
      | Unsupported
    [@@deriving sexp_of]

    type elist = (string, error) error_list

    let comptime_println _env args =
      let f = Caml.Format.std_formatter in
      List.iter args ~f:(fun x -> Sexplib.Sexp.pp_hum f (sexp_of_term x)) ;
      Caml.Format.pp_print_newline f () ;
      Void

    let empty_scope =
      [ ("Int257", Builtin "Int257");
        ("Bool", Builtin "Bool");
        ("println", Function (BuiltinFn comptime_println)) ]

    (* Resolves referenced types *)
    class ['s] reference_resolver ((env, errors) : env * elist) =
      object (s : 's)
        inherit ['s] map

        val p_env = env

        val p_errors = errors

        val lookup_path = ref []

        method! visit_Reference env ref =
          match in_amap p_env.scope (fun m -> Map.find m ref) with
          | Some (Reference ref') ->
              if List.exists !lookup_path ~f:(String.equal ref) then (
                new_error (Recursive_Reference ref) p_errors ;
                Reference ref )
              else (
                lookup_path := ref' :: !lookup_path ;
                let ref_ = s#visit_Reference env ref' in
                lookup_path := List.drop !lookup_path 1 ;
                match ref_ with
                | ResolvedReference (_, v) ->
                    ResolvedReference (ref, v)
                | v ->
                    v )
          | Some (ResolvedReference (_, t)) | Some t ->
              ResolvedReference (ref, t)
          | None ->
              new_error (Unresolved ref) p_errors ;
              Reference ref

        method! visit_ReferenceKind _env ref =
          match in_amap p_env.scope (fun m -> Map.find m ref) with
          | Some (ResolvedReference (_, t)) | Some t ->
              ResolvedReferenceKind (ref, term_to_kind t)
          | None ->
              new_error (Unresolved ref) p_errors ;
              ReferenceKind ref
      end

    (* Strips resolved references types *)
    class ['s] resolved_references_stripper (env : env) =
      object (_ : 's)
        inherit ['s] map

        val p_env = env

        method! visit_ResolvedReference _env _ t = t

        method! visit_ResolvedReferenceKind _env _ k = k
      end

    (* Evaluates compile-time function calls *)
    (* NB: Currently handles builtin functions only *)
    class ['s] function_call_evaluator (env : env) =
      object (_ : 's)
        inherit ['s] map

        val p_env = env

        method! visit_FunctionCall _env f args =
          match f with
          | Function (BuiltinFn fn)
          | ResolvedReference (_, Function (BuiltinFn fn)) ->
              fn p_env args
          | _ ->
              FunctionCall (f, args)

        method! visit_ResolvedReferenceKind _env _ k = k
      end

    let rec env_from_program (stx : Syntax.program) (elist : elist) =
      let scope = scope_from_bindings stx.bindings elist in
      (* Resolve references inside *)
      let env =
        let env = {scope} in
        let resolver = new reference_resolver (env, elist) in
        resolver#visit_env () env
      in
      let env =
        let evaluator = new function_call_evaluator env in
        evaluator#visit_env () env
      in
      env

    and scope_from_bindings bindings elist =
      let scope = empty_scope in
      List.fold ~init:scope
        ~f:(fun scope binding ->
          let binding = Syntax.value binding in
          let ident =
            Syntax.ident_to_string (Syntax.value binding.binding_name)
          in
          match in_amap scope (fun m -> Map.find m ident) with
          | None -> (
            match binding_to_term binding elist with
            | Ok data ->
                as_amap scope (fun m -> Map.set m ~key:ident ~data)
            | Error e ->
                new_error e elist ; scope )
          | Some existing ->
              new_error (Duplicate_Identifier (ident, existing)) elist ;
              scope )
        bindings

    and binding_to_term binding elist =
      expr_to_term (Syntax.value binding.binding_expr) () elist

    and expr_to_term expr loc elist =
      match expr with
      | Type s ->
          Ok (type_to_type s loc elist)
      | Int i ->
          Ok (Integer i)
      | Reference ref ->
          Ok (Reference (Syntax.ident_to_string ref))
      | Function f ->
          Ok (function_to_function f loc elist)
      | FunctionCall fc ->
          Ok (function_call_to_function_call fc elist)
      | _ ->
          Error Unsupported

    and expr_to_stmt expr loc elist =
      match expr with
      | Syntax.Return expr' ->
          Result.map (expr_to_term expr' loc elist) ~f:(fun v -> Return v)
      | _ ->
          Result.map (expr_to_term expr loc elist) ~f:(fun v -> Term v)

    and type_to_type s _loc elist =
      let s' = {type_fields = []; type_methods = []} in
      let s =
        List.fold ~init:s'
          ~f:(fun s' field ->
            let ident =
              Syntax.ident_to_string
                ((Syntax.value field).field_name |> Syntax.value)
            in
            match in_amap s'.type_fields (fun m -> Map.find m ident) with
            | Some _ ->
                new_error (Duplicate_Field (ident, s')) elist ;
                s'
            | None -> (
                let value =
                  expr_to_term
                    ((Syntax.value field).field_type |> Syntax.value)
                    () elist
                in
                match Result.map value ~f:term_to_kind with
                | Ok value ->
                    { s' with
                      type_fields =
                        as_amap s'.type_fields (fun m ->
                            Map.set m ~key:ident ~data:{field_type = value} ) }
                | Error e ->
                    new_error e elist ; s' ) )
          s.fields
      in
      Type s

    and function_to_function f _loc elist =
      (* return kind *)
      let return =
        match expr_to_term (Syntax.value f.returns) () elist with
        | Ok v ->
            v
        | Error err ->
            new_error err elist ; Invalid
      in
      let f' =
        { function_params = [];
          function_returns = UnsupportedKind return;
          function_body =
            Option.map f.exprs ~f:(fun exprs ->
                let code =
                  Result.map
                    (List.fold_result ~init:[]
                       ~f:(fun acc expr ->
                         Result.map
                           (expr_to_stmt (Syntax.value expr) () elist)
                           ~f:(fun e -> e :: acc) )
                       exprs )
                    ~f:List.rev
                in
                match code with Ok code -> code | Error _ -> [] ) }
      in
      let f' =
        { f' with
          function_returns =
            ( match term_to_kind return with
            | UnsupportedKind _value ->
                new_error (Invalid_Return_Kind (Fn f')) elist ;
                f'.function_returns
            | kind ->
                kind ) }
      in
      (* collect params *)
      let f =
        List.fold ~init:f'
          ~f:(fun f' param ->
            let name, expr = Syntax.value param in
            let ident = Syntax.ident_to_string (Syntax.value name) in
            match in_amap f'.function_params (fun m -> Map.find m ident) with
            | Some _ ->
                new_error (Duplicate_Param (ident, Fn f')) elist ;
                f'
            | None -> (
                let value = expr_to_term (Syntax.value expr) () elist in
                match Result.map value ~f:term_to_kind with
                | Ok (UnsupportedKind _) ->
                    new_error (Invalid_Param_Kind (ident, Fn f')) elist ;
                    f'
                | Ok kind ->
                    { f' with
                      function_params =
                        as_amap f'.function_params (fun m ->
                            Map.set m ~key:ident ~data:kind ) }
                | Error e ->
                    new_error e elist ; f' ) )
          f.params
      in
      Function (Fn f)

    and function_call_to_function_call fc elist =
      let args =
        List.map fc.arguments ~f:(fun x ->
            result_to_term (expr_to_term (Syntax.value x) () elist) elist )
      in
      FunctionCall
        (result_to_term (expr_to_term (Syntax.value fc.fn) () elist) elist, args)

    and result_to_term r elist =
      match r with Ok t -> t | Error e -> new_error e elist ; Invalid
  end

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

    let equal_z = Z.equal

    type 'a named_map = ((string * 'a) list[@sexp.list])

    and term =
      | Void
      | Struct of struct_
      | Function of function_
      | FunctionCall of term * term list
      | Integer of (z[@visitors.name "z"])
      | Reference of string
      | ResolvedReference of string * term
      | Builtin of builtin
      | Invalid
      | Hole

    and builtin = string

    and type_ =
      | VoidType
      | ResolvedReferenceType of string * type_
      | BuiltinType of builtin
      | StructType of struct_
      | FunctionType of function_
      | ReferenceType of string
      | TermType of term
      | HoleType

    and fn =
      { function_params : type_ named_map;
        function_returns : type_;
        function_body : stmt list option [@sexp.option] }

    and builtin_fn = (env -> term list -> term[@visitors.opaque] [@equal.ignore])

    and function_ = Fn of fn | BuiltinFn of builtin_fn

    and stmt = Return of term | Term of term

    and struct_ =
      { struct_fields : struct_field named_map;
        struct_methods : function_ named_map;
        id : (int[@sexp.opaque]) }

    and struct_field = {field_type : type_}

    and env = {scope : scope}

    and scope = term named_map
    [@@deriving
      equal,
        sexp_of,
        visitors
          { variety = "map";
            polymorphic = true;
            concrete = true;
            ancestors = ["base_map"] }]

    let as_amap (l : 'a named_map) f =
      Map.to_alist (f (Map.of_alist_exn (module String) l))

    let in_amap (l : 'a named_map) f = f (Map.of_alist_exn (module String) l)

    let term_to_type value =
      match value with
      | Reference value ->
          ReferenceType value
      | Struct struct_ ->
          StructType struct_
      | Function function_ ->
          FunctionType function_
      | Builtin builtin ->
          BuiltinType builtin
      | Void ->
          VoidType
      | Hole ->
          HoleType
      | _ ->
          TermType value
      [@@deriving sexp_of]

    type error =
      | Duplicate_Identifier of string * term
      | Duplicate_Field of string * struct_
      | Duplicate_Param of string * function_
      | Invalid_Param_Type of string * function_
      | Invalid_Return_Type of function_
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
      [ ("Void", Void);
        ("Type", Builtin "Type");
        ("Int257", Builtin "Int257");
        ("Bool", Builtin "Bool");
        ("println", Function (BuiltinFn comptime_println)) ]

    let default_env = {scope = empty_scope}

    let rec is_immediate_term = function
      | Void ->
          true
      | Struct _ ->
          true
      | Function _ ->
          true
      | FunctionCall _ ->
          false
      | Integer _ ->
          true
      | Reference _ ->
          false
      | ResolvedReference (_, t) ->
          is_immediate_term t
      | Builtin _ ->
          true
      | Hole ->
          false
      | Invalid ->
          false

    (* Resolves referenced types *)
    class ['s] reference_resolver ((env, errors) : env * elist) =
      object (s : 's)
        inherit ['s] map as super

        val p_env = env

        val p_errors = errors

        val mutable lookup_path = []

        val mutable scoped_identifiers = []

        method! visit_fn env fn =
          let scoped_identifiers' = scoped_identifiers in
          scoped_identifiers <-
            List.map fn.function_params ~f:(fun (n, _) -> n)
            :: scoped_identifiers' ;
          let fn' = super#visit_fn env fn in
          scoped_identifiers <- scoped_identifiers' ;
          fn'

        method private scoped_identifier ref =
          Option.is_some
            (List.find scoped_identifiers ~f:(fun terms ->
                 Option.is_some (List.find terms ~f:(String.equal ref)) ) )

        method private find_ref : 'env. 'env -> string -> term option =
          fun env ref ->
            match in_amap p_env.scope (fun m -> Map.find m ref) with
            | Some (Reference ref') ->
                if List.exists lookup_path ~f:(String.equal ref') then (
                  new_error (Recursive_Reference ref) p_errors ;
                  None )
                else
                  let path = lookup_path in
                  lookup_path <- ref :: lookup_path ;
                  let t = s#find_ref env ref' in
                  lookup_path <- path ;
                  t
            | Some (ResolvedReference (_, t)) | Some t ->
                Some (s#visit_term env t)
            | other ->
                other

        method! visit_Reference env ref =
          if s#scoped_identifier ref then Reference ref
          else
            match s#find_ref env ref with
            | Some (ResolvedReference (_, t)) | Some t ->
                ResolvedReference (ref, t)
            | None ->
                new_error (Unresolved ref) p_errors ;
                Reference ref

        method! visit_ReferenceType env ref =
          if s#scoped_identifier ref then ReferenceType ref
          else
            match s#find_ref env ref with
            | Some (ResolvedReference (_, t)) | Some t ->
                ResolvedReferenceType (ref, term_to_type t)
            | None ->
                new_error (Unresolved ref) p_errors ;
                ReferenceType ref
      end

    (* Strips resolved references types *)
    class ['s] resolved_references_stripper (env : env) =
      object (s : 's)
        inherit ['s] map

        val p_env = env

        method! visit_ResolvedReference env _ t = s#visit_term env t

        method! visit_ResolvedReferenceType env _ k = s#visit_type_ env k
      end

    (* Evaluates compile-time function calls *)
    class ['s] function_call_evaluator (env : env) =
      object (s : 's)
        inherit ['s] map

        val p_env = env

        method private interpret_function fn args =
          let eval_term _env = function term -> term in
          let env = p_env in
          let args' =
            List.map (List.zip_exn fn.function_params args)
              ~f:(fun ((n, _), a) -> (n, a))
          in
          let env' = {scope = args' @ env.scope} in
          let stripped =
            equal_env env'
              ((new resolved_references_stripper env)#visit_env () env')
          in
          let statements =
            List.map (Option.value fn.function_body ~default:[]) ~f:(fun stmt ->
                let stmt =
                  (new reference_resolver
                     (env', {warnings = ref []; errors = ref []}) )
                    #visit_stmt () stmt
                in
                let stmt =
                  if stripped then
                    (new resolved_references_stripper env')#visit_stmt () stmt
                  else stmt
                in
                let fce : 's = new function_call_evaluator env' in
                fce#visit_stmt () stmt )
          in
          List.fold_until ~init:Void
            ~f:
              (fun _ -> function
                | Return term ->
                    Stop (eval_term env' term)
                | Term term ->
                    Continue (eval_term env' term) )
            ~finish:(fun state -> state)
            statements

        method! visit_FunctionCall _env f args =
          let immediate_arguments =
            Option.is_none
              (List.find args ~f:(fun a -> not (is_immediate_term a)))
          in
          let rewrite_fn fn =
            let has_body = Option.is_some fn.function_body in
            if immediate_arguments && has_body then s#interpret_function fn args
            else FunctionCall (Function (Fn fn), args)
          in
          match f with
          | Function (BuiltinFn fn)
          | ResolvedReference (_, Function (BuiltinFn fn))
            when immediate_arguments ->
              fn p_env args
          | Function (Fn fn) ->
              rewrite_fn fn
          | ResolvedReference (n, Function (Fn fn)) ->
              ResolvedReference (n, rewrite_fn fn)
          | _ ->
              FunctionCall (f, args)
      end

    (* Assigns unique identifiers to types *)
    class ['s] struct_unique_id_assigner (env : env) =
      object (_ : 's)
        inherit ['s] map

        val p_env = env

        val mutable counter = 0

        method! visit_struct_ _env t =
          let t' = {t with id = counter} in
          counter <- counter + 1 ;
          t'
      end

    let rec env_from_program ?(env = default_env) (stx : Syntax.program)
        (elist : elist) =
      let scope = scope_from_bindings stx.bindings elist ~scope:env.scope in
      let env = {scope} in
      (* Resolve references inside *)
      let env = (new struct_unique_id_assigner env)#visit_env () env in
      let env = (new reference_resolver (env, elist))#visit_env () env in
      env

    and eval_env env =
      let evaluator = new function_call_evaluator env in
      evaluator#visit_env () env

    and scope_from_bindings ?(scope = empty_scope) bindings elist =
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
      | Struct s ->
          Ok (struct_to_struct s loc elist)
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

    and struct_to_struct s _loc elist =
      let s' = {struct_fields = []; struct_methods = []; id = 0} in
      let s =
        List.fold ~init:s'
          ~f:(fun s' field ->
            let ident =
              Syntax.ident_to_string
                ((Syntax.value field).field_name |> Syntax.value)
            in
            match in_amap s'.struct_fields (fun m -> Map.find m ident) with
            | Some _ ->
                new_error (Duplicate_Field (ident, s')) elist ;
                s'
            | None -> (
                let value =
                  expr_to_term
                    ((Syntax.value field).field_type |> Syntax.value)
                    () elist
                in
                match Result.map value ~f:term_to_type with
                | Ok value ->
                    { s' with
                      struct_fields =
                        as_amap s'.struct_fields (fun m ->
                            Map.set m ~key:ident ~data:{field_type = value} ) }
                | Error e ->
                    new_error e elist ; s' ) )
          s.fields
      in
      Struct s

    and function_to_function f _loc elist =
      (* return type *)
      let return =
        match
          Option.map f.returns ~f:(fun e ->
              expr_to_term (Syntax.value e) () elist )
          |> Option.value ~default:(Ok Hole)
        with
        | Ok v ->
            v
        | Error err ->
            new_error err elist ; Invalid
      in
      let f' =
        { function_params = [];
          function_returns = TermType return;
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
            ( match term_to_type return with
            | TermType _value ->
                new_error (Invalid_Return_Type (Fn f')) elist ;
                f'.function_returns
            | type_ ->
                type_ ) }
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
                match Result.map value ~f:term_to_type with
                | Ok (TermType _) ->
                    new_error (Invalid_Param_Type (ident, Fn f')) elist ;
                    f'
                | Ok type_ ->
                    { f' with
                      function_params =
                        as_amap f'.function_params (fun m ->
                            Map.set m ~key:ident ~data:type_ ) }
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

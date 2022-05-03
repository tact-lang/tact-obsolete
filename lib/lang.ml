open Base

(* Error management *)
type ('w, 'e) error_list =
  {mutable warnings : 'w list ref; mutable errors : 'e list ref}
[@@deriving make]

let new_warn warn errors = errors.warnings := !(errors.warnings) @ [warn]

let new_error error errors = errors.errors := !(errors.errors) @ [error]

(* *)

type 'a named_map = (string, 'a, String.comparator_witness) Map.t

let equal_named_map = Map.equal

type value =
  | Type of type_
  | Function of function_
  | Integer of Z.t
  | Reference of string
  | ResolvedReference of string * value
  | Builtin of builtin
  | Invalid

and builtin = string

and kind =
  | ResolvedReferenceKind of string * kind
  | BuiltinKind of builtin
  | TypeKind of type_
  | FunctionKind of function_
  | ReferenceKind of string
  | UnsupportedKind of value

and function_ =
  { function_loc : Syntax.loc;
    function_params : kind named_map;
    function_returns : kind;
    function_body : code_expr list option }

and code_expr = Return of value | Value of value

and type_ =
  { type_loc : Syntax.loc;
    type_fields : type_field named_map;
    type_methods : function_ named_map }

and type_field = {field_type : kind}

and env = {scope : scope}

and scope = value named_map

and error =
  | Duplicate_Identifier of string * value
  | Duplicate_Field of string * type_
  | Duplicate_Param of string * function_
  | Invalid_Param_Kind of string * function_
  | Invalid_Return_Kind of function_
  | Unresolved of string
  | Recursive_Reference of string
  | Unsupported
[@@deriving equal]

type elist = (string, error) error_list

let empty_scope =
  Map.of_alist_exn
    (module String)
    [("Int257", Builtin "Int257"); ("Bool", Builtin "Bool")]

let rec env_from_program (stx : Syntax.program) (elist : elist) =
  let scope = scope_from_bindings stx.bindings elist in
  (* Resolve top-level scope first *)
  let scope = resolve_scope scope elist in
  (* Resolve references inside *)
  let scope = resolve_inner scope elist in
  (* Build env *)
  {scope}

and resolve_inner scope elist =
  let scope' = Map.to_alist scope in
  let resolve resolved (k, v) =
    match v with
    | Type s ->
        let fields = Map.to_alist s.type_fields in
        let _, resolved =
          List.fold ~init:(s, resolved)
            ~f:(fun (s, resolved) (field_name, field) ->
              match field with
              | {field_type = ReferenceKind ref; _} -> (
                match Map.find resolved ref with
                | Some (ResolvedReference (_, t)) | Some t ->
                    let s' =
                      { s with
                        type_fields =
                          Map.set s.type_fields ~key:field_name
                            ~data:
                              { field_type =
                                  ResolvedReferenceKind (ref, value_to_kind t)
                              } }
                    in
                    (s', Map.set resolved ~key:k ~data:(Type s'))
                | None ->
                    new_error (Unresolved ref) elist ;
                    (s, resolved) )
              | _ ->
                  (s, resolved) )
            fields
        in
        resolved
    | Function f ->
        let return = f.function_returns in
        let f' =
          match return with
          | ReferenceKind ref -> (
            match Map.find resolved ref with
            | Some (ResolvedReference (_, t)) | Some t ->
                let f' =
                  { f with
                    function_returns =
                      ResolvedReferenceKind (ref, value_to_kind t) }
                in
                List.fold ~init:f'
                  ~f:(fun f (param, kind) ->
                    match kind with
                    | ReferenceKind ref -> (
                      match Map.find resolved ref with
                      | Some (ResolvedReference (_, t)) | Some t ->
                          { f with
                            function_params =
                              Map.set f.function_params ~key:param
                                ~data:
                                  (ResolvedReferenceKind (ref, value_to_kind t))
                          }
                      | None ->
                          new_error (Unresolved ref) elist ;
                          f )
                    | _ ->
                        f )
                  (Map.to_alist f.function_params)
            | None ->
                new_error (Unresolved ref) elist ;
                f )
          | _ ->
              f
        in
        Map.set resolved ~key:k ~data:(Function f')
    | _ ->
        resolved
  in
  List.fold ~init:scope ~f:resolve scope'

and resolve_scope scope elist =
  let scope' = Map.to_alist scope in
  let rec resolve path resolved (k, v) =
    match v with
    | Reference ref -> (
        if List.exists path ~f:(String.equal ref) then (
          new_error (Recursive_Reference k) elist ;
          resolved )
        else
          match Map.find resolved ref with
          | None ->
              new_error (Unresolved ref) elist ;
              resolved
          | Some (Reference ref') ->
              resolve (ref :: path) resolved (k, Reference ref')
          | Some value ->
              Map.set resolved ~key:k ~data:value )
    | _ ->
        resolved
  in
  List.fold ~init:scope ~f:(resolve []) scope'

and scope_from_bindings bindings elist =
  let scope = empty_scope in
  List.fold ~init:scope
    ~f:(fun scope binding ->
      let binding = binding.value in
      let ident = Syntax.ident_to_string binding.binding_name.value in
      match Map.find scope ident with
      | None -> (
        match binding_to_value binding elist with
        | Ok data ->
            Map.set scope ~key:ident ~data
        | Error e ->
            new_error e elist ; scope )
      | Some existing ->
          new_error (Duplicate_Identifier (ident, existing)) elist ;
          scope )
    bindings

and binding_to_value binding elist =
  expr_to_value binding.binding_expr.value binding.binding_expr.loc elist

and expr_to_value expr loc elist =
  match expr with
  | Type s ->
      Ok (type_to_type s loc elist)
  | Int i ->
      Ok (Integer i)
  | Reference ref ->
      Ok (Reference (Syntax.ident_to_string ref))
  | Function f ->
      Ok (function_to_function f loc elist)
  | _ ->
      Error Unsupported

and expr_to_code_expr expr loc elist =
  match expr with
  | Syntax.Return expr' ->
      Result.map (expr_to_value expr' loc elist) ~f:(fun v -> Return v)
  | _ ->
      Result.map (expr_to_value expr loc elist) ~f:(fun v -> Value v)

and type_to_type s loc elist =
  let s' =
    { type_loc = loc;
      type_fields = Map.empty (module String);
      type_methods = Map.empty (module String) }
  in
  let s =
    List.fold ~init:s'
      ~f:(fun s' field ->
        let ident = Syntax.ident_to_string field.value.field_name.value in
        match Map.find s'.type_fields ident with
        | Some _ ->
            new_error (Duplicate_Field (ident, s')) elist ;
            s'
        | None -> (
            let value =
              expr_to_value field.value.field_type.value
                field.value.field_type.loc elist
            in
            match Result.map value ~f:value_to_kind with
            | Ok value ->
                { s' with
                  type_fields =
                    Map.set s'.type_fields ~key:ident ~data:{field_type = value}
                }
            | Error e ->
                new_error e elist ; s' ) )
      s.fields
  in
  Type s

and function_to_function f loc elist =
  (* return kind *)
  let return =
    match expr_to_value f.returns.value f.returns.loc elist with
    | Ok v ->
        v
    | Error err ->
        new_error err elist ; Invalid
  in
  let f' =
    { function_loc = loc;
      function_params = Map.empty (module String);
      function_returns = UnsupportedKind return;
      function_body =
        Option.map f.exprs ~f:(fun exprs ->
            let code =
              Result.map
                (List.fold_result ~init:[]
                   ~f:(fun acc expr ->
                     Result.map (expr_to_code_expr expr.value expr.loc elist)
                       ~f:(fun e -> e :: acc) )
                   exprs )
                ~f:List.rev
            in
            match code with Ok code -> code | Error _ -> [] ) }
  in
  let f' =
    { f' with
      function_returns =
        ( match value_to_kind return with
        | UnsupportedKind _value ->
            new_error (Invalid_Return_Kind f') elist ;
            f'.function_returns
        | kind ->
            kind ) }
  in
  (* collect params *)
  let f =
    List.fold ~init:f'
      ~f:(fun f' param ->
        let name, expr = param.value in
        let ident = Syntax.ident_to_string name.value in
        match Map.find f'.function_params ident with
        | Some _ ->
            new_error (Duplicate_Param (ident, f')) elist ;
            f'
        | None -> (
            let value = expr_to_value expr.value expr.loc elist in
            match Result.map value ~f:value_to_kind with
            | Ok (UnsupportedKind _) ->
                new_error (Invalid_Param_Kind (ident, f')) elist ;
                f'
            | Ok kind ->
                { f' with
                  function_params =
                    Map.set f'.function_params ~key:ident ~data:kind }
            | Error e ->
                new_error e elist ; f' ) )
      f.params
  in
  Function f

and value_to_kind value =
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

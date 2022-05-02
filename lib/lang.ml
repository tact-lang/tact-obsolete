open Base

type 'a named_map = (string, 'a, String.comparator_witness) Map.t

let equal_named_map = Map.equal

type value =
  | Struct of struct_
  | Integer of Z.t
  | Reference of string
  | Resolved_Reference of string * value
  | Builtin of string

and function_ =
  { function_arguments : value named_map;
    function_returns : value;
    function_body : code_expr list }

and code_expr =
  | FunctionCall of function_ * code_expr named_map
  | Value of value

and struct_ =
  { struct_loc : Syntax.loc;
    struct_fields : struct_field named_map;
    struct_methods : function_ named_map }

and struct_field = {field_type : value}

and env = {scope : scope}

and scope = value named_map

and error =
  | Duplicate_Identifier of string * value
  | Duplicate_Field of string * struct_
  | Unresolved of string
  | Unsupported
[@@deriving equal]

let empty_scope =
  Map.of_alist_exn
    (module String)
    [("Int257", Builtin "Int257"); ("Bool", Builtin "Bool")]

let rec env_from_program (stx : Syntax.program) =
  let scope = scope_from_bindings stx.bindings in
  Result.bind scope ~f:(fun scope ->
      (* Resolve top-level scope first *)
      let scope = resolve_scope scope in
      (* Resolve references inside *)
      let scope = Result.bind scope ~f:resolve_inner in
      (* Build env *)
      Result.map scope ~f:(fun scope -> {scope}) )

and resolve_inner scope =
  let scope' = Map.to_alist scope in
  let resolve resolved (k, v) =
    match v with
    | Struct s ->
        let fields = Map.to_alist s.struct_fields in
        let resolved =
          List.fold_result ~init:(s, resolved)
            ~f:(fun (s, resolved) (field_name, field) ->
              match field with
              | {field_type = Reference ref; _} -> (
                match Map.find resolved ref with
                | Some (Resolved_Reference (_, t)) | Some t ->
                    let s' =
                      { s with
                        struct_fields =
                          Map.set s.struct_fields ~key:field_name
                            ~data:{field_type = Resolved_Reference (ref, t)} }
                    in
                    Ok (s', Map.set resolved ~key:k ~data:(Struct s'))
                | None ->
                    Error (Unresolved ref) )
              | _ ->
                  Ok (s, resolved) )
            fields
        in
        Result.map resolved ~f:(fun (_, resolved) -> resolved)
    | _ ->
        Ok resolved
  in
  List.fold_result ~init:scope ~f:resolve scope'

and resolve_scope scope =
  let scope' = Map.to_alist scope in
  let rec resolve resolved (k, v) =
    match v with
    | Reference ref -> (
      match Map.find resolved ref with
      | None ->
          Error (Unresolved ref)
      | Some (Reference ref') ->
          resolve resolved (k, Reference ref')
      | Some value ->
          Ok (Map.set resolved ~key:k ~data:value) )
    | _ ->
        Ok resolved
  in
  List.fold_result ~init:scope ~f:resolve scope'

and scope_from_bindings bindings =
  let scope = empty_scope in
  List.fold_result ~init:scope
    ~f:(fun scope binding ->
      let binding = binding.value in
      let ident = Syntax.ident_to_string binding.binding_name.value in
      match Map.find scope ident with
      | None ->
          let data = binding_to_value binding in
          Result.map data ~f:(fun data -> Map.set scope ~key:ident ~data)
      | Some existing ->
          Error (Duplicate_Identifier (ident, existing)) )
    bindings

and binding_to_value binding =
  expr_to_value binding.binding_expr.value binding.binding_expr.loc

and expr_to_value expr loc =
  match expr with
  | Struct s ->
      struct_to_struct s loc
  | Int i ->
      Ok (Integer i)
  | Reference ref ->
      Ok (Reference (Syntax.ident_to_string ref))
  | _ ->
      Error Unsupported

and struct_to_struct s loc =
  let s' =
    { struct_loc = loc;
      struct_fields = Map.empty (module String);
      struct_methods = Map.empty (module String) }
  in
  let s =
    List.fold_result ~init:s'
      ~f:(fun s' field ->
        let ident = Syntax.ident_to_string field.value.field_name.value in
        match Map.find s'.struct_fields ident with
        | Some _ ->
            Error (Duplicate_Field (ident, s'))
        | None ->
            let value =
              expr_to_value field.value.field_type.value
                field.value.field_type.loc
            in
            Result.map value ~f:(fun value ->
                { s' with
                  struct_fields =
                    Map.set s'.struct_fields ~key:ident
                      ~data:{field_type = value} } ) )
      s.fields
  in
  Result.map s ~f:(fun s -> Struct s)

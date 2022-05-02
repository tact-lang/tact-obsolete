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

and type_ =
  { type_loc : Syntax.loc;
    type_fields : type_field named_map;
    type_methods : function_ named_map }

and type_field = {field_type : value}

and env = {scope : scope}

and scope = value named_map

and error =
  | Duplicate_Identifier of string * value
  | Duplicate_Field of string * type_
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
              | {field_type = Reference ref; _} -> (
                match Map.find resolved ref with
                | Some (Resolved_Reference (_, t)) | Some t ->
                    let s' =
                      { s with
                        type_fields =
                          Map.set s.type_fields ~key:field_name
                            ~data:{field_type = Resolved_Reference (ref, t)} }
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
  | _ ->
      Error Unsupported

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
            match value with
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

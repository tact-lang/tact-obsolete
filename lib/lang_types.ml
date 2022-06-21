open Base

class ['s] base_map =
  object (_ : 's)
    inherit ['s] Zint.map

    inherit ['s] Asm.map
  end

class virtual ['s] base_reduce =
  object (_ : 's)
    method virtual visit_instr : _

    method virtual visit_z : _
  end

class virtual ['s] base_visitor =
  object (_ : 's)
    inherit ['s] VisitorsRuntime.map

    inherit ['s] Zint.map

    inherit ['s] Asm.map
  end

type comptime_counter = (int[@sexp.opaque])

and metadata = (string * string) list

and binding = string * expr

and tbinding = string * binding_scope

and binding_scope = Comptime of expr | Runtime of type_

and program =
  { bindings : (string * expr) list;
    mutable structs : (int * struct_) list;
    mutable unions : (int * union) list; [@sexp.list]
    mutable struct_counter : (int[@sexp.opaque]);
    mutable memoized_fcalls : (((value * value list) * value) list[@sexp.opaque])
  }

and expr =
  | FunctionCall of function_call
  | MkStructDef of mk_struct
  | MkUnionDef of mk_union
  | MkFunction of function_
  | MakeUnionVariant of (expr * int)
  | Reference of (string * type_)
  | ResolvedReference of (string * (expr[@sexp.opaque]))
  | Value of value
  | StructField of (expr * string * type_)
  | Hole
  | Primitive of primitive
  | InvalidExpr

and interface = {interface_methods : (string * function_signature) list}

and if_ = {if_condition : expr; if_then : stmt; if_else : stmt option}

and value =
  | Void
  | Struct of (int * (string * expr) list)
  | UnionVariant of (value * int)
  | Function of function_
  | Integer of (Zint.t[@visitors.name "z"])
  | Bool of bool
  | String of string
  | Builtin of builtin
  | Type of type_

and stmt =
  | If of if_
  | Let of (string * expr) list
  | Return of expr
  | Break of stmt
  | Expr of expr
  | Block of stmt list
  | Switch of switch
  | Invalid

and builtin = string

and type_ =
  | TypeN of int
  | IntegerType
  | BoolType
  | StringType
  | VoidType
  | BuiltinType of builtin
  | StructType of int
  | UnionType of int
  | FunctionType of function_signature
  | InterfaceType of interface
  | HoleType
  | SelfType
  | InvalidType of expr
  | ExprType of expr
  | Dependent of string * type_

and mk_union =
  { mk_cases : expr list;
    mk_union_methods : (string * expr) list;
    mk_union_impls : impl list; [@sexp.list]
    mk_union_id : int }

and union =
  { cases : (type_ * discriminator) list;
    union_methods : (string * function_) list;
    union_impls : impl list; [@sexp.list]
    union_id : int }

and mk_struct =
  { mk_struct_fields : (string * expr) list;
    mk_methods : (string * expr) list;
    mk_impls : impl list;
    mk_struct_id : int }

and struct_ =
  { struct_fields : (string * struct_field) list;
    struct_methods : (string * function_) list;
    struct_impls : impl list;
    struct_id : int }

and discriminator = Discriminator of int

and struct_field = {field_type : type_}

and impl = {impl_interface : expr; impl_methods : binding list}

and function_body = (stmt option[@sexp.option])

and native_function =
  (program -> value list -> value[@visitors.opaque] [@equal.ignore])

and builtin_fn = native_function * (int[@sexp.opaque])

and function_ =
  {function_signature : function_signature; function_impl : function_impl}

and function_signature =
  {function_params : (string * type_) list; function_returns : type_}

and function_impl = Fn of function_body | BuiltinFn of builtin_fn | InvalidFn

and function_call = expr * expr list

and switch = {switch_condition : expr; branches : branch list}

and branch = {branch_ty : type_; branch_var : string; branch_stmt : stmt}

and primitive =
  | EmptyBuilder
  | StoreInt of {builder : expr; length : expr; integer : expr; signed : bool}
  | BuildCell of {builder : expr}
[@@deriving
  equal,
    sexp_of,
    visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]},
    visitors {variety = "reduce"; ancestors = ["base_reduce"]},
    visitors {variety = "fold"; name = "visitor"; ancestors = ["base_visitor"]}]

let type0 = TypeN 0

let make_runtime (x, type_) = (x, Runtime type_)

let make_comptime (x, value) = (x, Comptime value)

let find_comptime name bindings =
  List.find_map bindings ~f:(fun bindings ->
      List.find_map bindings ~f:(function
        | b_name, Comptime value ->
            if equal_string b_name name then Some (Ok value) else None
        | b_name, Runtime _ ->
            if equal_string b_name name then Some (Error ()) else None ) )

let extract_comptime_bindings bindings =
  List.filter_map bindings ~f:(fun (name, scope) ->
      match scope with Comptime value -> Some (name, value) | _ -> None )

let rec expr_to_type = function
  | Value (Type type_) ->
      type_
  | FunctionCall
      ( ResolvedReference
          (_, Value (Function {function_signature = {function_returns; _}; _})),
        _ )
  | FunctionCall
      (Value (Function {function_signature = {function_returns; _}; _}), _) ->
      function_returns
  | Reference (ref, ty) ->
      ExprType (Reference (ref, ty))
  | ResolvedReference (_, e) ->
      expr_to_type e
  | expr ->
      ExprType expr

let rec type_of = function
  | Value (Struct (sid, _)) ->
      StructType sid
  | Value (Function {function_signature; _}) ->
      FunctionType function_signature
  | Value (Builtin builtin) ->
      BuiltinType builtin
  | Value (Integer _) ->
      IntegerType
  | Value (Bool _) ->
      BoolType
  | Value Void ->
      VoidType
  | Value (Type (Dependent (_, ty))) ->
      unwrap_type_ @@ ty
  | Value (Type t) ->
      type_of_type t
  | Hole ->
      HoleType
  | FunctionCall
      ( ResolvedReference
          ( _,
            Value
              (Function
                {function_signature = {function_returns; function_params}; _} )
          ),
        args )
  | FunctionCall
      ( Value
          (Function
            {function_signature = {function_returns; function_params}; _} ),
        args ) ->
      unwrap_type_ @@ type_of_call args function_params function_returns
  | Reference (_, t) ->
      unwrap_type_ t
  | ResolvedReference (_, e) ->
      unwrap_type_ @@ type_of e
  | MakeUnionVariant (_, u) ->
      UnionType u
  | MkStructDef _ ->
      type0
  | StructField (_, _, ty) ->
      unwrap_type_ ty
  | expr ->
      InvalidType expr

and unwrap_type_ = function
  | ExprType (Value (Type ty)) ->
      unwrap_type_ ty
  | ExprType (ResolvedReference (_, Value (Type t))) ->
      unwrap_type_ t
  | type_ ->
      type_

and type_of_type = function TypeN x -> TypeN (x + 1) | _otherwise -> TypeN 0

and type_of_call args arg_types returns =
  let associated =
    match List.map2 args arg_types ~f:(fun expr (name, _) -> (name, expr)) with
    | Ok t ->
        t
    | _ ->
        raise Errors.InternalCompilerError
  in
  let dependent_types_monomophizer (associated : (string * expr) list) =
    object (_self : _)
      inherit [_] map

      method! visit_Dependent _ ref _ =
        List.find_map associated ~f:(fun (name, x) ->
            if equal_string name ref then
              Some
                ( match x with
                (* If we depend on reference, it means we depend on function argument,
                   so type must be dependent. *)
                | Reference (r, t) ->
                    Dependent (r, t)
                | x ->
                    type_of x )
            else None )
        |> Option.value_exn
    end
  in
  let monomorphizer = dependent_types_monomophizer associated in
  monomorphizer#visit_type_ () returns

class ['s] boolean_reduce (zero : bool) =
  object (_self : 's)
    inherit [_] reduce

    method private zero = zero

    method private plus = if zero then ( && ) else ( || )

    method visit_instr _env _instr = zero

    method visit_z _env _z = zero
  end

class ['s] primitive_presence =
  object (_self : 's)
    inherit [_] boolean_reduce false

    method! visit_Primitive _env _primitive = true

    method! visit_mk_struct _ _ = false
  end

let has_primitives = (new primitive_presence)#visit_function_ ()

class ['s] expr_immediacy_check =
  object (self : 's)
    inherit [_] boolean_reduce true as super

    val mutable in_function_call = 0

    method! visit_Reference _env _ref = false

    method! visit_Hole _env = false

    method! visit_InvalidExpr _env = false

    method! visit_Primitive _env _primitive = false

    method! visit_function_call env (f, args) =
      match f with
      | Value (Function {function_impl = BuiltinFn _; _}) ->
          true
      | _ ->
          in_function_call <- in_function_call + 1 ;
          let result = super#visit_function_call env (f, args) in
          in_function_call <- in_function_call - 1 ;
          result

    method! visit_function_ env f =
      self#plus
        ( if in_function_call > 0 then
          (* If we're calling this function, check if there are no primitives *)
          not @@ has_primitives f
        else
          (* Any function is assumed to be immediate as it can be evaluated otherwise *)
          true )
        (super#visit_function_signature env f.function_signature)

    method! visit_mk_struct _ _ = true
  end

let rec is_immediate_expr expr =
  let checker = new expr_immediacy_check in
  checker#visit_expr () expr

and are_immediate_arguments args =
  Option.is_none (List.find args ~f:(fun a -> not (is_immediate_expr a)))

let rec builtin_fun_counter = ref 0

and builtin_fun f =
  let res = (f, !builtin_fun_counter) in
  builtin_fun_counter := !builtin_fun_counter + 1 ;
  res

let find_in_scope : string -> tbinding list list -> binding_scope option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      List.find_map bindings ~f:(fun (s, x) ->
          if String.equal ref s then Some x else None ) )

let find_in_runtime_scope : 'a. string -> (string * 'a) list list -> 'a option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      List.find_map bindings ~f:(fun (name, value) ->
          if String.equal ref name then Some value else None ) )

let print_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

module Program = struct
  let methods_of p = function
    | StructType s ->
        List.find_map_exn p.structs ~f:(fun (id, s') ->
            if equal_int id s then Some s'.struct_methods else None )
    | _ ->
        []

  let get_struct p s = List.Assoc.find_exn p.structs s ~equal:equal_int

  let rec update_list id new_s = function
    | [] ->
        raise Errors.InternalCompilerError
    | (xid, old_s) :: xs ->
        if equal_int xid id then
          match new_s with Ok new_s -> (id, new_s) :: xs | Error _ -> xs
        else (xid, old_s) :: update_list id new_s xs

  let with_struct p s f =
    p.structs <- (s.struct_id, s) :: p.structs ;
    let new_s = f () in
    p.structs <- update_list s.struct_id new_s p.structs ;
    new_s

  (* Creates new struct id, calls function with this new id and then
     places returning struct to the program.structs *)
  let with_id p f =
    let id = p.struct_counter in
    p.struct_counter <- p.struct_counter + 1 ;
    let new_s = f id in
    p.structs <- (id, new_s) :: p.structs ;
    new_s

  let with_union p u f =
    p.unions <- (u.union_id, u) :: p.unions ;
    let new_u = f () in
    p.unions <- update_list u.union_id new_u p.unions ;
    new_u

  (* Creates new struct id, calls function with this new id and then
     places returning union to the program.unions *)
  let with_union_id p mk_union f =
    let id = p.struct_counter in
    p.struct_counter <- p.struct_counter + 1 ;
    let u = mk_union id in
    p.unions <- (u.union_id, u) :: p.unions ;
    let new_union = f u in
    p.unions <- update_list id new_union p.unions ;
    new_union
end

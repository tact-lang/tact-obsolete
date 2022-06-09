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

and program = {bindings : (string * expr) list}

and expr =
  | FunctionCall of function_call
  | MakeUnionVariant of (expr * union)
  | Reference of (string * type_)
  | ResolvedReference of (string * (expr[@sexp.opaque]))
  | Value of value
  | StructField of (expr * string)
  | Hole
  | Primitive of primitive
  | InvalidExpr

and interface = {interface_methods : (string * function_signature) list}

and if_ = {if_condition : expr; if_then : stmt; if_else : stmt option}

and value =
  | Void
  | Struct of (struct_ * (string * expr) list)
  | UnionVariant of (value * union)
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
  | Invalid

and builtin = string

and type_ =
  | TypeN of int
  | IntegerType
  | BoolType
  | StringType
  | VoidType
  | BuiltinType of builtin
  | StructType of struct_
  | UnionType of union
  | FunctionType of function_signature
  | InterfaceType of interface
  | HoleType
  | SelfType
  | InvalidType of expr
  | ExprType of expr
  | Dependent of string * type_

and union = {cases : type_ list}

and struct_ =
  { struct_fields : (string * struct_field) list;
    struct_methods : (string * function_) list;
    struct_impls : impl list;
    struct_id : int }

and struct_field = {field_type : type_}

and impl = {impl_interface : expr; impl_methods : binding list}

and function_body = (stmt option[@sexp.option])

and native_function =
  (program -> value list -> value[@visitors.opaque] [@equal.ignore])

and builtin_fn = native_function * int

and function_ =
  {function_signature : function_signature; function_impl : function_impl}

and function_signature =
  {function_params : (string * type_) list; function_returns : type_}

and function_impl = Fn of function_body | BuiltinFn of builtin_fn | InvalidFn

and function_call = expr * expr list

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
      InvalidType expr

let rec type_of = function
  | Value (Struct (struct_, _)) ->
      StructType struct_
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
      ty
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
      type_of_call args function_params function_returns
  | Reference (_, t) ->
      t
  | ResolvedReference (_, e) ->
      type_of e
  | MakeUnionVariant (_, u) ->
      UnionType u
  | expr ->
      InvalidType expr

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

    method! visit_type_ _env _struct = false
  end

let has_primitives = (new primitive_presence)#visit_function_ ()

class ['s] expr_immediacy_check =
  object (_self : 's)
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

    method! visit_function_ _env f =
      if in_function_call > 0 then
        (* If we're calling this function, check if there are no primitives *)
        not @@ has_primitives f
      else
        (* Any function is assumed to be immediate as it can be evaluated otherwise *)
        true

    method! visit_struct_ _env _struct = true
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

(* We declare the struct counter here to count all structs *)
let struct_counter = ref 0

let methods_of = function StructType s -> s.struct_methods | _ -> []

let impls_of = function StructType s -> s.struct_impls | _ -> []

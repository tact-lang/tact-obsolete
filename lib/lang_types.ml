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

and tbinding = {tbinding : binding; binding_scope : binding_scope}

and binding_scope = Comptime | Runtime

and program =
  { bindings : (string * expr) list;
    mutable methods : (value * (string * function_) list) list; [@sexp.list]
    mutable impls : (value * impl list) list [@sexp.list] }

and expr =
  | FunctionCall of function_call
  (* Note that it is TYPE EXPR, means that `string` has `expr` type, not `string` references to `type` *)
  (* FIXME: make new type *)
  | Reference of (string * expr)
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
  | TypeType
  | IntegerType
  | BoolType
  | StringType
  | VoidType
  | BuiltinType of builtin
  | StructType of struct_
  | FunctionType of function_signature
  | InterfaceType of interface
  | HoleType
  | SelfType
  | InvalidType of expr

and struct_ =
  {struct_fields : (string * struct_field) list; struct_id : (int[@sexp.opaque])}

and struct_field = {field_type : expr}

and impl = {impl_interface : expr; impl_methods : binding list}

and function_body = (stmt option[@sexp.option])

and native_function =
  (program -> value list -> value[@visitors.opaque] [@equal.ignore])

and builtin_fn = native_function * (int[@sexp.opaque])

and function_ =
  {function_signature : function_signature; function_impl : function_impl}

and function_signature =
  {function_params : (string * expr) list; function_returns : expr}

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

let make_runtime binding = {tbinding = binding; binding_scope = Runtime}

let make_comptime binding = {tbinding = binding; binding_scope = Runtime}

let find_comptime name bindings =
  List.find_map bindings ~f:(fun bindings ->
      List.find_map bindings ~f:(function
        | {tbinding = b_name, value; binding_scope = Comptime} ->
            if equal_string b_name name then Some (Ok value) else None
        | {tbinding = b_name, _; binding_scope = Runtime} ->
            if equal_string b_name name then Some (Error ()) else None ) )

let extract_comptime_bindings bindings =
  List.filter_map bindings ~f:(fun {tbinding; binding_scope} ->
      match binding_scope with Comptime -> Some tbinding | _ -> None )

let rec expr_to_type = function
  | Value (Type type_) ->
      type_
  | FunctionCall
      ( ResolvedReference
          (_, Value (Function {function_signature = {function_returns; _}; _})),
        _ )
  | FunctionCall
      (Value (Function {function_signature = {function_returns; _}; _}), _) ->
      expr_to_type function_returns
  | Reference (_, t) ->
      expr_to_type t
  | ResolvedReference (_, e) ->
      expr_to_type e
  | expr ->
      InvalidType expr

let rec type_of = function
  | Value (Struct (struct_, _)) ->
      Value (Type (StructType struct_))
  | Value (Function {function_signature; _}) ->
      Value (Type (FunctionType function_signature))
  | Value (Builtin builtin) ->
      Value (Type (BuiltinType builtin))
  | Value (Integer _) ->
      Value (Type IntegerType)
  | Value (Bool _) ->
      Value (Type BoolType)
  | Value Void ->
      Value (Type VoidType)
  | Value (Type _) ->
      Value (Type TypeType)
  | Hole ->
      Value (Type HoleType)
  | FunctionCall
      ( ResolvedReference
          (_, Value (Function {function_signature = {function_returns; _}; _})),
        _ )
  | FunctionCall
      (Value (Function {function_signature = {function_returns; _}; _}), _) ->
      function_returns
  | Reference (_, t) ->
      t
  | ResolvedReference (_, e) ->
      type_of e
  | expr ->
      Value (Type (InvalidType expr))

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

let find_in_scope : string -> tbinding list list -> tbinding option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      List.find bindings ~f:(fun {tbinding = s, _; _} -> String.equal ref s) )

let find_in_runtime_scope : 'a. string -> (string * 'a) list list -> 'a option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      List.find_map bindings ~f:(fun (name, value) ->
          if String.equal ref name then Some value else None ) )

(* We declare the struct counter here to count all structs *)
let struct_counter = ref 0

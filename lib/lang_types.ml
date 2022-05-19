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

and program =
  { stmts : stmt list; [@sexp.list]
    bindings : (string * expr) list;
    mutable methods : (value * (string * function_) list) list [@sexp.list] }

and expr =
  | FunctionCall of function_call
  | Reference of (string * type_)
  | Value of value
  | StructField of (expr * string)
  | Hole
  | Primitive of primitive
  | InvalidExpr

and value =
  | Void
  | Struct of struct_
  (* Instance of a Struct *)
  | StructInstance of (struct_ * (string * value) list)
  | Function of function_
  | Integer of (Zint.t[@visitors.name "z"])
  | String of string
  | Builtin of builtin
  | Type of type_

and stmt =
  | Let of (string * expr) list
  | Return of expr
  | Break of stmt
  | Expr of expr
  | Invalid

and builtin = string

and type_ =
  | TypeType
  | IntegerType
  | StringType
  | VoidType
  | BuiltinType of builtin
  | StructType of struct_
  | FunctionType of function_signature
  | HoleType
  | InvalidType

and struct_ =
  {struct_fields : (string * struct_field) list; struct_id : (int[@sexp.opaque])}

and struct_field = {field_type : expr}

and function_body = (stmt list option[@sexp.option])

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
    yojson_of,
    visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]},
    visitors {variety = "reduce"; ancestors = ["base_reduce"]},
    visitors {variety = "fold"; name = "visitor"; ancestors = ["base_visitor"]}]

let rec expr_to_type = function
  | Value (Struct _) ->
      TypeType
  | Value (StructInstance (struct_, _)) ->
      StructType struct_
  | Value (Function {function_signature; _}) ->
      FunctionType function_signature
  | Value (Builtin builtin) ->
      BuiltinType builtin
  | Value (Integer _) ->
      IntegerType
  | Value Void ->
      VoidType
  | Value (Type type_) ->
      type_
  | Hole ->
      HoleType
  | FunctionCall
      (Value (Function {function_signature = {function_returns; _}; _}), _) ->
      expr_to_type function_returns
  | Reference (_, t) ->
      t
  | _ ->
      InvalidType

class ['s] primitive_presence =
  object (_self : 's)
    inherit [_] reduce

    method private zero = false

    method private plus = ( || )

    method! visit_Primitive _env _primitive = true

    method visit_instr _env _instr = false

    method visit_z _env _z = false
  end

class ['s] expr_immediacy_check =
  object (_self : 's)
    inherit [_] reduce as super

    val mutable in_function_call = 0

    method private zero = true

    method private plus = ( && )

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
        not @@ (new primitive_presence)#visit_function_ () f
      else
        (* Any function is assumed to be immediate as it can be evaluated otherwise *)
        true

    method visit_instr _env _instr = false

    method visit_z _env _z = true
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

let find_in_scope : 'a. string -> (string * 'a) list list -> 'a option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      Option.map
        (List.find bindings ~f:(fun (s, _) -> String.equal ref s))
        ~f:(fun (_name, a) -> a) )

(* We declare the struct counter here to count all structs *)
let struct_counter = ref 0

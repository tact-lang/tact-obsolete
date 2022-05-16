(* Interpreted Representation *)
open Base

class ['s] base_map =
  object (_ : 's)
    inherit ['s] Zint.map

    inherit ['s] Asm.map
  end

class virtual ['s] base_visitor =
  object (_ : 's)
    inherit ['s] VisitorsRuntime.map

    inherit ['s] Zint.map

    inherit ['s] Asm.map
  end

type binding = string * expr

and program = {bindings : (string * expr) list}

and expr =
  | FunctionCall of function_call
  | Reference of (string * type_)
  | Value of value
  | Asm of Asm.instr list

and value =
  | Void
  | Struct of struct_
  (* Instance of a Struct *)
  | StructInstance of (struct_ * (string * value) list)
  | Function of function_
  | Integer of (Zint.t[@visitors.name "z"])
  | Builtin of builtin

and builtin = string

and stmt =
  | Let of let_bindings
  | Return of expr
  | Break of stmt
  | Expr of expr
  | Invalid

and let_bindings = (string * value) list

and type_ =
  | TypeType
  | IntegerType
  | VoidType
  | BuiltinType of builtin
  | StructType of struct_
  | FunctionType of function_
  | HoleType
  | InvalidType

and struct_ =
  { struct_fields : (string * struct_field) list;
    struct_methods : (string * function_) list }

and struct_field = {field_type : type_}

and function_body = (stmt list option[@sexp.option])

and function_ =
  { function_params : (string * type_) list;
    function_returns : type_;
    function_impl : function_body }

and function_call = value * value list
[@@deriving
  equal,
    sexp_of,
    yojson_of,
    visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]},
    visitors {variety = "fold"; name = "visitor"; ancestors = ["base_visitor"]}]

open Errors

(* Raised when constructor sees something it shouldn't *)
exception Invalid

let expr_to_type (e : Lang_types.expr) =
  match e with Value (Type t) -> t | _ -> raise Invalid

let expr_to_value = function Value v -> v | _ -> raise Invalid

class ['s] constructor ((_program, _errors) : Lang_types.program * _ errors) =
  object (s : 's)
    inherit ['s] Lang_types.visitor

    method build_Asm _env asm = Asm asm

    method build_Break _env e = Break e

    method build_Builtin _env b = Builtin b

    method build_BuiltinFn _env _b = raise Invalid

    method build_BuiltinType _env b = BuiltinType b

    method build_Expr _env expr = Expr expr

    method build_Fn _env fn = fn

    method build_Function _env f = Function f

    method build_FunctionCall _env (f, args) =
      FunctionCall (expr_to_value f, List.map args ~f:expr_to_value)

    method build_FunctionType _env ft = FunctionType ft

    method build_Hole _env = raise Invalid

    method build_HoleType _env = raise Invalid

    method build_Integer _env i = Integer i

    method build_IntegerType _env = IntegerType

    method build_Invalid _env = raise Invalid

    method build_InvalidExpr _env = raise Invalid

    method build_InvalidFn _env = raise Invalid

    method build_InvalidType _env = raise Invalid

    method build_Let _env bindings =
      Let (List.map bindings ~f:(fun (name, expr) -> (name, expr_to_value expr)))

    method build_Reference _env (name, t) = Reference (name, t)

    method build_Return _env expr = Return expr

    method build_String _env _s = raise Invalid

    method build_StringType _env = raise Invalid

    method build_Struct _env s = Struct s

    method build_StructInstance _env si = StructInstance si

    method build_StructType _env t = StructType t

    method build_Type _env _t = raise Invalid

    method build_TypeType _env = TypeType

    method build_Value _env v = Value v

    method build_Void _env = Void

    method build_VoidType _env = VoidType

    method visit_expr_as_type env (e : Lang_types.expr) =
      match e with Value (Type t) -> s#visit_type_ env t | _ -> raise Invalid

    method build_function'_ _env function_params function_returns function_impl
        =
      {function_params; function_returns; function_impl}

    method build_function_ _env _ _ _ = raise Invalid

    method! visit_function_ env f =
      let _visitors_r0 =
        s#visit_list
          (fun env (_visitors_c0, _visitors_c1) ->
            let _visitors_r0 = s#visit_string env _visitors_c0 in
            let _visitors_r1 = s#visit_expr_as_type env _visitors_c1 in
            (_visitors_r0, _visitors_r1) )
          env f.function_params
      in
      let _visitors_r1 = s#visit_expr_as_type env f.function_returns in
      let _visitors_r2 = s#visit_function_impl env f.function_impl in
      s#build_function'_ env _visitors_r0 _visitors_r1 _visitors_r2

    method build_program _env _p = raise Invalid

    method build_struct_ _env fields methods _ =
      {struct_fields = fields; struct_methods = methods}

    method build_struct_field _env _ = raise Invalid

    method! visit_struct_field env sf =
      let _visitors_r0 = s#visit_expr_as_type env sf.field_type in
      {field_type = _visitors_r0}

    method! visit_program env p =
      {bindings = s#visit_list s#visit_binding env p.bindings}
  end

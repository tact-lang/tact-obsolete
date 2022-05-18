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

type struct_modifiers = {is_builtin_int : bool; int_bits : int}
[@@deriving equal, sexp_of, yojson_of]

let default_modifiers = {is_builtin_int = false; int_bits = 0}

type comptime_counter = (int[@sexp.opaque])

and binding = string * expr

and program = {bindings : (string * expr) list}

and expr =
  | FunctionCall of function_call
  | Reference of (string * type_)
  | Value of value
  | Asm of Asm.instr list
  | Hole
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
  | Let of let_bindings
  | Return of expr
  | Break of stmt
  | Expr of expr
  | Invalid

and let_bindings = (string * expr) list

and builtin = string

and type_ =
  | TypeType
  | IntegerType
  | StringType
  | VoidType
  | BuiltinType of builtin
  | StructType of struct_
  | FunctionType of function_
  | HoleType
  | InvalidType

and struct_ =
  { struct_modifiers : struct_modifiers; [@visitors.opaque]
    struct_fields : (string * struct_field) list;
    struct_methods : (string * function_) list;
    struct_id : (int * int[@sexp.opaque]) }

and struct_field = {field_type : expr}

and function_body = (stmt list option[@sexp.option])

and native_function = (value list -> expr[@visitors.opaque] [@equal.ignore])

and builtin_fn = native_function * int

and function_ =
  { function_params : function_params;
    function_returns : expr;
    function_impl : function_impl }

and function_params = (string * expr) list

and function_impl = Fn of function_body | BuiltinFn of builtin_fn | InvalidFn

and function_call = expr * expr list
[@@deriving
  equal,
    sexp_of,
    yojson_of,
    visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]},
    visitors {variety = "fold"; name = "visitor"; ancestors = ["base_visitor"]}]

let rec expr_to_type = function
  | Value (Struct _) ->
      TypeType
  | Value (StructInstance (struct_, _)) ->
      StructType struct_
  | Value (Function function_) ->
      FunctionType function_
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
  | FunctionCall (Value (Function {function_returns; _}), _) ->
      expr_to_type function_returns
  | Reference (_, t) ->
      t
  | _ ->
      InvalidType

let real_expr_to_type = function
  | Value (Struct s) ->
      StructType s
  | _ ->
      InvalidType

(*FIXME: iter or reduce visitor*)
class ['s] immediate_check =
  object (_s : 's)
    inherit ['s] map as super

    val mutable is_immediate = true

    method! visit_Reference env ref =
      is_immediate <- false ;
      super#visit_Reference env ref

    method! visit_Asm env asm =
      is_immediate <- false ;
      super#visit_Asm env asm

    method! visit_Hole env =
      is_immediate <- false ;
      super#visit_Hole env

    method! visit_InvalidExpr env =
      is_immediate <- false ;
      super#visit_InvalidExpr env

    method! visit_function_ _env f = f

    method get_is_immediate = is_immediate
  end

let rec is_immediate_expr expr =
  let checker = new immediate_check in
  let _expr = checker#visit_expr () expr in
  checker#get_is_immediate

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

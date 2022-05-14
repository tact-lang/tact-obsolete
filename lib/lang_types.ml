open Base

class ['s] base_map =
  object (_ : 's)
    inherit ['s] Zint.map
  end

type 'a named_map = (string * 'a) list

and comptime_counter = (int[@sexp.opaque])

and binding = string * expr

and program = {stmts : stmt list; [@sexp.list] bindings : expr named_map}

and expr =
  | FunctionCall of (function_call * (value option[@sexp.option]) ref)
    (* expr option is cached result *)
  | Reference of (string * type_)
  | Value of value
  | Hole
  | InvalidExpr

and value =
  | Void
  | Struct of struct_
  (* Instance of a Struct *)
  | StructInstance of (struct_ * value named_map)
  | Function of function_
  | Integer of (Zint.t[@visitors.name "z"])
  | Builtin of builtin
  | Type of type_

and stmt =
  | Let of expr named_map
  | Return of expr
  | Break of stmt
  | Expr of expr
  | Invalid

and builtin = string

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
  { struct_fields : struct_field named_map;
    struct_methods : function_ named_map;
    struct_id : (int * int[@sexp.opaque]) }

and struct_field = {field_type : expr}

and 'a typed_fn =
  {function_params : expr named_map; function_returns : expr; function_impl : 'a}

and function_body = (stmt list option[@sexp.option])

and fn = function_body typed_fn

and native_function =
  (program -> value list -> value[@visitors.opaque] [@equal.ignore])

and builtin_fn = (native_function * comptime_counter) typed_fn

and function_ = Fn of fn | BuiltinFn of builtin_fn | InvalidFn

and function_call = expr * expr list
[@@deriving
  equal,
    sexp_of,
    visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]}]

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
  | FunctionCall ((Value (Function (Fn {function_returns; _})), _), _)
  | FunctionCall ((Value (Function (BuiltinFn {function_returns; _})), _), _) ->
      expr_to_type function_returns
  | Reference (_, t) ->
      t
  | _ ->
      InvalidType

let rec is_immediate_expr = function
  | Value _ ->
      true
  | FunctionCall ((_, args), _) ->
      are_immediate_arguments args
  | Hole ->
      false
  | Reference _ ->
      false
  | InvalidExpr ->
      false

and are_immediate_arguments args =
  Option.is_none (List.find args ~f:(fun a -> not (is_immediate_expr a)))

let rec builtin_fun_counter = ref 0

and builtin_fun f =
  let res = (f, !builtin_fun_counter) in
  builtin_fun_counter := !builtin_fun_counter + 1 ;
  res

let find_in_scope : 'a. string -> 'a named_map list -> 'a option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      Option.map
        (List.find bindings ~f:(fun (s, _) -> String.equal ref s))
        ~f:(fun (_name, a) -> a) )

(* We declare the struct counter here to count all structs *)
let struct_counter = ref 0

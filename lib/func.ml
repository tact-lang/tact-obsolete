open Base

exception Unsupported

(* Subset of FunC used by FunC codegen *)

type function_ =
  { function_name : ident;
    function_args : (ident * type_) list;
    function_returns : type_;
    function_body : function_body }

and function_body =
  | AsmFn of Asm.instr list [@sexp.list]
  | Fn of stmt list [@sexp.list]

and stmt = Vars of (type_ * ident * expr) list | Return of expr

and expr =
  | Integer of Zint.t
  | Reference of (ident * type_)
  | Tuple of expr list
  | FunctionCall of (ident * expr list)

and ident = string

and type_ =
  | IntType
  | CellType
  | SliceType
  | BuilderType
  | TupleType of type_ list
  | TensorType of type_ list
  | FunctionType of function_
  | ContType

and top_level_expr = Function of function_ | Global of type_ * ident

and program = top_level_expr list [@@deriving sexp_of]

exception UnknownType

let rec type_of = function
  | Integer _ ->
      IntType
  | Reference (_, ty) ->
      ty
  | Tuple exprs ->
      TupleType (List.map exprs ~f:type_of)
  | FunctionCall _ ->
      raise UnknownType

open Caml.Format

let list_iter ~f ~flast l =
  match (List.drop_last l, List.last l) with
  | Some rest, Some last ->
      List.iter rest ~f ; flast last
  | _ ->
      ()

let indentation = "  "

let rec pp_program f program =
  let prev_margin = pp_get_margin f () in
  let prev_indent = pp_get_max_indent f () in
  pp_set_margin f 80 ;
  pp_set_max_indent f 40 ;
  List.iter program ~f:(function
    | Function fn ->
        pp_function f fn ; pp_print_newline f ()
    | Global _ ->
        () ) ;
  pp_set_margin f prev_margin ;
  pp_set_max_indent f prev_indent

and pp_function f fn =
  pp_open_box f 4 ;
  pp_type f fn.function_returns ;
  pp_print_space f () ;
  pp_ident f fn.function_name ;
  pp_print_string f "(" ;
  list_iter fn.function_args
    ~f:(fun (name, t) ->
      pp_type f t ;
      pp_print_space f () ;
      pp_ident f name ;
      pp_print_string f ", " )
    ~flast:(fun (name, t) -> pp_type f t ; pp_print_space f () ; pp_ident f name) ;
  pp_print_string f ")" ;
  pp_print_space f () ;
  pp_print_string f "{" ;
  pp_print_newline f () ;
  pp_print_string f indentation ;
  pp_open_box f 4 ;
  pp_function_body f fn.function_body ;
  pp_close_box f () ;
  pp_print_string f "}" ;
  pp_close_box f ()

and pp_function_body f = function
  | Fn stmts ->
      List.iter stmts ~f:(pp_stmt f)
  | _ ->
      raise Unsupported

and pp_stmt f = function
  | Vars vars ->
      List.iter vars ~f:(fun (t, n, expr) ->
          pp_type f t ;
          pp_print_space f () ;
          pp_ident f n ;
          pp_print_space f () ;
          pp_print_string f "=" ;
          pp_print_space f () ;
          pp_expr f expr ;
          pp_print_string f ";" ;
          pp_print_newline f () )
  | Return expr ->
      pp_print_string f "return" ;
      pp_print_space f () ;
      pp_expr f expr ;
      pp_print_string f ";" ;
      pp_print_newline f ()

and pp_expr f = function
  | Integer i ->
      pp_print_string f (Zint.to_string i)
  | Reference (ref, _) ->
      pp_print_string f ref
  | FunctionCall (name, args) ->
      pp_print_string f name ;
      pp_print_string f "(" ;
      list_iter args
        ~f:(fun t -> pp_expr f t ; pp_print_string f ", ")
        ~flast:(pp_expr f) ;
      pp_print_string f ")"
  | Tuple _ ->
      ()

and pp_type f = function
  | IntType ->
      pp_print_string f "int"
  | CellType ->
      pp_print_string f "cell"
  | SliceType ->
      pp_print_string f "slice"
  | BuilderType ->
      pp_print_string f "builder"
  | ContType ->
      pp_print_string f "cont"
  | TupleType tuple ->
      pp_print_string f "[" ;
      list_iter tuple
        ~f:(fun (t : type_) -> pp_type f t ; pp_print_string f ", ")
        ~flast:(pp_type f) ;
      pp_print_string f "]"
  | TensorType tuple ->
      pp_print_string f "(" ;
      list_iter tuple
        ~f:(fun t -> pp_type f t ; pp_print_string f ", ")
        ~flast:(pp_type f) ;
      pp_print_string f ")"
  | FunctionType _ ->
      raise UnknownType

and pp_ident f i = pp_print_string f i

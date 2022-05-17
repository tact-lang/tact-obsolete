open Base
open Errors

exception Unsupported

exception Invalid

(* FunC codegen defines a subset of funC AST that it uses *)

type function_ =
  { function_name : ident;
    function_args : (ident * type_) list;
    function_returns : type_;
    function_body : function_body }

and function_body =
  | AsmFn of Asm.instr list [@sexp.list]
  | Fn of stmt list [@sexp.list]

and stmt = Vars of (type_ * ident * expr) list | Return of expr

and expr = Integer of Zint.t | Reference of (ident * type_) | AnExpr

and ident = string

and type_ =
  | Int
  | Cell
  | Slice
  | Builder
  | Tuple of type_ list
  | Tensor of type_ list
  | Cont

and top_level_expr = Function of function_ | Global of type_ * ident

and program = top_level_expr list [@@deriving sexp_of]

open Caml.Format

let rec pp_program f program =
  List.iter program ~f:(function
    | Function fn ->
        pp_function f fn
    | Global _ ->
        () )

and pp_function f fn =
  pp_open_box f 80 ;
  pp_type f fn.function_returns ;
  pp_print_space f () ;
  pp_ident f fn.function_name ;
  pp_print_string f "(" ;
  List.iter fn.function_args ~f:(fun (name, t) ->
      pp_type f t ; pp_print_space f () ; pp_ident f name ) ;
  pp_print_string f ")" ;
  pp_function_body f fn.function_body ;
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
  | AnExpr ->
      raise Invalid (* this is temporary for figuring out typing [FIXME] *)

and pp_type f = function
  | Int ->
      pp_print_string f "int"
  | Cell ->
      pp_print_string f "cell"
  | Slice ->
      pp_print_string f "slice"
  | Builder ->
      pp_print_string f "builder"
  | Cont ->
      pp_print_string f "cont"
  | Tuple tuple ->
      pp_print_string f "[" ;
      list_iter tuple
        ~f:(fun t -> pp_type f t ; pp_print_string f ", ")
        ~flast:(pp_type f) ;
      pp_print_string f "]"
  | Tensor tuple ->
      pp_print_string f "(" ;
      list_iter tuple
        ~f:(fun t -> pp_type f t ; pp_print_string f ", ")
        ~flast:(pp_type f) ;
      pp_print_string f ")"

and pp_ident f i = pp_print_string f i

and list_iter ~f ~flast l =
  match (List.drop_last l, List.last l) with
  | Some rest, Some last ->
      List.iter rest ~f ; flast last
  | _ ->
      ()

(* FunC code constructor *)

let rec typeof e = typeof_type @@ Lang_types.expr_to_type e

and typeof_type = function
  | IntegerType ->
      Int
  | VoidType ->
      Tensor []
  | _ ->
      raise Invalid

and typeof_expr = function
  | Integer _ ->
      Int
  | Reference (_, type_) ->
      type_
  | AnExpr ->
      raise Invalid

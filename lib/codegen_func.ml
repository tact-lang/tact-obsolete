open Base

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

and expr =
  | Integer of Zint.t
  | Reference of (ident * type_)
  | FunctionCall of (ident * expr list)

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

let list_iter ~f ~flast l =
  match (List.drop_last l, List.last l) with
  | Some rest, Some last ->
      List.iter rest ~f ; flast last
  | _ ->
      ()

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
  pp_function_body f fn.function_body ;
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
        ~f:(fun (t : type_) -> pp_type f t ; pp_print_string f ", ")
        ~flast:(pp_type f) ;
      pp_print_string f "]"
  | Tensor tuple ->
      pp_print_string f "(" ;
      list_iter tuple
        ~f:(fun t -> pp_type f t ; pp_print_string f ", ")
        ~flast:(pp_type f) ;
      pp_print_string f ")"

and pp_ident f i = pp_print_string f i

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
  | FunctionCall _ ->
      raise Invalid

class ['s] collector =
  object (self : 's)
    inherit ['s] Lang_types.map as super

    val mutable struct_instances = []

    method! visit_struct_ env s =
      let is_immediate =
        let checker = new Lang_types.immediate_check in
        let _ = checker#visit_struct_ () s in
        checker#get_is_immediate
      in
      match is_immediate with
      | true ->
          self#add_struct_ s ; super#visit_struct_ env s
      (* If struct does not flow into runtime so it fields too. *)
      | false ->
          s

    method private add_struct_ struct_ =
      match
        List.find struct_instances ~f:(Lang_types.equal_struct_ struct_)
      with
      | Some _ ->
          ()
      | None ->
          struct_instances <- struct_ :: struct_instances

    method get_structs = struct_instances
  end

exception DoesNotWantToWriteItsOk

type codegen_ctx =
  {serialize_builder_functions : (Lang_types.type_ * string) list}

let get_serialize_method type_ ctx =
  List.find_map ctx.serialize_builder_functions ~f:(fun (ty, fc) ->
      match Lang_types.equal_type_ ty type_ with
      | true ->
          Some fc
      | false ->
          None )

(* TODO: struct name? field types? *)
let generate_struct_name : Lang_types.struct_ -> string =
 fun {struct_id = x, _; _} -> "struct" ^ Printf.sprintf "%d" x

let lang_type_to_type =
  let open Lang_types in
  function IntegerType -> Int | _ -> raise DoesNotWantToWriteItsOk

let rec lang_expr_to_type =
  let open Lang_types in
  function
  | Value value -> (
    match value with
    | Type type_ ->
        lang_type_to_type type_
    | Struct s -> (
      match s.struct_fields with
      | [(_, {field_type})] ->
          lang_expr_to_type field_type
      | _ ->
          raise DoesNotWantToWriteItsOk )
    | value ->
        Sexplib.Sexp.pp_hum Caml.Format.std_formatter
          (Lang_types.sexp_of_value value) ;
        raise DoesNotWantToWriteItsOk )
  | _ ->
      raise DoesNotWantToWriteItsOk

let rec codegen_serialize_function_body_struct_ :
    codegen_ctx ref -> (string * Lang_types.struct_field) list -> _ =
 fun ctx -> function
  | (name, {field_type}) :: xs ->
      let func_name =
        Option.value
          (get_serialize_method (Lang_types.real_expr_to_type field_type) !ctx)
          ~default:"unknown"
      in
      let serialize_this =
        Vars
          [ ( Builder,
              "b",
              FunctionCall
                ( func_name,
                  [ Reference ("b", Builder);
                    Reference (name, lang_expr_to_type field_type) ] ) ) ]
      in
      serialize_this :: codegen_serialize_function_body_struct_ ctx xs
  | [] ->
      []

let codegen_serialize_function_struct_ ctx struct_ =
  let function_name = "serialize_" ^ generate_struct_name struct_ in
  let struct_fields =
    List.map struct_.struct_fields ~f:(fun (name, {field_type}) ->
        (name, lang_expr_to_type field_type) )
  in
  let function_args = ("b", Builder) :: struct_fields in
  let function_returns = Builder in
  let function_body =
    let serialize =
      codegen_serialize_function_body_struct_ ctx struct_.struct_fields
    in
    let ret = Return (Reference ("b", Builder)) in
    Fn (serialize @ [ret])
  in
  {function_name; function_args; function_returns; function_body}

let codegen_serialize_cell_function_struct_ builder_fun_name args =
  let function_name = builder_fun_name ^ "_cell" in
  let function_args = args in
  let function_returns = Cell in
  let function_body =
    let builder_stmt =
      Vars [(Builder, "b", FunctionCall ("new_builder", []))]
    in
    let serialize =
      Vars
        [ ( Builder,
            "b",
            FunctionCall
              ( builder_fun_name,
                Reference ("b", Builder)
                :: List.map args ~f:(fun (name, ty) -> Reference (name, ty)) )
          ) ]
    in
    let ret = Return (FunctionCall ("build", [Reference ("b", Builder)])) in
    Fn (builder_stmt :: serialize :: [ret])
  in
  {function_name; function_args; function_returns; function_body}

let codegen_integer bits =
  let function_name = "serialize_int" ^ Printf.sprintf "%d" bits in
  let function_args = [("b", Builder); ("value", Int)] in
  let function_returns = Cell in
  let function_body =
    let serialize =
      Vars
        [ ( Builder,
            "b",
            FunctionCall
              ( "store_int",
                [ Reference ("b", Builder);
                  Reference ("value", Int);
                  Integer (Z.of_int bits) ] ) ) ]
    in
    let ret = Return (Reference ("b", Builder)) in
    Fn (serialize :: [ret])
  in
  {function_name; function_args; function_returns; function_body}

let codegen_struct_ :
    codegen_ctx ref -> Lang_types.struct_ -> top_level_expr list =
 fun ctx s ->
  let serialize_builder_fn =
    match s.struct_modifiers.is_builtin_int with
    | true ->
        codegen_integer s.struct_modifiers.int_bits
    | false ->
        codegen_serialize_function_struct_ ctx s
  in
  ctx :=
    { serialize_builder_functions =
        (Lang_types.StructType s, serialize_builder_fn.function_name)
        :: !ctx.serialize_builder_functions } ;
  let serialize_cell_fn =
    codegen_serialize_cell_function_struct_ serialize_builder_fn.function_name
      (Option.value (List.tl serialize_builder_fn.function_args) ~default:[])
  in
  [Function serialize_builder_fn; Function serialize_cell_fn]

let rec flatten = function [] -> [] | l :: r -> l @ flatten r

let codegen_prog : Lang_types.program -> program =
 fun program ->
  let codegen_ctx = ref {serialize_builder_functions = []} in
  let structs =
    let collector = new collector in
    let _ = collector#visit_program () program in
    collector#get_structs
  in
  flatten (List.map structs ~f:(codegen_struct_ codegen_ctx))

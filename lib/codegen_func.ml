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
  List.iter program ~f:(function
    | Function fn ->
        pp_function f fn ; pp_print_newline f ()
    | Global _ ->
        () )

and pp_function f fn =
  pp_open_hvbox f 2 ;
  pp_type f fn.function_returns ;
  pp_print_space f () ;
  pp_ident f fn.function_name ;
  pp_print_string f "(" ;
  List.iter fn.function_args ~f:(fun (name, t) ->
      pp_type f t ; pp_print_space f () ; pp_ident f name ) ;
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

let rec codegen_serialize_function_body_struct_ = function
  | (name, field_type) :: xs ->
      let serialize_this =
        Vars
          [ ( Builder,
              "b",
              FunctionCall
                ( "store_uint",
                  [Reference ("b", Builder); Reference (name, field_type)] ) )
          ]
      in
      serialize_this :: codegen_serialize_function_body_struct_ xs
  | [] ->
      []

let codegen_serialize_function_struct_ struct_ =
  let function_name = generate_struct_name struct_ in
  let struct_fields =
    List.map struct_.struct_fields ~f:(fun (name, {field_type}) ->
        (name, lang_expr_to_type field_type) )
  in
  let function_args = struct_fields in
  let function_returns = Cell in
  let function_body =
    let builder_stmt =
      Vars [(Builder, "b", FunctionCall ("new_builder", []))]
    in
    let serialize = codegen_serialize_function_body_struct_ struct_fields in
    let ret = Return (FunctionCall ("build", [Reference ("b", Builder)])) in
    Fn ((builder_stmt :: serialize) @ [ret])
  in
  {function_name; function_args; function_returns; function_body}

let codegen_struct_ : Lang_types.struct_ -> top_level_expr list =
 fun s ->
  let serialize_fn = codegen_serialize_function_struct_ s in
  [Function serialize_fn]

let rec flatten = function [] -> [] | l :: r -> l @ flatten r

let codegen_prog : Lang_types.program -> program =
 fun program ->
  let structs =
    let collector = new collector in
    let _ = collector#visit_program () program in
    collector#get_structs
  in
  flatten (List.map structs ~f:codegen_struct_)

(*
class codegen_ir =
  object (self)
    val program = []

    method codegen_ir : Lang.program -> program =
      function
      | {bindings = (name, expr) :: xs} ->
          self#codegen_top_level_expr name expr ;
          self#codegen_ir xs
      | {bindings = []} ->
          program

    method codegen
  end

class ['s] constructor ((program, errors) : Lang_types.program * _ errors) =
  object (s : 's)
    inherit ['s] Lang_types.visitor as super

    method build_Asm _env asm = raise Unsupported

    method build_Break _env e = raise Unsupported

    method build_Builtin _env b = raise Invalid

    method build_BuiltinFn _env b = raise Invalid

    method build_BuiltinType _env = raise Invalid

    method build_Expr _env expr = raise Invalid

    method build_Fn _env _ = raise Invalid

    method build_Function _env _f = raise Unsupported

    method build_FunctionCall _env _fc = AnExpr

    method build_FunctionType _env _ft = raise Invalid

    method build_Hole _env = raise Invalid

    method build_HoleType _env = raise Invalid

    method build_Integer _env i = Integer i

    method build_IntegerType _env = raise Invalid

    method build_Invalid _env = raise Invalid

    method build_InvalidExpr _env = raise Invalid

    method build_InvalidFn _env = raise Invalid

    method build_InvalidType _env = raise Invalid

    method build_Let _env bindings =
      Vars
        (List.map bindings ~f:(fun (name, expr) ->
             (typeof_expr expr, name, expr) ) )

    method build_Reference _env (name, t) = Reference (name, typeof_type t)

    method build_Return _env expr = Return expr

    method build_String : 's -> ident -> expr = fun _env -> raise Invalid

    method build_StringType _env = raise Invalid

    method build_Struct _env _s = raise Invalid

    method build_StructInstance _env _si = raise Unsupported

    method build_StructType _env _t = raise Invalid

    method build_Type _env _t = raise Invalid (* typeof_type t *)

    method build_TypeType _env = raise Invalid

    method build_Value _env v = v

    method build_Void _env = raise Invalid

    method build_VoidType _env = raise Invalid

    method build_function_ _env _params returns impl =
      Function
        { function_name = "dummy";
          function_args = [];
          function_returns = returns;
          function_body = impl }

    method build_program _env _p = raise Invalid

    method build_struct_ _env s = raise Invalid

    method build_struct_field _env _sf = raise Invalid

    method! visit_program env p = s#visit_list s#visit_binding env p.bindings
  end*)
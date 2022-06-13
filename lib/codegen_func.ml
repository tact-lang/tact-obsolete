open Base
module T = Lang_types
module F = Func
include Errors

exception Invalid

exception Unsupported

class constructor (program : T.program) =
  object (self)
    val mutable struct_representations : (T.struct_ * F.type_) list = []

    val mutable fn_name_counter = 0

    val mutable functions : (T.function_ * F.function_) list = []

    method cg_Fn body = F.Fn (List.concat (Option.value_exn body))

    method build_Integer : Zint.t -> F.expr = fun i -> F.Integer i

    method cg_Let : _ -> F.stmt =
      fun bindings ->
        F.Vars
          (List.map bindings ~f:(fun (name, expr) ->
               let expr = self#cg_expr expr in
               (F.type_of expr, name, expr) ) )

    method cg_Struct : T.struct_ * (string * T.expr) list -> F.expr =
      function
      | _, [(_, expr)] ->
          self#cg_expr expr
      | _, args ->
          F.Tuple (List.map args ~f:(fun (_, expr) -> self#cg_expr expr))

    method cg_expr : T.expr -> F.expr =
      function
      | Value (Integer i) ->
          F.Integer i
      | Value (Bool true) ->
          F.Integer (Zint.of_int (-1))
      | Value (Bool false) ->
          F.Integer Zint.zero
      | StructField x ->
          self#cg_StructField x
      | Value (Struct (id, inst)) ->
          self#cg_Struct (T.Program.get_struct program id, inst)
      | ResolvedReference s ->
          self#cg_ResolvedReference s
      | Reference (name, ty) ->
          F.Reference (name, self#lang_type_to_type ty)
      | Primitive p ->
          self#cg_Primitive p
      | Value (Function f) ->
          let f' = self#add_function f in
          F.Reference (f'.function_name, F.FunctionType f')
      | FunctionCall (func, args) -> (
          let args = List.map args ~f:self#cg_expr in
          match self#cg_expr func with
          | Reference (name, F.FunctionType f) ->
              F.FunctionCall (name, args, f.function_returns)
          | _ ->
              raise Invalid )
      | _ ->
          raise Unsupported

    method cg_stmt : T.stmt -> F.stmt =
      function
      | Let bindings ->
          self#cg_Let bindings
      | Return expr ->
          F.Return (self#cg_expr expr)
      | Expr e ->
          F.Expr (self#cg_expr e)
      | Block stmts ->
          F.Block (List.map stmts ~f:self#cg_stmt)
      | If {if_condition; if_then; if_else} ->
          F.If
            ( self#cg_expr if_condition,
              self#cg_stmt if_then,
              Option.map if_else ~f:self#cg_stmt )
      | Break stmt ->
          self#cg_stmt stmt (* FIXME: this is unlikely to be correct *)
      | _ ->
          raise Unsupported

    method cg_function_ : string -> T.function_ -> F.function_ =
      fun name fn ->
        let body =
          match fn.function_impl with
          | Fn (Some (Block stmts)) ->
              stmts
          | Fn (Some stmt) ->
              [stmt]
          | _ ->
              []
        in
        { function_name = name;
          function_args =
            List.map fn.function_signature.function_params ~f:(fun (name, ty) ->
                (name, self#lang_type_to_type ty) );
          function_returns =
            self#lang_type_to_type fn.function_signature.function_returns;
          function_body = F.Fn (List.map body ~f:self#cg_stmt) }

    method cg_top_level_stmt : string -> T.expr -> F.top_level_expr option =
      fun name -> function
        | Value (Function f) -> (
          try Some (F.Function (self#add_function f ~name:(Some name)))
          with _ -> None )
        | _ ->
            None

    method cg_program : T.program -> F.program =
      fun program ->
        let _ =
          List.filter_map (List.rev program.bindings)
            ~f:(fun (name, top_level_stmt) ->
              self#cg_top_level_stmt name top_level_stmt )
        in
        List.map (List.rev functions) ~f:(fun (_, f) -> F.Function f)

    method cg_StructField (from_expr, field) =
      let build_access struct_ty field field_ty =
        let name =
          match field with
          | 0 ->
              "first"
          | 1 ->
              "second"
          | 2 ->
              "third"
          | _ ->
              raise Unsupported
        in
        F.FunctionCall (name, [struct_ty], field_ty)
      in
      match T.type_of from_expr with
      | StructType s -> (
          let s = T.Program.get_struct program s in
          match s.struct_fields with
          | [_] ->
              self#cg_expr from_expr
          | _ ->
              let field_id, (_, field) =
                Option.value_exn
                  (List.findi s.struct_fields ~f:(fun _ (name, _) ->
                       equal_string name field ) )
              in
              build_access (self#cg_expr from_expr) field_id
                (self#lang_type_to_type field.field_type) )
      | _ ->
          raise Invalid

    method cg_ResolvedReference (_, expr) = self#cg_expr expr

    method cg_StoreInt builder length int_ is_signed =
      let name =
        match is_signed with true -> "store_int" | false -> "store_uint"
      in
      F.FunctionCall (name, [builder; int_; length], F.IntType)

    method cg_Primitive : T.primitive -> F.expr =
      function
      | EmptyBuilder ->
          self#cg_EmptyBuilder
      | BuildCell {builder} ->
          self#cg_BuildCell builder
      | StoreInt {builder; length; integer; signed} ->
          self#cg_StoreInt (self#cg_expr builder) (self#cg_expr length)
            (self#cg_expr integer) signed

    method cg_EmptyBuilder = F.FunctionCall ("new_builder", [], F.BuilderType)

    method cg_BuildCell builder_arg =
      F.FunctionCall ("build", [self#cg_expr builder_arg], F.CellType)

    method private lang_type_to_type : T.type_ -> F.type_ =
      function
      | IntegerType ->
          F.IntType
      | BoolType ->
          F.IntType
      | StructType s ->
          self#struct_to_ty (T.Program.get_struct program s)
      | BuiltinType "Builder" ->
          F.BuilderType
      | BuiltinType "Cell" ->
          F.CellType
      | HoleType ->
          F.InferType
      | _ ->
          raise Invalid

    method private struct_to_ty : T.struct_ -> F.type_ =
      fun s ->
        match
          List.find struct_representations ~f:(fun (s', _) ->
              Lang_types.equal_struct_ s s' )
        with
        | Some (_, ty) ->
            ty
        | None ->
            let ty = self#create_ty_from_struct s in
            struct_representations <- (s, ty) :: struct_representations ;
            ty

    method private create_ty_from_struct : T.struct_ -> F.type_ =
      function
      | {struct_fields = [(_, {field_type})]; _} ->
          self#lang_type_to_type field_type
      | {struct_fields; _} ->
          let types =
            List.map struct_fields ~f:(fun (_, {field_type}) ->
                self#lang_type_to_type field_type )
          in
          TupleType types

    method private add_function
        : ?name:string option -> T.function_ -> F.function_ =
      fun ?(name = None) fn ->
        let default () =
          let name =
            Option.value_or_thunk name ~default:(fun () ->
                self#generate_func_name )
          in
          let fn' = self#cg_function_ name fn in
          functions <- (fn, fn') :: functions ;
          fn'
        in
        List.Assoc.find functions ~equal:T.equal_function_ fn
        |> Option.value_or_thunk ~default

    method private generate_func_name =
      let num = fn_name_counter in
      fn_name_counter <- fn_name_counter + 1 ;
      "f" ^ Printf.sprintf "%d" num
  end

let codegen program =
  let constructor = new constructor program in
  constructor#cg_program program

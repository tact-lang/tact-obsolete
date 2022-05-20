open Base
module T = Lang_types
module F = Func
include Errors

exception Invalid

exception Unsupported

class constructor =
  object (self)
    val mutable struct_representations : (T.struct_ * F.type_) list = []

    val mutable functions : (string * F.function_) list = []

    method cg_Fn body = F.Fn (List.concat (Option.value_exn body))

    method build_Integer : Zint.t -> F.expr = fun i -> F.Integer i

    method cg_Let : _ -> F.stmt =
      fun bindings ->
        F.Vars
          (List.map bindings ~f:(fun (name, expr) ->
               let expr = self#cg_expr expr in
               (F.type_of expr, name, expr) ) )

    method cg_expr : T.expr -> F.expr =
      function
      | Value (Integer i) ->
          F.Integer i
      | StructField x ->
          self#cg_StructField x
      | ResolvedReference s ->
          self#cg_ResolvedReference s
      | Primitive p ->
          self#cg_Primitive p
      | _ ->
          raise Unsupported

    method cg_stmt : T.stmt -> F.stmt =
      function
      | Let bindings ->
          self#cg_Let bindings
      | Return expr ->
          F.Return (self#cg_expr expr)
      | _ ->
          raise Unsupported

    method cg_function_ : string -> T.function_ -> F.function_ =
      fun name fn ->
        let body =
          match fn.function_impl with
          | Fn body ->
              Option.value_exn body
          | _ ->
              []
        in
        { function_name = name;
          function_args =
            List.map fn.function_signature.function_params ~f:(fun (name, ty) ->
                (name, self#lang_expr_to_type ty) );
          function_returns =
            self#lang_expr_to_type fn.function_signature.function_returns;
          function_body = F.Fn (List.map body ~f:self#cg_stmt) }

    method cg_top_level_stmt : string -> T.expr -> F.top_level_expr option =
      fun name -> function
        | Value (Function f) -> (
          try Some (F.Function (self#cg_function_ name f)) with _ -> None )
        | _ ->
            None

    method cg_program : T.program -> F.program =
      fun program ->
        List.filter_map program.bindings ~f:(fun (name, top_level_stmt) ->
            self#cg_top_level_stmt name top_level_stmt )

    method cg_StructField (from_expr, field) =
      let build_access struct_ty field =
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
        F.FunctionCall (name, [struct_ty])
      in
      match T.type_of from_expr with
      | StructType s ->
          let field_id, (_, _) =
            Option.value_exn
              (List.findi s.struct_fields ~f:(fun _ (name, _) ->
                   equal_string name field ) )
          in
          build_access (self#cg_expr from_expr) field_id
      | _ ->
          raise Invalid

    method cg_ResolvedReference (name, _) =
      match
        List.find functions ~f:(fun (fname, _) -> equal_string name fname)
      with
      | Some (_, fn) ->
          F.Reference (name, fn.function_returns)
      | None ->
          raise Invalid

    method cg_StoreInt builder length int_ is_signed =
      let name =
        match is_signed with true -> "store_int" | false -> "store_uint"
      in
      F.FunctionCall (name, [builder; int_; length])

    method cg_Primitive : T.primitive -> F.expr =
      function
      | EmptyBuilder ->
          self#cg_EmptyBuilder
      | BuildCell {builder} ->
          self#cg_BuildCell builder
      | StoreInt {builder; length; integer; signed} ->
          self#cg_StoreInt (self#cg_expr builder) (self#cg_expr length)
            (self#cg_expr integer) signed

    method cg_EmptyBuilder = F.FunctionCall ("new_builder", [])

    method cg_BuildCell builder_arg =
      F.FunctionCall ("build", [self#cg_expr builder_arg])

    method private lang_expr_to_type : T.expr -> F.type_ =
      function
      | Value (Type t) ->
          self#lang_type_to_type t
      | ResolvedReference (_ref, expr) ->
          self#lang_expr_to_type expr
      | _ ->
          raise Invalid

    method private lang_type_to_type : T.type_ -> F.type_ =
      function
      | IntegerType ->
          F.IntType
      | StructType s ->
          self#struct_to_ty s
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
      fun {struct_fields; _} ->
        let types =
          List.map struct_fields ~f:(fun (_, {field_type}) ->
              self#lang_expr_to_type field_type )
        in
        TupleType types
  end

let codegen program =
  let constructor = new constructor in
  constructor#cg_program program

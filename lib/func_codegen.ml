open Base
module T = Lang_types
module F = Func
include Errors

exception Invalid

exception Unsupported

class ['s] constructor ((_program, _errors) : T.program * _ errors) =
  object (self : 's)
    inherit ['s] Lang_types.visitor as _super

    val mutable struct_representations : (T.struct_ * F.type_) list = []

    val mutable functions : (string * F.function_) list = []

    method build_Asm _env _asm = raise Unsupported

    method build_Break _env _e = raise Unsupported

    method build_Builtin _env _b = raise Invalid

    method build_BuiltinFn _env _b = raise Invalid

    method build_BuiltinType _env = raise Invalid

    method build_Expr _env _expr = raise Invalid

    method build_Fn _env _ = raise Invalid

    method build_Function _env _f = raise Unsupported

    method build_FunctionCall _env _fc = raise Invalid

    method build_FunctionType _env _ft = raise Invalid

    method build_Hole _env = raise Invalid

    method build_HoleType _env = raise Invalid

    method build_Integer : _ -> Zint.t -> F.expr = fun _env i -> F.Integer i

    method build_IntegerType _env = F.IntType

    method build_Invalid _env = raise Invalid

    method build_InvalidExpr _env = raise Invalid

    method build_InvalidFn _env = raise Invalid

    method build_InvalidType _env = raise Invalid

    method build_Let _env _bindings = raise Invalid

    method build_Reference _env (name, ty) = F.Reference (name, ty)

    method build_Return _env expr = F.Return expr

    method build_String : 's -> string -> F.expr = fun _env -> raise Invalid

    method build_StringType _env = raise Invalid

    method build_StructInstance _env _si = raise Unsupported

    method build_StructType _env ty = ty

    method build_Type _env _t = raise Invalid (* typeof_type t *)

    method build_TypeType _env = raise Invalid

    method build_Value _env _v = raise Invalid

    method build_Void _env = raise Invalid

    method build_VoidType _env = raise Invalid

    method build_function_ _env _params returns impl =
      F.Function
        { function_name = "dummy";
          function_args = [];
          function_returns = returns;
          function_body = impl }

    method build_program _env _p = raise Invalid

    method build_struct_ _env _field _id = raise Invalid

    method! visit_struct_ _env s = self#struct_to_ty s

    method build_struct_field _env _sf = raise Invalid

    method build_function_signature _env _sf = raise Invalid

    method build_StructField _env _ = raise Unsupported

    method! visit_StructField env (from_expr, field) =
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
          build_access (self#visit_expr env from_expr) field_id
      | _ ->
          raise Invalid

    method build_ResolvedReference _env (name, _) =
      match
        List.find functions ~f:(fun (fname, _) -> equal_string name fname)
      with
      | Some (_, fn) ->
          F.Reference (name, fn.function_returns)
      | None ->
          raise Invalid

    method build_StoreInt _env builder length int_ is_signed =
      let name =
        match is_signed with true -> "store_int" | false -> "store_uint"
      in
      (name, [builder; int_; length])

    method build_Primitive _env p = FunctionCall p

    method build_EmptyBuilder _env = ("new_builder", [])

    method build_BuildCell _env builder_arg = ("build", [builder_arg])

    method! visit_program env p =
      self#visit_list self#visit_binding env p.bindings

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
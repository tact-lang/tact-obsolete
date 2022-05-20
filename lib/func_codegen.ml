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

    method build_IntegerType _env = raise Invalid

    method build_Invalid _env = raise Invalid

    method build_InvalidExpr _env = raise Invalid

    method build_InvalidFn _env = raise Invalid

    method build_InvalidType _env = raise Invalid

    method build_Let _env _bindings = raise Invalid

    method build_Reference _env (_name, _t) = raise Invalid

    method build_Return _env expr = F.Return expr

    method build_String : 's -> string -> F.expr = fun _env -> raise Invalid

    method build_StringType _env = raise Invalid

    method build_Struct _env _s = raise Invalid

    method build_StructInstance _env _si = raise Unsupported

    method build_StructType _env _t = raise Invalid

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

    method! visit_struct_ _env _s = ()

    method build_struct_field _env _sf = raise Invalid

    method build_function_signature _env _sf = raise Invalid

    method build_StructField _env _sf = raise Invalid

    method build_StoreInt _env _sf = raise Invalid

    method build_ResolvedReference _env _sf = raise Invalid

    method build_Primitive _env _sf = raise Invalid

    method build_EmptyBuilder _env _sf = raise Invalid

    method build_BuildCell _env _sf = raise Invalid

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
          F.Int
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
        Tuple types
  end
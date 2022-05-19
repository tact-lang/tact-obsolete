open Lang_types
open PrintBox
open Base
open Caml.Format

class ['s] formatter =
  object (self : 's)
    inherit [_] visitor

    method build_Break _env stmt = hlist [text "break"; stmt]

    method build_BuildCell _env expr = hlist [text "BuildCell"; expr]

    method build_Builtin _env builtin = text @@ String.concat ["("; builtin; ")"]

    method build_BuiltinFn _env (_, fn_id) =
      text @@ String.concat ["fn("; Int.to_string fn_id; ")"]

    method build_BuiltinType _env builtin =
      text @@ String.concat ["("; builtin; ")"]

    method build_EmptyBuilder _env = text "EmptyBuilder"

    method build_Expr _env expr = expr

    method build_Fn _env = function Some fb -> vlist fb | None -> vlist []

    method build_Function _env function_ = function_

    method build_FunctionCall _env (fn, args) =
      frame @@ hlist [fn; text "("; hlist args; text ")"]

    method build_FunctionType _env sig_ = sig_

    method build_Hole _env = text "_"

    method build_HoleType _env = text "_"

    method build_Integer _env z = text @@ Zint.to_string z

    method build_IntegerType _env = text "int"

    method build_Invalid _env = text "❌"

    method build_InvalidExpr _env = text "❌"

    method build_InvalidFn _env = text "❌"

    method build_InvalidType _env = text "❌"

    method build_Let _env bindings =
      frame @@ vlist [text "Let binding"; v_record bindings]

    method build_Primitive _env prim = hlist [text "〈"; prim; text "〉"]

    method build_Reference _env (name, type_) =
      hlist [text name; text " : "; type_]

    method build_ResolvedReference _env (name, _expr) =
      hlist [text name; text "↗"]

    method build_Return _env expr = hlist [text "return"; expr]

    method build_StoreInt _env builder length integer signed =
      hlist
        [ builder;
          (text @@ if signed then "signed" else "");
          integer;
          text "bits: ";
          length ]

    method build_String _env s = text s

    method build_StringType _env = text "string"

    method build_Struct _env s = hlist [text "struct"; s]

    method build_StructField _env (struct_, field) =
      hlist ~bars:false [struct_; text "•"; text field]

    method build_StructInstance _env _si = text "struct instance"

    method build_StructType env struct_ = self#build_Struct env struct_

    method build_Type _env type_ = type_

    method build_TypeType _env = text "Type"

    method build_Value _env value = value

    method build_Void _env = text "void"

    method build_VoidType _env = text "void"

    method build_struct_ _env fields _ =
      frame @@ v_record
      @@ List.map fields ~f:(fun (name, type_) : (string * t) -> (name, type_))

    method build_binding _env (name, expr) = hlist [text name; expr]

    method build_program _env _stmts bindings _methods =
      tree (text "◉")
      @@ List.map bindings ~f:(fun (name, expr) -> hlist [text name; expr])

    method build_stmt _env stmt = stmt

    method build_struct_field _env type_ = type_

    method build_function_signature _env params returns =
      hlist [record params; text " ⟶ "; returns]

    method build_function_ _env sig_ impl =
      vlist [hlist [text "fn "; vlist ~bars:false [sig_]]; impl]
  end

let pp_program f p =
  let box = (new formatter)#visit_program () p in
  pp_print_string f @@ PrintBox_text.to_string box ;
  pp_print_newline f ()

open Sexplib.Std

module Make =
functor
  (Config : Config.T)
  ->
  struct
    include Config

    class ['s] base_visitor =
      object (_ : 's)
        inherit ['s] VisitorsRuntime.map

        inherit ['s] Zint.map

        method visit_located
            : 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a located -> 'b located =
          fun f env l -> {value = f env l.value; span = l.span}

        method visit_span _ span = span
      end

    type ident = Ident of string

    and struct_definition =
      { struct_attributes : attribute list; [@sexp.list]
        fields : struct_field located list; [@sexp.list]
        struct_bindings : binding located list; [@sexp.list]
        impls : impl list; [@sexp.list]
        struct_span : (span[@sexp.opaque]) }

    and impl =
      { impl_attributes : attribute list; [@sexp.list]
        interface : expr located;
        methods : binding located list }

    and interface_definition =
      { interface_attributes : attribute list; [@sexp.list]
        interface_members : binding located list [@sexp.list] }

    and function_call =
      { fn : expr located;
        arguments : expr located list; [@sexp.list]
        is_type_func_call : bool [@sexp.bool] }

    and method_call =
      { receiver : expr located;
        receiver_fn : ident located;
        receiver_arguments : expr located list }

    and enum_definition =
      { enum_attributes : attribute list; [@sexp.list]
        enum_members : enum_member located list; [@sexp.list]
        enum_bindings : binding located list; [@sexp.list]
        enum_span : (span[@sexp.opaque]) }

    and enum_member =
      { enum_name : ident located;
        enum_value : expr located option [@sexp.option] }

    (* TODO: union impls *)
    and union_definition =
      { union_attributes : attribute list; [@sexp.list]
        union_members : expr located list; [@sexp.list]
        union_bindings : binding located list; [@sexp.list]
        union_impls : impl list; [@sexp.list]
        union_span : (span[@sexp.opaque]) }

    and attribute =
      { attribute_ident : ident located;
        attribute_exprs : expr located list [@sexp.list] }

    and expr =
      | Struct of struct_definition
      | StructConstructor of struct_constructor
      | Interface of interface_definition
      | Enum of enum_definition
      | Union of union_definition
      | Reference of ident located
      | FieldAccess of field_access
      | FunctionCall of function_call
      | MethodCall of method_call
      | Function of function_definition
      | Int of (Zint.t[@visitors.name "z"])
      | Bool of bool
      | String of string
      | MutRef of ident located

    and stmt =
      | CodeBlock of (stmt located list[@sexp.list])
      | Let of binding located
      | DestructuringLet of destructuring_binding located
      | Assignment of assignment
      | If of if_
      | Return of expr located
      | Break of stmt located
      | Expr of expr located
      | Switch of switch
      | WhileLoop of while_loop

    and while_loop = {while_cond : expr located; while_body : stmt located}

    and switch =
      { switch_condition : expr located;
        branches : switch_branch located list;
        default : stmt option [@sexp.option] }

    and switch_branch =
      {ty : expr located; var : ident located; stmt : stmt located}

    and struct_constructor =
      { constructor_id : expr located;
        fields_construction : (ident located * expr located) list [@sexp.list]
      }

    and struct_field =
      { field_attributes : attribute list; [@sexp.list]
        field_name : ident located;
        field_type : expr located }

    and function_param = ident located * expr located

    and function_definition =
      { function_attributes : attribute list; [@sexp.list]
        name : ident located option; [@sexp.option]
        is_type_function : bool; [@sexp.bool]
        params : function_param located list; [@sexp.list]
        returns : expr located option; [@sexp.option]
        function_body : function_body option; [@sexp.option]
        function_def_span : (span[@sexp.opaque]) }

    and function_body = {function_stmt : stmt located}

    and binding = {binding_name : ident located; binding_expr : expr located}

    and destructuring_binding =
      { destructuring_binding : (ident located * ident located) list located;
        destructuring_binding_expr : expr located;
        destructuring_binding_rest : bool }

    and assignment =
      {assignment_ident : ident located; assignment_expr : expr located}

    and if_ =
      { condition : expr located;
        body : stmt located;
        else_ : stmt located option [@sexp.option] }

    and field_access = {from_expr : expr located; to_field : ident located}

    and program = {stmts : stmt located list [@sexp.list]}
    [@@deriving
      show {with_path = false},
        make,
        sexp_of,
        visitors
          {variety = "fold"; name = "visitor"; ancestors = ["base_visitor"]}]

    let ident_to_string = function Ident s -> s
  end

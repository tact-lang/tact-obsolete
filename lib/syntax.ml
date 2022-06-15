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
            : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a located -> 'b located
            =
          fun f env l -> make_located ~value:(f env (value l)) ~loc:(loc l) ()
      end

    type ident = Ident of string

    and struct_definition =
      { fields : struct_field located list; [@sexp.list]
        struct_bindings : binding located list; [@sexp.list]
        impls : impl list [@sexp.list] }

    and impl = {interface : expr located; methods : binding located list}

    and interface_definition =
      {interface_members : binding located list [@sexp.list]}

    and function_call =
      {fn : expr located; arguments : expr located list [@sexp.list]}

    and method_call =
      { receiver : expr located;
        receiver_fn : ident located;
        receiver_arguments : expr located list }

    and enum_definition =
      { enum_members : enum_member located list; [@sexp.list]
        enum_bindings : binding located list [@sexp.list] }

    and enum_member =
      { enum_name : ident located;
        enum_value : expr located option [@sexp.option] }

    (* TODO: union impls *)
    and union_definition =
      { union_members : expr located list; [@sexpa.list]
        union_bindings : binding located list [@sexp.list] }

    and expr =
      | Struct of struct_definition
      | StructConstructor of struct_constructor
      | Interface of interface_definition
      | Enum of enum_definition
      | Union of union_definition
      | Reference of ident
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
      | If of if_
      | Return of expr
      | Break of stmt
      | Expr of expr
      | Switch of switch

    and switch = {switch_condition : expr located; branches : switch_branch list}

    and switch_branch = {ty : expr located; var : ident located; stmt : stmt}

    and struct_constructor =
      { constructor_id : expr located;
        fields_construction : (ident located * expr located) list [@sexp.list]
      }

    and struct_field = {field_name : ident located; field_type : expr located}

    and function_param = ident located * expr located

    and function_definition =
      { name : ident located option; [@sexp.option]
        params : function_param located list; [@sexp.list]
        returns : expr located option; [@sexp.option]
        function_body : function_body option [@sexp.option] }

    and function_body = {function_stmt : stmt}

    and binding = {binding_name : ident located; binding_expr : expr located}

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

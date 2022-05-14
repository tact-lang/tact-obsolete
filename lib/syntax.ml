open Sexplib.Std

module type T = sig
  include Located.T

  type ident = Ident of string

  and struct_definition =
    {fields : struct_field located list; struct_bindings : binding located list}

  and interface_definition = {interface_members : binding located list}

  and function_call = {fn : expr located; arguments : expr located list}

  and method_call =
    { receiver : expr located;
      receiver_fn : ident located;
      receiver_arguments : expr located list }

  and enum_definition =
    { enum_members : enum_member located list;
      enum_bindings : binding located list }

  and enum_member = {enum_name : ident located; enum_value : expr located option}

  and union_definition =
    {union_members : expr located list; union_bindings : binding located list}

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
    | Int of Zint.t
    | MutRef of ident located

  and stmt =
    | CodeBlock of stmt located list
    | Let of binding located
    | If of if_
    | Return of expr
    | Break of stmt
    | Expr of expr

  and struct_constructor =
    { constructor_id : expr located;
      fields_construction : (ident located * expr located) list }

  and struct_field = {field_name : ident located; field_type : expr located}

  and function_param = ident located * expr located

  and function_definition =
    { name : ident located option;
      params : function_param located list;
      returns : expr located option;
      function_body : function_body option }

  and function_body = {function_stmts : stmt located list}

  and binding = {binding_name : ident located; binding_expr : expr located}

  and if_ =
    { condition : expr located;
      body : stmt located list located;
      else_ : stmt located option [@sexp.option] }

  and field_access = {from_expr : expr located; to_field : ident located}

  and program = {stmts : stmt located list [@sexp.list]}
  [@@deriving show, make, sexp_of]

  class virtual ['c] visitor :
    object ('c)
      constraint
      'c = < build_Break : 'd -> 'g -> 'g
           ; build_CodeBlock : 'd -> 'g located list -> 'g
           ; build_Enum : 'd -> 'h -> 'i
           ; build_Expr : 'd -> 'i -> 'g
           ; build_FieldAccess : 'd -> 'j -> 'i
           ; build_Function : 'd -> 'k -> 'i
           ; build_FunctionCall : 'd -> 'l -> 'i
           ; build_Ident : 'd -> string -> 'm
           ; build_If : 'd -> 'n -> 'g
           ; build_Int : 'd -> Zint.t -> 'i
           ; build_Interface : 'd -> 'o -> 'i
           ; build_Let : 'd -> 'p located -> 'g
           ; build_MethodCall : 'd -> 'q -> 'i
           ; build_MutRef : 'd -> 'm located -> 'i
           ; build_Reference : 'd -> 'm -> 'i
           ; build_Return : 'd -> 'i -> 'g
           ; build_Struct : 'd -> 'r -> 'i
           ; build_StructConstructor : 'd -> 's -> 'i
           ; build_Union : 'd -> 't -> 'i
           ; build_binding : 'd -> 'm located -> 'i located -> 'p
           ; build_enum_definition :
               'd -> 'u located list -> 'p located list -> 'h
           ; build_enum_member : 'd -> 'm located -> 'i located option -> 'u
           ; build_field_access : 'd -> 'i located -> 'm located -> 'j
           ; build_function_body : 'd -> 'g located list -> 'v
           ; build_function_call : 'd -> 'i located -> 'i located list -> 'l
           ; build_function_definition :
               'd ->
               'm located option ->
               ('m located * 'i located) located list ->
               'i located option ->
               'v option ->
               'k
           ; build_if_ :
               'd ->
               'i located ->
               'g located list located ->
               'g located option ->
               'n
           ; build_interface_definition : 'd -> 'p located list -> 'o
           ; build_method_call :
               'd -> 'i located -> 'm located -> 'i located list -> 'q
           ; build_program : 'd -> 'g located list -> 'w
           ; build_struct_constructor :
               'd -> 'i located -> ('m located * 'i located) list -> 's
           ; build_struct_definition :
               'd -> 'x located list -> 'p located list -> 'r
           ; build_struct_field : 'd -> 'm located -> 'i located -> 'x
           ; build_union_definition :
               'd -> 'i located list -> 'p located list -> 't
           ; visit_Break : 'd -> stmt -> 'g
           ; visit_CodeBlock : 'd -> stmt located list -> 'g
           ; visit_Enum : 'd -> enum_definition -> 'i
           ; visit_Expr : 'd -> expr -> 'g
           ; visit_FieldAccess : 'd -> field_access -> 'i
           ; visit_Function : 'd -> function_definition -> 'i
           ; visit_FunctionCall : 'd -> function_call -> 'i
           ; visit_Ident : 'd -> string -> 'm
           ; visit_If : 'd -> if_ -> 'g
           ; visit_Int : 'd -> Zint.t -> 'i
           ; visit_Interface : 'd -> interface_definition -> 'i
           ; visit_Let : 'd -> binding located -> 'g
           ; visit_MethodCall : 'd -> method_call -> 'i
           ; visit_MutRef : 'd -> ident located -> 'i
           ; visit_Reference : 'd -> ident -> 'i
           ; visit_Return : 'd -> expr -> 'g
           ; visit_Struct : 'd -> struct_definition -> 'i
           ; visit_StructConstructor : 'd -> struct_constructor -> 'i
           ; visit_Union : 'd -> union_definition -> 'i
           ; visit_binding : 'd -> binding -> 'p
           ; visit_enum_definition : 'd -> enum_definition -> 'h
           ; visit_enum_member : 'd -> enum_member -> 'u
           ; visit_expr : 'd -> expr -> 'i
           ; visit_field_access : 'd -> field_access -> 'j
           ; visit_function_body : 'd -> function_body -> 'v
           ; visit_function_call : 'd -> function_call -> 'l
           ; visit_function_definition : 'd -> function_definition -> 'k
           ; visit_function_param :
               'd -> function_param -> 'm located * 'i located
           ; visit_ident : 'd -> ident -> 'm
           ; visit_if_ : 'd -> if_ -> 'n
           ; visit_interface_definition : 'd -> interface_definition -> 'o
           ; visit_located :
               'env 'a 'b.
               ('env -> 'a -> 'b) -> 'env -> 'a located -> 'b located
           ; visit_method_call : 'd -> method_call -> 'q
           ; visit_program : 'd -> program -> 'w
           ; visit_stmt : 'd -> stmt -> 'g
           ; visit_struct_constructor : 'd -> struct_constructor -> 's
           ; visit_struct_definition : 'd -> struct_definition -> 'r
           ; visit_struct_field : 'd -> struct_field -> 'x
           ; visit_union_definition : 'd -> union_definition -> 't
           ; visit_z : 'env. 'env -> Zint.t -> Zint.t
           ; .. >

      method virtual build_Break : 'd -> 'g -> 'g

      method virtual build_CodeBlock : 'd -> 'g located list -> 'g

      method virtual build_Enum : 'd -> 'h -> 'i

      method virtual build_Expr : 'd -> 'i -> 'g

      method virtual build_FieldAccess : 'd -> 'j -> 'i

      method virtual build_Function : 'd -> 'k -> 'i

      method virtual build_FunctionCall : 'd -> 'l -> 'i

      method virtual build_Ident : 'd -> string -> 'm

      method virtual build_If : 'd -> 'n -> 'g

      method virtual build_Int : 'd -> Zint.t -> 'i

      method virtual build_Interface : 'd -> 'o -> 'i

      method virtual build_Let : 'd -> 'p located -> 'g

      method virtual build_MethodCall : 'd -> 'q -> 'i

      method virtual build_MutRef : 'd -> 'm located -> 'i

      method virtual build_Reference : 'd -> 'm -> 'i

      method virtual build_Return : 'd -> 'i -> 'g

      method virtual build_Struct : 'd -> 'r -> 'i

      method virtual build_StructConstructor : 'd -> 's -> 'i

      method virtual build_Union : 'd -> 't -> 'i

      method virtual build_binding : 'd -> 'm located -> 'i located -> 'p

      method virtual build_enum_definition :
        'd -> 'u located list -> 'p located list -> 'h

      method virtual build_enum_member :
        'd -> 'm located -> 'i located option -> 'u

      method virtual build_field_access : 'd -> 'i located -> 'm located -> 'j

      method virtual build_function_body : 'd -> 'g located list -> 'v

      method virtual build_function_call :
        'd -> 'i located -> 'i located list -> 'l

      method virtual build_function_definition :
        'd ->
        'm located option ->
        ('m located * 'i located) located list ->
        'i located option ->
        'v option ->
        'k

      method virtual build_if_ :
        'd -> 'i located -> 'g located list located -> 'g located option -> 'n

      method virtual build_interface_definition : 'd -> 'p located list -> 'o

      method virtual build_method_call :
        'd -> 'i located -> 'm located -> 'i located list -> 'q

      method virtual build_program : 'd -> 'g located list -> 'w

      method virtual build_struct_constructor :
        'd -> 'i located -> ('m located * 'i located) list -> 's

      method virtual build_struct_definition :
        'd -> 'x located list -> 'p located list -> 'r

      method virtual build_struct_field : 'd -> 'm located -> 'i located -> 'x

      method virtual build_union_definition :
        'd -> 'i located list -> 'p located list -> 't

      method visit_Break : 'd -> stmt -> 'g

      method visit_CodeBlock : 'd -> stmt located list -> 'g

      method visit_Enum : 'd -> enum_definition -> 'i

      method visit_Expr : 'd -> expr -> 'g

      method visit_FieldAccess : 'd -> field_access -> 'i

      method visit_Function : 'd -> function_definition -> 'i

      method visit_FunctionCall : 'd -> function_call -> 'i

      method visit_Ident : 'd -> string -> 'm

      method visit_If : 'd -> if_ -> 'g

      method visit_Int : 'd -> Zint.t -> 'i

      method visit_Interface : 'd -> interface_definition -> 'i

      method visit_Let : 'd -> binding located -> 'g

      method visit_MethodCall : 'd -> method_call -> 'i

      method visit_MutRef : 'd -> ident located -> 'i

      method visit_Reference : 'd -> ident -> 'i

      method visit_Return : 'd -> expr -> 'g

      method visit_Struct : 'd -> struct_definition -> 'i

      method visit_StructConstructor : 'd -> struct_constructor -> 'i

      method visit_Union : 'd -> union_definition -> 'i

      method private visit_array :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

      method visit_binding : 'd -> binding -> 'p

      method private visit_bool : 'env. 'env -> bool -> bool

      method private visit_bytes : 'env. 'env -> bytes -> bytes

      method private visit_char : 'env. 'env -> char -> char

      method visit_enum_definition : 'd -> enum_definition -> 'h

      method visit_enum_member : 'd -> enum_member -> 'u

      method visit_expr : 'd -> expr -> 'i

      method visit_field_access : 'd -> field_access -> 'j

      method private visit_float : 'env. 'env -> float -> float

      method visit_function_body : 'd -> function_body -> 'v

      method visit_function_call : 'd -> function_call -> 'l

      method visit_function_definition : 'd -> function_definition -> 'k

      method visit_function_param :
        'd -> function_param -> 'm located * 'i located

      method visit_ident : 'd -> ident -> 'm

      method visit_if_ : 'd -> if_ -> 'n

      method private visit_int : 'env. 'env -> int -> int

      method private visit_int32 : 'env. 'env -> int32 -> int32

      method private visit_int64 : 'env. 'env -> int64 -> int64

      method visit_interface_definition : 'd -> interface_definition -> 'o

      method private visit_lazy_t :
        'env 'a 'b.
        ('env -> 'a -> 'b) ->
        'env ->
        'a Sexplib.Std.Lazy.t ->
        'b Sexplib.Std.Lazy.t

      method private visit_list :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

      method visit_located :
        ('env -> 'a -> 'b) -> 'env -> 'a located -> 'b located

      method visit_method_call : 'd -> method_call -> 'q

      method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

      method private visit_option :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

      method visit_program : 'd -> program -> 'w

      method private visit_ref :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

      method private visit_result :
        'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env ->
        ('a, 'e) Result.result ->
        ('b, 'f) Result.result

      method visit_stmt : 'd -> stmt -> 'g

      method private visit_string : 'env. 'env -> string -> string

      method visit_struct_constructor : 'd -> struct_constructor -> 's

      method visit_struct_definition : 'd -> struct_definition -> 'r

      method visit_struct_field : 'd -> struct_field -> 'x

      method visit_union_definition : 'd -> union_definition -> 't

      method private visit_unit : 'env. 'env -> unit -> unit

      method visit_z : 'env -> Zint.t -> Zint.t
    end

  val ident_to_string : ident -> string
end

module Make =
functor
  (L : Located.T)
  ->
  struct
    include L

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
        struct_bindings : binding located list [@sexp.list] }

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
      | MutRef of ident located

    and stmt =
      | CodeBlock of (stmt located list[@sexp.list])
      | Let of binding located
      | If of if_
      | Return of expr
      | Break of stmt
      | Expr of expr

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

    and function_body = {function_stmts : stmt located list [@sexp.list]}

    and binding = {binding_name : ident located; binding_expr : expr located}

    and if_ =
      { condition : expr located;
        body : (stmt located list[@sexp.list]) located;
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

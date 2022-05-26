open Sexplib.Std

module type T = sig
  include Located.T

  type ident = Ident of string

  and struct_definition =
    { fields : struct_field located list;
      struct_bindings : binding located list;
      impls : impl list }

  and impl = {interface : expr located; methods : binding located list}

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
    | Bool of bool
    | String of string
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

  and function_body = {function_stmt : stmt}

  and binding = {binding_name : ident located; binding_expr : expr located}

  and if_ =
    { condition : expr located;
      body : stmt located;
      else_ : stmt located option [@sexp.option] }

  and field_access = {from_expr : expr located; to_field : ident located}

  and program = {stmts : stmt located list [@sexp.list]}
  [@@deriving show, make, sexp_of]

  class virtual ['c] visitor :
    object ('c)
      constraint
      'c = < build_Bool : 'd -> bool -> 'g
           ; build_Break : 'd -> 'h -> 'h
           ; build_CodeBlock : 'd -> 'h located list -> 'h
           ; build_Enum : 'd -> 'i -> 'g
           ; build_Expr : 'd -> 'g -> 'h
           ; build_FieldAccess : 'd -> 'j -> 'g
           ; build_Function : 'd -> 'k -> 'g
           ; build_FunctionCall : 'd -> 'l -> 'g
           ; build_Ident : 'd -> string -> 'm
           ; build_If : 'd -> 'n -> 'h
           ; build_Int : 'd -> Zint.t -> 'g
           ; build_Interface : 'd -> 'o -> 'g
           ; build_Let : 'd -> 'p located -> 'h
           ; build_MethodCall : 'd -> 'q -> 'g
           ; build_MutRef : 'd -> 'm located -> 'g
           ; build_Reference : 'd -> 'm -> 'g
           ; build_Return : 'd -> 'g -> 'h
           ; build_String : 'd -> string -> 'g
           ; build_Struct : 'd -> 'r -> 'g
           ; build_StructConstructor : 'd -> 's -> 'g
           ; build_Union : 'd -> 't -> 'g
           ; build_binding : 'd -> 'm located -> 'g located -> 'p
           ; build_enum_definition :
               'd -> 'u located list -> 'p located list -> 'i
           ; build_enum_member : 'd -> 'm located -> 'g located option -> 'u
           ; build_field_access : 'd -> 'g located -> 'm located -> 'j
           ; build_function_body : 'd -> 'h -> 'v
           ; build_function_call : 'd -> 'g located -> 'g located list -> 'l
           ; build_function_definition :
               'd ->
               'm located option ->
               ('m located * 'g located) located list ->
               'g located option ->
               'v option ->
               'k
           ; build_if_ :
               'd -> 'g located -> 'h located -> 'h located option -> 'n
           ; build_impl : 'd -> 'g located -> 'p located list -> 'w
           ; build_interface_definition : 'd -> 'p located list -> 'o
           ; build_method_call :
               'd -> 'g located -> 'm located -> 'g located list -> 'q
           ; build_program : 'd -> 'h located list -> 'x
           ; build_struct_constructor :
               'd -> 'g located -> ('m located * 'g located) list -> 's
           ; build_struct_definition :
               'd -> 'y located list -> 'p located list -> 'w list -> 'r
           ; build_struct_field : 'd -> 'm located -> 'g located -> 'y
           ; build_union_definition :
               'd -> 'g located list -> 'p located list -> 't
           ; visit_Bool : 'd -> bool -> 'g
           ; visit_Break : 'd -> stmt -> 'h
           ; visit_CodeBlock : 'd -> stmt located list -> 'h
           ; visit_Enum : 'd -> enum_definition -> 'g
           ; visit_Expr : 'd -> expr -> 'h
           ; visit_FieldAccess : 'd -> field_access -> 'g
           ; visit_Function : 'd -> function_definition -> 'g
           ; visit_FunctionCall : 'd -> function_call -> 'g
           ; visit_Ident : 'd -> string -> 'm
           ; visit_If : 'd -> if_ -> 'h
           ; visit_Int : 'd -> Zint.t -> 'g
           ; visit_Interface : 'd -> interface_definition -> 'g
           ; visit_Let : 'd -> binding located -> 'h
           ; visit_MethodCall : 'd -> method_call -> 'g
           ; visit_MutRef : 'd -> ident located -> 'g
           ; visit_Reference : 'd -> ident -> 'g
           ; visit_Return : 'd -> expr -> 'h
           ; visit_String : 'd -> string -> 'g
           ; visit_Struct : 'd -> struct_definition -> 'g
           ; visit_StructConstructor : 'd -> struct_constructor -> 'g
           ; visit_Union : 'd -> union_definition -> 'g
           ; visit_binding : 'd -> binding -> 'p
           ; visit_enum_definition : 'd -> enum_definition -> 'i
           ; visit_enum_member : 'd -> enum_member -> 'u
           ; visit_expr : 'd -> expr -> 'g
           ; visit_field_access : 'd -> field_access -> 'j
           ; visit_function_body : 'd -> function_body -> 'v
           ; visit_function_call : 'd -> function_call -> 'l
           ; visit_function_definition : 'd -> function_definition -> 'k
           ; visit_function_param :
               'd -> function_param -> 'm located * 'g located
           ; visit_ident : 'd -> ident -> 'm
           ; visit_if_ : 'd -> if_ -> 'n
           ; visit_impl : 'd -> impl -> 'w
           ; visit_interface_definition : 'd -> interface_definition -> 'o
           ; visit_located :
               'env 'a 'b.
               ('env -> 'a -> 'b) -> 'env -> 'a located -> 'b located
           ; visit_method_call : 'd -> method_call -> 'q
           ; visit_program : 'd -> program -> 'x
           ; visit_stmt : 'd -> stmt -> 'h
           ; visit_struct_constructor : 'd -> struct_constructor -> 's
           ; visit_struct_definition : 'd -> struct_definition -> 'r
           ; visit_struct_field : 'd -> struct_field -> 'y
           ; visit_union_definition : 'd -> union_definition -> 't
           ; visit_z : 'env. 'env -> Zint.t -> Zint.t
           ; .. >

      method virtual build_Bool : 'd -> bool -> 'g

      method virtual build_Break : 'd -> 'h -> 'h

      method virtual build_CodeBlock : 'd -> 'h located list -> 'h

      method virtual build_Enum : 'd -> 'i -> 'g

      method virtual build_Expr : 'd -> 'g -> 'h

      method virtual build_FieldAccess : 'd -> 'j -> 'g

      method virtual build_Function : 'd -> 'k -> 'g

      method virtual build_FunctionCall : 'd -> 'l -> 'g

      method virtual build_Ident : 'd -> string -> 'm

      method virtual build_If : 'd -> 'n -> 'h

      method virtual build_Int : 'd -> Zint.t -> 'g

      method virtual build_Interface : 'd -> 'o -> 'g

      method virtual build_Let : 'd -> 'p located -> 'h

      method virtual build_MethodCall : 'd -> 'q -> 'g

      method virtual build_MutRef : 'd -> 'm located -> 'g

      method virtual build_Reference : 'd -> 'm -> 'g

      method virtual build_Return : 'd -> 'g -> 'h

      method virtual build_String : 'd -> string -> 'g

      method virtual build_Struct : 'd -> 'r -> 'g

      method virtual build_StructConstructor : 'd -> 's -> 'g

      method virtual build_Union : 'd -> 't -> 'g

      method virtual build_binding : 'd -> 'm located -> 'g located -> 'p

      method virtual build_enum_definition :
        'd -> 'u located list -> 'p located list -> 'i

      method virtual build_enum_member :
        'd -> 'm located -> 'g located option -> 'u

      method virtual build_field_access : 'd -> 'g located -> 'm located -> 'j

      method virtual build_function_body : 'd -> 'h -> 'v

      method virtual build_function_call :
        'd -> 'g located -> 'g located list -> 'l

      method virtual build_function_definition :
        'd ->
        'm located option ->
        ('m located * 'g located) located list ->
        'g located option ->
        'v option ->
        'k

      method virtual build_if_ :
        'd -> 'g located -> 'h located -> 'h located option -> 'n

      method virtual build_impl : 'd -> 'g located -> 'p located list -> 'w

      method virtual build_interface_definition : 'd -> 'p located list -> 'o

      method virtual build_method_call :
        'd -> 'g located -> 'm located -> 'g located list -> 'q

      method virtual build_program : 'd -> 'h located list -> 'x

      method virtual build_struct_constructor :
        'd -> 'g located -> ('m located * 'g located) list -> 's

      method virtual build_struct_definition :
        'd -> 'y located list -> 'p located list -> 'w list -> 'r

      method virtual build_struct_field : 'd -> 'm located -> 'g located -> 'y

      method virtual build_union_definition :
        'd -> 'g located list -> 'p located list -> 't

      method visit_Bool : 'd -> bool -> 'g

      method visit_Break : 'd -> stmt -> 'h

      method visit_CodeBlock : 'd -> stmt located list -> 'h

      method visit_Enum : 'd -> enum_definition -> 'g

      method visit_Expr : 'd -> expr -> 'h

      method visit_FieldAccess : 'd -> field_access -> 'g

      method visit_Function : 'd -> function_definition -> 'g

      method visit_FunctionCall : 'd -> function_call -> 'g

      method visit_Ident : 'd -> string -> 'm

      method visit_If : 'd -> if_ -> 'h

      method visit_Int : 'd -> Zint.t -> 'g

      method visit_Interface : 'd -> interface_definition -> 'g

      method visit_Let : 'd -> binding located -> 'h

      method visit_MethodCall : 'd -> method_call -> 'g

      method visit_MutRef : 'd -> ident located -> 'g

      method visit_Reference : 'd -> ident -> 'g

      method visit_Return : 'd -> expr -> 'h

      method visit_String : 'd -> string -> 'g

      method visit_Struct : 'd -> struct_definition -> 'g

      method visit_StructConstructor : 'd -> struct_constructor -> 'g

      method visit_Union : 'd -> union_definition -> 'g

      method private visit_array :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

      method visit_binding : 'd -> binding -> 'p

      method private visit_bool : 'env. 'env -> bool -> bool

      method private visit_bytes : 'env. 'env -> bytes -> bytes

      method private visit_char : 'env. 'env -> char -> char

      method visit_enum_definition : 'd -> enum_definition -> 'i

      method visit_enum_member : 'd -> enum_member -> 'u

      method visit_expr : 'd -> expr -> 'g

      method visit_field_access : 'd -> field_access -> 'j

      method private visit_float : 'env. 'env -> float -> float

      method visit_function_body : 'd -> function_body -> 'v

      method visit_function_call : 'd -> function_call -> 'l

      method visit_function_definition : 'd -> function_definition -> 'k

      method visit_function_param :
        'd -> function_param -> 'm located * 'g located

      method visit_ident : 'd -> ident -> 'm

      method visit_if_ : 'd -> if_ -> 'n

      method visit_impl : 'd -> impl -> 'w

      method private visit_int : 'env. 'env -> int -> int

      method private visit_int32 : 'env. 'env -> int32 -> int32

      method private visit_int64 : 'env. 'env -> int64 -> int64

      method visit_interface_definition : 'd -> interface_definition -> 'o

      method private visit_lazy_t :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t

      method private visit_list :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

      method visit_located :
        ('env -> 'a -> 'b) -> 'env -> 'a located -> 'b located

      method visit_method_call : 'd -> method_call -> 'q

      method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

      method private visit_option :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

      method visit_program : 'd -> program -> 'x

      method private visit_ref :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

      method private visit_result :
        'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env ->
        ('a, 'e) Result.result ->
        ('b, 'f) Result.result

      method visit_stmt : 'd -> stmt -> 'h

      method private visit_string : 'env. 'env -> string -> string

      method visit_struct_constructor : 'd -> struct_constructor -> 's

      method visit_struct_definition : 'd -> struct_definition -> 'r

      method visit_struct_field : 'd -> struct_field -> 'y

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

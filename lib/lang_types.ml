open Base

let print_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

module Arena = struct
  type 'a t =
    { mutable current_id : int; [@hash.ignore]
      mutable items : (int * 'a) list [@hash.ignore] }
  [@@deriving equal, compare, hash, sexp_of]

  class ['s] visitor =
    object (_self : 's)
      method visit_arena : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a t -> 'a t =
        fun _ _ a -> a
    end

  let default () = {current_id = 0; items = []}

  let get a id =
    List.Assoc.find a.items id ~equal:equal_int
    |> Option.value_exn
         ~message:(Printf.sprintf "Try to get non existent id %i" id)

  let with_id a ~f =
    let current_id = a.current_id in
    a.current_id <- current_id + 1 ;
    let item = f current_id in
    a.items <- (current_id, item) :: a.items ;
    (current_id, item)

  open struct
    let rec update_list id new_item = function
      | [] ->
          raise Errors.InternalCompilerError
      | (xid, old_s) :: xs ->
          if equal_int xid id then (id, new_item) :: xs
          else (xid, old_s) :: update_list id new_item xs
  end

  let update a id ~f =
    let item = get a id in
    let new_item = f item in
    a.items <- update_list id new_item a.items ;
    new_item

  (* For tests purposes only *)
  let strip_if_exists left right =
    { left with
      items =
        List.filter left.items ~f:(fun (id, _) ->
            Option.is_none (List.Assoc.find right.items id ~equal:equal_int) )
    }

  let unsafe_drop_first a =
    a.current_id <- a.current_id - 1 ;
    a.items <- List.tl_exn a.items
end

class ['s] base_map =
  object (_ : 's)
    inherit ['s] Zint.map

    inherit ['s] Asm.map

    inherit ['s] Arena.visitor
  end

class virtual ['s] base_reduce =
  object (_ : 's)
    method virtual visit_instr : _

    method virtual visit_z : _

    method virtual visit_arena
        : 'env 'a. ('env -> 'a -> _) -> 'env -> 'a Arena.t -> _
  end

class virtual ['s] base_visitor =
  object (_ : 's)
    inherit ['s] VisitorsRuntime.map

    inherit ['s] Zint.map

    inherit ['s] Asm.map

    inherit ['s] Arena.visitor
  end

type comptime_counter = (int[@sexp.opaque])

and metadata = (string * string) list

and binding = string * expr

and tbinding = string * binding_scope

and binding_scope = Comptime of expr | Runtime of type_

and program =
  { bindings : (string * expr) list;
    mutable structs : (int * struct_) list; [@hash.ignore]
    mutable unions : (int * union) list; [@sexp.list] [@hash.ignore]
    mutable interfaces : (int * interface) list; [@sexp.list] [@hash.ignore]
    mutable type_counter : (int[@sexp.opaque]); [@hash.ignore]
    mutable memoized_fcalls :
      (((value * value list) * value) list[@sexp.opaque]);
        [@hash.ignore]
    mutable struct_signs : struct_sig Arena.t;
        [@hash.ignore] [@visitors.name "arena"]
    mutable union_signs : union_sig Arena.t
        [@hash.ignore] [@visitors.name "arena"] }

and expr =
  | FunctionCall of function_call
  | IntfMethodCall of intf_method_call
  | StructSigMethodCall of st_sig_method_call
  | MkStructDef of mk_struct
  | MkUnionDef of mk_union
  | MkInterfaceDef of mk_interface
  | MkFunction of function_
  | MakeUnionVariant of (expr * int)
  | Reference of (string * type_)
  | ResolvedReference of (string * (expr[@sexp.opaque]))
  | Value of value
  | StructField of (expr * string * type_)
  | Hole
  | Primitive of primitive
  | InvalidExpr

and mk_interface = {mk_interface_methods : (string * function_signature) list}

and interface = {interface_methods : (string * function_signature) list}

and if_ = {if_condition : expr; if_then : stmt; if_else : stmt option}

and value =
  | Void
  | Struct of (expr * (string * expr) list)
  | UnionVariant of (value * int)
  | Function of function_
  | Integer of (Zint.t[@visitors.name "z"])
  | Bool of bool
  | String of string
  | Builtin of builtin
  | Type of type_

and stmt =
  | If of if_
  | Let of (string * expr) list
  | DestructuringLet of destructuring_let
  | Return of expr
  | Break of stmt
  | Expr of expr
  | Block of stmt list
  | Switch of switch
  | Invalid

and destructuring_let =
  { destructuring_let : (string * string) list;
    destructuring_let_expr : expr;
    destructuring_let_rest : bool }

and builtin = string

and type_ =
  | TypeN of int
  | IntegerType
  | BoolType
  | StringType
  | VoidType
  | BuiltinType of builtin
  | StructType of int
  | UnionType of int
  | InterfaceType of int
  | StructSig of int
  | UnionSig of int
  | FunctionType of function_signature
  | HoleType
  | SelfType
  | InvalidType of expr
  | ExprType of expr
  | Dependent of string * type_
  | ValueOf of type_

and mk_union =
  { mk_cases : expr list;
    mk_union_methods : (string * expr) list;
    mk_union_impls : mk_impl list; [@sexp.list]
    mk_union_id : int;
    mk_union_sig : int }

and union =
  { cases : (type_ * discriminator) list;
    union_methods : (string * function_) list;
    union_impls : impl list; [@sexp.list]
    union_id : int;
    union_base_id : int }

and mk_struct =
  { mk_struct_fields : (string * expr) list;
    mk_methods : (string * expr) list;
    mk_impls : mk_impl list;
    mk_struct_id : int;
    mk_struct_sig : int }

and struct_ =
  { struct_fields : (string * struct_field) list;
    struct_methods : (string * function_) list;
    struct_impls : impl list;
    struct_id : int;
    struct_base_id : int;
    (* Used by codegen to determine if this is a tensor *)
    tensor : bool [@sexp.bool] }

and struct_sig =
  { st_sig_fields : (string * expr) list;
    st_sig_methods : (string * function_signature) list;
    (* ID of the base of the struct. *)
    st_sig_base_id : int }

and union_sig =
  { un_sig_cases : type_ list;
    un_sig_methods : (string * function_signature) list;
    (* ID of the base of the struct. *)
    un_sig_base_id : int }

and discriminator = Discriminator of int

and struct_field = {field_type : type_}

and mk_impl = {mk_impl_interface : expr; mk_impl_methods : binding list}

and impl = {impl_interface : int; impl_methods : (string * function_) list}

and function_body = (stmt option[@sexp.option])

and native_function =
  (program -> value list -> value
  [@visitors.opaque] [@equal.ignore] [@compare.ignore] )

and builtin_fn = native_function * (int[@sexp.opaque])

and function_ =
  {function_signature : function_signature; function_impl : function_impl}

and function_signature =
  {function_params : (string * type_) list; function_returns : type_}

and function_impl = Fn of function_body | BuiltinFn of builtin_fn | InvalidFn

and function_call = expr * expr list

and intf_method_call =
  { intf_instance : expr;
    intf_def : int;
    intf_method : string * function_signature;
    intf_args : expr list }

and st_sig_method_call =
  { st_sig_call_instance : expr;
    st_sig_call_def : int;
    st_sig_call_method : string * function_signature;
    st_sig_call_args : expr list;
    st_sig_call_kind : sig_kind }

and sig_kind = UnionSigKind | StructSigKind

and switch = {switch_condition : expr; branches : branch list}

and branch = {branch_ty : type_; branch_var : string; branch_stmt : stmt}

and primitive =
  | Divmod of {x : expr; y : expr}
  | Equality of {x : expr; y : expr}
  | EmptyBuilder
  | StoreInt of {builder : expr; length : expr; integer : expr; signed : bool}
  | StoreCoins of {builder : expr; coins : expr}
  | BuildCell of {builder : expr}
  | SendRawMsg of {msg : expr; flags : expr}
  | ParseCell of {cell : expr}
  | SliceEndParse of {slice : expr}
  | SliceLoadInt of {slice : expr; bits : expr}
[@@deriving
  equal,
    compare,
    hash,
    sexp_of,
    visitors {variety = "map"; polymorphic = true; ancestors = ["base_map"]},
    visitors {variety = "reduce"; ancestors = ["base_reduce"]},
    visitors {variety = "fold"; name = "visitor"; ancestors = ["base_visitor"]}]

let type0 = TypeN 0

let make_runtime (x, type_) = (x, Runtime type_)

let make_comptime (x, value) = (x, Comptime value)

let find_comptime name bindings =
  List.find_map bindings ~f:(fun bindings ->
      List.find_map bindings ~f:(function
        | b_name, Comptime value ->
            if equal_string b_name name then Some (Ok value) else None
        | b_name, Runtime _ ->
            if equal_string b_name name then Some (Error ()) else None ) )

let extract_comptime_bindings bindings =
  List.filter_map bindings ~f:(fun (name, scope) ->
      match scope with Comptime value -> Some (name, value) | _ -> None )

let sig_of_struct {struct_fields; struct_methods; struct_base_id; _} =
  { st_sig_fields =
      List.Assoc.map struct_fields ~f:(fun {field_type} ->
          match field_type with
          | ExprType ex ->
              ex
          | field_type ->
              Value (Type field_type) );
    st_sig_methods =
      List.Assoc.map struct_methods ~f:(fun x -> x.function_signature);
    st_sig_base_id = struct_base_id }

let sig_of_union {cases; union_methods; union_base_id; _} =
  { un_sig_cases = List.map cases ~f:fst;
    un_sig_methods =
      List.Assoc.map union_methods ~f:(fun x -> x.function_signature);
    un_sig_base_id = union_base_id }

let rec expr_to_type program = function
  | Value (Type type_) ->
      type_
  | Reference (ref, ty) ->
      ExprType (Reference (ref, ty))
  (* | FunctionCall (f, args) -> (
      let f = type_of program f in
      match f with
      | FunctionType sign ->
          type_of_call program args sign.function_params sign.function_returns
      | _ ->
          raise Errors.InternalCompilerError ) *)
  | ResolvedReference (_, e) ->
      expr_to_type program e
  | expr ->
      ExprType expr

and type_of program = function
  | Value (Struct (s, _)) ->
      expr_to_type program s
  | Value (UnionVariant (_, uid)) ->
      UnionType uid
  | Value (Function {function_signature; _}) ->
      FunctionType function_signature
  | Value (Builtin builtin) ->
      BuiltinType builtin
  | Value (Integer _) ->
      IntegerType
  | Value (Bool _) ->
      BoolType
  | Value Void ->
      VoidType
  | Value (Type (Dependent (_, ty))) ->
      ty
  | Value (Type t) ->
      type_of_type program t
  | Hole ->
      HoleType
  | FunctionCall (f, args) -> (
      let f' = type_of program f in
      match f' with
      | FunctionType sign ->
          type_of_call program args sign.function_params sign.function_returns
      | _ ->
          raise Errors.InternalCompilerError )
  | Reference (_, t) ->
      t
  | ResolvedReference (_, e) ->
      type_of program e
  | MakeUnionVariant (_, u) ->
      UnionType u
  | MkStructDef mk ->
      StructSig mk.mk_struct_sig
  | StructField (_, _, ty) ->
      ty
  | IntfMethodCall {intf_method = _, sign; intf_args; _} ->
      type_of_call program intf_args sign.function_params sign.function_returns
  | StructSigMethodCall {st_sig_call_method = _, sign; st_sig_call_args; _} ->
      type_of_call program st_sig_call_args sign.function_params
        sign.function_returns
  | MkFunction mk_function ->
      FunctionType mk_function.function_signature
  | MkUnionDef uni ->
      UnionSig uni.mk_union_sig
  | expr ->
      InvalidType expr

and type_of_type program = function
  | TypeN x ->
      TypeN (x + 1)
  | StructSig _ | UnionSig _ ->
      TypeN 1
  | ValueOf ty ->
      ty
  | ExprType ex ->
      type_of program ex
  | _otherwise ->
      TypeN 0

and type_of_call program args arg_types returns =
  let associated =
    match List.map2 args arg_types ~f:(fun expr (name, _) -> (name, expr)) with
    | Ok t ->
        t
    | _ ->
        raise Errors.InternalCompilerError
  in
  let dependent_types_monomophizer (program : program)
      (associated : (string * expr) list) =
    object (self : _)
      inherit [_] map

      val mutable visited_signs : (int * int) list = []

      val mutable visited_union_signs : (int * int) list = []

      method! visit_StructSig env sign_id =
        match List.Assoc.find visited_signs sign_id ~equal:equal_int with
        | Some new_id ->
            StructSig new_id
        | None ->
            let sign = Arena.get program.struct_signs sign_id in
            let id, new_sign =
              Arena.with_id program.struct_signs ~f:(fun new_id ->
                  let prev_vis_signs = visited_signs in
                  visited_signs <- (sign_id, new_id) :: visited_signs ;
                  let new_sign = self#visit_struct_sig env sign in
                  visited_signs <- prev_vis_signs ;
                  new_sign )
            in
            if equal_struct_sig sign new_sign then (
              Arena.unsafe_drop_first program.struct_signs ;
              StructSig sign_id )
            else StructSig id

      method! visit_UnionSig env sign_id =
        match List.Assoc.find visited_union_signs sign_id ~equal:equal_int with
        | Some new_id ->
            UnionSig new_id
        | None ->
            let sign = Arena.get program.union_signs sign_id in
            let id, new_sign =
              Arena.with_id program.union_signs ~f:(fun new_id ->
                  let prev_vis_signs = visited_union_signs in
                  visited_union_signs <-
                    (sign_id, new_id) :: visited_union_signs ;
                  let new_sign = self#visit_union_sig env sign in
                  visited_union_signs <- prev_vis_signs ;
                  new_sign )
            in
            if equal_union_sig sign new_sign then (
              Arena.unsafe_drop_first program.union_signs ;
              UnionSig sign_id )
            else UnionSig id

      method! visit_Dependent _ ref ty =
        (* if equal_string ref "T" then (
           print_sexp @@ sexp_of_list sexp_of_binding associated ;
           print_sexp @@ sexp_of_type_ returns ) ; *)
        List.find_map associated ~f:(fun (name, x) ->
            if equal_string name ref then
              Some
                ( match x with
                (* If we depend on reference, it means we depend on function argument,
                   so type must be dependent. *)
                | Reference (r, t) ->
                    if equal_string r "Self" then ExprType (Reference (r, t))
                    else Dependent (r, t)
                | x ->
                    type_of program x )
            else None )
        |> Option.value_or_thunk ~default:(fun _ -> Dependent (ref, ty))
    end
  in
  let monomorphizer = dependent_types_monomophizer program associated in
  monomorphizer#visit_type_ () returns

class ['s] boolean_reduce (zero : bool) =
  object (_self : 's)
    inherit [_] reduce

    method private zero = zero

    method private plus = if zero then ( && ) else ( || )

    method visit_instr _env _instr = zero

    method visit_z _env _z = zero

    method visit_arena _ _ _ = zero
  end

class ['s] expr_immediacy_check ?(inside_function_call = false)
  ?(arguments = []) ?(is_primitive_immediate = false)
  (scope : tbinding list list) (program : program) =
  object (self : 's)
    inherit [_] boolean_reduce true as super

    val mutable arguments : string list list = arguments

    method! visit_Reference _ (ref, _) =
      match find_comptime ref scope with
      | Some (Ok _) ->
          true
      | Some (Error _) ->
          false
      | None ->
          List.find arguments ~f:(List.exists ~f:(equal_string ref))
          |> Option.is_some

    method! visit_Primitive _env _primitive = is_primitive_immediate

    method! visit_Let env vars =
      self#visit_list
        (fun env (name, expr) ->
          let is_expr_immediate = self#visit_expr env expr in
          arguments <- [name] :: arguments ;
          is_expr_immediate )
        env vars

    method! visit_Block env block =
      self#with_arguments [] (fun _ -> super#visit_Block env block)

    method! visit_function_ env f =
      if not inside_function_call then
        let is_sig_immediate =
          self#visit_function_signature env f.function_signature
        in
        let args = List.map f.function_signature.function_params ~f:fst in
        let out =
          self#with_arguments args (fun ars ->
              (new expr_immediacy_check
                 ~is_primitive_immediate:true ~arguments:ars scope program )
                #visit_function_impl
                env f.function_impl )
        in
        self#plus is_sig_immediate out
      else
        let is_sig_immediate =
          self#visit_function_signature env f.function_signature
        in
        let args = List.map f.function_signature.function_params ~f:fst in
        let out =
          self#with_arguments args (fun ars ->
              (new expr_immediacy_check
                 ~is_primitive_immediate ~inside_function_call:false
                 ~arguments:ars scope program )
                #visit_function_impl
                env f.function_impl )
        in
        self#plus is_sig_immediate out

    method! visit_branch env {branch_var; branch_stmt; _} =
      self#with_arguments [branch_var] (fun _ ->
          self#visit_stmt env branch_stmt )

    method! visit_function_call env (f, args) =
      let args = self#visit_list self#visit_expr env args in
      let f =
        (new expr_immediacy_check
           ~is_primitive_immediate ~inside_function_call:true ~arguments scope
           program )
          #visit_expr
          env f
      in
      self#plus f args

    method! visit_function_signature env sign =
      let is_args_immediate =
        List.fold ~init:true
          ~f:(fun prev (_, ty2) -> prev && self#visit_type_ env ty2)
          sign.function_params
      in
      let args = List.map ~f:(fun (name, _) -> name) sign.function_params in
      let is_ret_immediate =
        self#with_arguments args (fun _ ->
            self#visit_type_ env sign.function_returns )
      in
      self#plus is_args_immediate is_ret_immediate

    method! visit_StructSig env sid =
      let sign = Arena.get program.struct_signs sid in
      let _1 = self#visit_list self#visit_binding env sign.st_sig_fields in
      _1

    method! visit_mk_struct env mk =
      self#with_arguments ["Self"] (fun _ -> super#visit_mk_struct env mk)

    method! visit_mk_union env mk =
      self#with_arguments ["Self"] (fun _ -> super#visit_mk_union env mk)

    (* FIXME: I'm not sure why `Int` function is not immediate,
       this should be investigated. *)
    method! visit_ResolvedReference env (ref, ex) =
      if equal_string ref "Int" then true
      else super#visit_ResolvedReference env (ref, ex)

    method private with_arguments args f =
      let prev_args = arguments in
      arguments <- args :: arguments ;
      let out = f arguments in
      arguments <- prev_args ;
      out
  end

let rec is_immediate_expr scope program expr =
  let checker = new expr_immediacy_check scope program in
  checker#visit_expr () expr

and are_immediate_arguments scope program args =
  Option.is_none
    (List.find args ~f:(fun a -> not (is_immediate_expr scope program a)))

let rec builtin_fun_counter = ref 0

and builtin_fun f =
  let res = (f, !builtin_fun_counter) in
  builtin_fun_counter := !builtin_fun_counter + 1 ;
  res

let find_in_scope : string -> tbinding list list -> binding_scope option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      List.find_map bindings ~f:(fun (s, x) ->
          if String.equal ref s then Some x else None ) )

let find_in_runtime_scope : 'a. string -> (string * 'a) list list -> 'a option =
 fun ref scope ->
  List.find_map scope ~f:(fun bindings ->
      List.find_map bindings ~f:(fun (name, value) ->
          if String.equal ref name then Some value else None ) )

module Value = struct
  let unwrap_function = function
    | Function f ->
        f
    | _ ->
        raise Errors.InternalCompilerError

  let unwrap_intf_id = function
    | Type (InterfaceType intf_id) ->
        intf_id
    | _ ->
        raise Errors.InternalCompilerError
end

module Program = struct
  let methods_of p = function
    | StructType s ->
        List.find_map_exn p.structs ~f:(fun (id, s') ->
            if equal_int id s then Some s'.struct_methods else None )
    | UnionType u ->
        List.find_map_exn p.unions ~f:(fun (id, u') ->
            if equal_int id u then Some u'.union_methods else None )
    | _ ->
        []

  let impls_of p = function
    | StructType s ->
        List.find_map_exn p.structs ~f:(fun (id, s') ->
            if equal_int id s then Some s'.struct_impls else None )
    | UnionType u ->
        List.find_map_exn p.unions ~f:(fun (id, u') ->
            if equal_int id u then Some u'.union_impls else None )
    | _ ->
        []

  let insert_interface p i =
    let c = p.type_counter in
    p.type_counter <- p.type_counter + 1 ;
    p.interfaces <- (c, i) :: p.interfaces ;
    InterfaceType c

  (* Caller must guarantee that index is not used and will not be used by other types. *)
  let insert_interface_with_id p idx intf =
    p.interfaces <- (idx, intf) :: p.interfaces ;
    InterfaceType idx

  let get_intf p id = List.Assoc.find_exn p.interfaces id ~equal:equal_int

  let get_struct p s = List.Assoc.find_exn p.structs s ~equal:equal_int

  let get_union p u = List.Assoc.find_exn p.unions u ~equal:equal_int

  let rec update_list id new_s = function
    | [] ->
        raise Errors.InternalCompilerError
    | (xid, old_s) :: xs ->
        if equal_int xid id then
          match new_s with Ok new_s -> (id, new_s) :: xs | Error _ -> xs
        else (xid, old_s) :: update_list id new_s xs

  let update_struct p sid ~f =
    let s = get_struct p sid in
    let new_s = f s in
    p.structs <- update_list sid (Ok new_s) p.structs ;
    new_s

  let update_union p sid ~f =
    let s = get_union p sid in
    let new_u = f s in
    p.unions <- update_list sid (Ok new_u) p.unions ;
    new_u

  let with_struct p s f =
    p.structs <- (s.struct_id, s) :: p.structs ;
    let new_s = f () in
    p.structs <- update_list s.struct_id new_s p.structs ;
    new_s

  (* Creates new struct id, calls function with this new id and then
     places returning struct to the program.structs *)
  let with_id p f =
    let id = p.type_counter in
    p.type_counter <- p.type_counter + 1 ;
    let new_s = f id in
    p.structs <- (id, new_s) :: p.structs ;
    new_s

  let with_union p u f =
    p.unions <- (u.union_id, u) :: p.unions ;
    let new_u = f () in
    p.unions <- update_list u.union_id new_u p.unions ;
    new_u

  (* Creates new struct id, calls function with this new id and then
     places returning union to the program.unions *)
  let with_union_id p mk_union f =
    let id = p.type_counter in
    p.type_counter <- p.type_counter + 1 ;
    let u = mk_union id in
    p.unions <- (u.union_id, u) :: p.unions ;
    let new_union = f u in
    p.unions <- update_list id new_union p.unions ;
    new_union

  let find_impl_intf p impl = function
    | StructType s ->
        List.find (get_struct p s).struct_impls ~f:(fun {impl_interface; _} ->
            equal_int impl_interface impl )
    | UnionType u ->
        List.find (get_union p u).union_impls ~f:(fun {impl_interface; _} ->
            equal_int impl_interface impl )
    | _ ->
        None
end

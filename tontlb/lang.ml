open Base

type ident = string [@@deriving sexp_of, equal]

and tag = Bitstr.t [@@deriving equal]

type combinator =
  { combinator_ident : ident;
    combinator_eqs : (constructor * combinator_type) list }

and constructor = {constructor_tag : tag option; constructor_params : field list}

and field =
  | ExprField of expr
  | Field of {field_ident : ident option; field_expr : expr}

and type_ =
  [ `Uint of int
  | `Int of Zint.t
  | `Type
  | `NatLeq of Zint.t
  | `Combinator of combinator_type
  | `Conditional of type_expression * type_
  | `TBD
  | `Mismatch
  | `NotFound of ident ]

and combinator_type = ident * type_ list

and expr =
  | Equals of (expr * expr)
  | Less of (expr * expr)
  | Leq of (expr * expr)
  | Greater of (expr * expr)
  | Geq of (expr * expr)
  | Plus of (expr * expr)
  | Times of (expr * expr)
  | Exprs of expr0 list

and expr0 = type_expression * type_expression option * type_expression option

and type_expression =
  | Runtime of type_expression
  | Expr of expr
  | AnonymousConstr of field list
  | CellRef of type_expression
  | Int of Zint.t
  | NamedRef of ident
  | NatLess
  | NatLeq
  | Type of type_
  | UIntN
[@@deriving sexp_of, equal]

let rec type_of_type_expression (fields : Syntax.field list)
    (combinators : (ident, combinator) Hashtbl.t) :
    Syntax.type_expression -> type_ = function
  | Runtime expr ->
      type_of_type_expression fields combinators expr
  | Expr expr ->
      type_of_expr fields combinators expr
  | NamedRef name ->
      let field_type =
        List.find_map fields ~f:(function
          | ImplicitField {field_ident; field_type; _}
            when String.equal field_ident name ->
              Some (field_type :> type_)
          | NamedField {field_ident; field_expr; _}
            when String.equal field_ident name ->
              Some (type_of_expr fields combinators (Exprs [field_expr]))
          | _ ->
              None )
        |> Option.value_or_thunk ~default:(fun () ->
               try
                 `Combinator
                   ((Hashtbl.find_exn combinators name).combinator_ident, [])
               with _ -> `NotFound name )
      in
      field_type
  | Int i ->
      `Int i
  | _ ->
      `TBD

and type_of_expr (fields : Syntax.field list)
    (combinators : (ident, combinator) Hashtbl.t) : Syntax.expr -> type_ =
  function
  | Exprs exprs -> (
      let typ_expr =
        List.fold ~init:None
          ~f:(fun state (expr, _, _) ->
            let typ = type_of_type_expression fields combinators expr in
            match (state, typ) with
            | None, _ ->
                Some (Type typ)
            | Some NatLeq, `Int i ->
                Some (Type (`NatLeq i))
            | other, _ ->
                other )
          exprs
        |> Option.value_exn
      in
      match typ_expr with Type t -> t | _ -> failwith "expected type" )
  | Plus (x, y) | Times (x, y) ->
      let t_x = type_of_expr fields combinators x
      and t_y = type_of_expr fields combinators y in
      reconcile_types t_x t_y
  | _ ->
      `TBD

and reconcile_types (t1 : type_) (t2 : type_) : type_ =
  match (t1, t2) with
  | `Uint u, `Int i | `Int i, `Uint u ->
      if u >= Zint.numbits i then `Uint u else failwith "Mismatched int sizes"
  | t1, t2 ->
      if equal_type_ t1 t2 then t2 else failwith "Mismatched types"

let of_syntax ({decls; _} : Syntax.root) =
  let combinators = Hashtbl.create (module String) in
  (* Pre-populate combinators without details just so that 
   * we can validate whether a combinator exists *)
  List.iter
    ~f:(fun {combinator = {combinator_ident; _}; _} ->
      ignore
      @@ Hashtbl.add combinators ~key:combinator_ident
           ~data:{combinator_ident; combinator_eqs = []} )
    decls ;
  (* Populate equations
     List.iter
       ~f:(fun { combinator = {combinator_ident; _};
                 combinator_constructor;
                 combinator_params;
                 combinator_fields;
                 _ } ->
         let combinator_params =
           List.map
             ~f:(type_of_type_expression combinator_fields combinators)
             combinator_params
         in
         let constructor = match {constructor_tag} in
         let eq = (constructor, combinator_type) in
         ignore
         @@ Hashtbl.change combinators combinator_ident
              ~f:(Option.map ~f:(fun c -> {c with combinator_eqs = eq :: c.combinator_eqs})) )
       decls ; *)
  Hashtbl.data combinators

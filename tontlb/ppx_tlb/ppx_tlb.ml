open Camelsnakekebab
open Ppxlib

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

open Base

let reserved =
  [ "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "unit";
    "bool";
    "bytes" ]

let type_name_case t =
  let t = lower_snake_case t in
  if List.exists reserved ~f:(String.equal t) then t ^ "_" else t

let typeof ~loc tlb params (expr : Tlb.Syntax.expr0) =
  let is_type t =
    (Option.is_some @@ List.Assoc.find tlb ~equal:String.equal t)
    || String.equal "bits" t || String.equal "Cell" t || String.equal "Any" t
    || String.is_prefix t ~prefix:"uint"
    || String.is_prefix t ~prefix:"int"
    || String.is_prefix t ~prefix:"bits"
  in
  let rec typeof_ ~loc params (expr : Tlb.Syntax.expr0) =
    match expr with
    | CellRef expr, x, y ->
        typeof_ ~loc params (expr, x, y)
    | Expr (Exprs exprs), _, _ -> (
      match exprs with
      | [(UIntN, _, _); (Int n, _, _)] ->
          Some
            (Ast_builder.Default.ptyp_constr ~loc
               (Loc.make ~loc
                  (Astlib.Longident.parse @@ "uint" ^ Z.to_string n) )
               [] )
      | e :: e' -> (
        match typeof_ ~loc params e with
        | Some ({ptyp_desc = Ptyp_constr (ident, _); _} as t) ->
            Some
              { t with
                ptyp_desc =
                  Ptyp_constr
                    (ident, List.filter_map e' ~f:(typeof_ ~loc params)) }
        | other ->
            other )
      | [] ->
          failwith "_" )
    | NamedRef r, None, None -> (
      match Hashtbl.find params (type_name_case r) with
      | Some (t, _) ->
          Some t
      | _ ->
          if not @@ is_type r then None
          else
            Some
              (Ast_builder.Default.ptyp_constr ~loc
                 (Loc.make ~loc (Astlib.Longident.parse @@ type_name_case r))
                 [] ) )
    | _ ->
        None
  in
  match typeof_ ~loc params expr with Some t -> t | _ -> [%type: unit]

let expand ~ctxt filename =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let content = filename |> read_file in
  let tlb = Tlb.parse ~filename content in
  let content = Ast_builder.Default.estring ~loc content in
  let filename = Ast_builder.Default.estring ~loc filename in
  let _src = [%stri let source = [%e content]] in
  let _src_file = [%stri let source_filename = [%e filename]] in
  let tlb =
    List.map tlb.decls ~f:(fun x -> (x.combinator.combinator_ident, x))
    |> List.Assoc.sort_and_group ~compare:String.compare
  in
  let types =
    List.map tlb ~f:(fun (name, decls) ->
        let name' = type_name_case name in
        let name = Loc.make ~loc name' in
        let params =
          let h = Hashtbl.create (module String) in
          List.iter decls ~f:(fun decl ->
              List.iter decl.combinator_fields ~f:(function
                | ImplicitField {field_ident; field_type = `Type} ->
                    Hashtbl.set h
                      ~key:(type_name_case field_ident)
                      ~data:
                        ( Ast_builder.Default.ptyp_var ~loc
                          @@ type_name_case field_ident,
                          (NoVariance, NoInjectivity) )
                | _ ->
                    () ) ) ;
          h
        in
        Ast_builder.Default.(
          let labels (fields : Tlb.Syntax.field list) =
            List.foldi fields ~init:[] ~f:(fun i acc -> function
              | NamedField {field_ident; field_expr} ->
                  label_declaration ~loc
                    ~name:(Loc.make ~loc @@ type_name_case field_ident)
                    ~mutable_:Immutable
                    ~type_:(typeof ~loc tlb params field_expr)
                  :: acc
              | AnonymousField {field_expr} ->
                  label_declaration ~loc
                    ~name:(Loc.make ~loc ("_" ^ Int.to_string i))
                    ~mutable_:Immutable
                    ~type_:(typeof ~loc tlb params field_expr)
                  :: acc
              | ExprField field_expr ->
                  label_declaration ~loc
                    ~name:(Loc.make ~loc ("_" ^ Int.to_string i))
                    ~mutable_:Immutable
                    ~type_:(typeof ~loc tlb params field_expr)
                  :: acc
              | _ ->
                  acc )
            |> List.rev
          in
          let args fields =
            match labels fields with
            | [] ->
                Pcstr_tuple []
            | labels ->
                Pcstr_record labels
          in
          let variants =
            List.filter_mapi decls ~f:(fun i -> function
              | { combinator_constructor = Constructor ident;
                  combinator_fields;
                  _ }
              | { combinator_constructor = TaggedConstructor (ident, _);
                  combinator_fields;
                  _ } ->
                  Some
                    (constructor_declaration ~loc
                       ~name:(Loc.make ~loc @@ upper_snake_case ident)
                       ~args:(args combinator_fields) ~res:None )
              | { combinator_constructor = AnonymousConstructor _;
                  combinator_fields;
                  _ } ->
                  Some
                    (constructor_declaration ~loc
                       ~name:
                         ( Loc.make ~loc @@ upper_snake_case
                         @@ upper_snake_case
                              ( name'
                              ^
                              (* Tag multiple anonymous constructors with their index *)
                              if List.length decls > 1 then Int.to_string i
                              else "" ) )
                       ~args:(args combinator_fields) ~res:None ) )
          in
          type_declaration ~loc ~name ~params:(Hashtbl.data params) ~cstrs:[]
            ~kind:(Ptype_variant variants) ~private_:Public ~manifest:None ) )
  in
  let encoders =
    Ast_builder.Default.(
      List.map tlb ~f:(fun (name, decls) ->
          let name = type_name_case name in
          let type_encoder =
            value_binding ~loc
              ~pat:(ppat_var ~loc (Loc.make ~loc @@ "encode_" ^ name))
              ~expr:
                ( pexp_function ~loc
                @@ List.mapi decls ~f:(fun i decl ->
                      (* Variant identifier *)
                       let ident =
                         upper_snake_case
                         @@
                         match decl with
                         | {combinator_constructor = Constructor ident; _}
                         | { combinator_constructor =
                               TaggedConstructor (ident, _);
                             _ } ->
                             ident
                         | {combinator_constructor = AnonymousConstructor _; _}
                           ->
                             name
                             ^
                             if List.length decls > 1 then Int.to_string i
                             else ""
                       in
                       let has_fields = List.find decl.combinator_fields ~f:(function
                           | ImplicitField _ -> false
                           | _ -> true) |> Option.is_some in
                       let pat = if has_fields then Some (ppat_any ~loc) else None in
                       case
                         ~lhs:(ppat_construct ~loc (Loc.make ~loc @@ Longident.parse ident) pat)
                         ~guard:None ~rhs:(eint ~loc 1) ) )
          in
          let variant_encoders = [] in
          type_encoder :: variant_encoders )
      |> List.concat )
  in
  let preamble =
    ( List.range 0 257 ~start:`exclusive ~stop:`inclusive
    |> List.map ~f:(fun i ->
           Ast_builder.Default.(
             pstr_type ~loc Recursive
               [ type_declaration ~loc
                   ~name:(Loc.make ~loc ("uint" ^ Int.to_string i))
                   ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
                   ~manifest:None ] ) ) )
    @ ( List.range 0 257 ~start:`exclusive ~stop:`inclusive
      |> List.map ~f:(fun i ->
             Ast_builder.Default.(
               pstr_type ~loc Recursive
                 [ type_declaration ~loc
                     ~name:(Loc.make ~loc ("int" ^ Int.to_string i))
                     ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
                     ~manifest:None ] ) ) )
    @ ( List.range 0 1023 ~start:`exclusive ~stop:`inclusive
      |> List.map ~f:(fun i ->
             Ast_builder.Default.(
               pstr_type ~loc Recursive
                 [ type_declaration ~loc
                     ~name:(Loc.make ~loc ("bits" ^ Int.to_string i))
                     ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
                     ~manifest:None ] ) ) )
    @ [%str
        type bits

        type cell

        type uint

        type any]
  in
  let t = Ast_builder.Default.pstr_type ~loc Recursive types in
  let encoder = Ast_builder.Default.pstr_value ~loc Nonrecursive encoders in
  let str =
    [%str
      [%%i t]

      [%%i encoder]]
  in
  { pmod_desc = Pmod_structure (preamble @ str);
    pmod_loc = loc;
    pmod_attributes =
      [ Ast_builder.Default.(
          attribute ~loc ~name:(Loc.make ~loc "warning")
            ~payload:(PStr [pstr_eval ~loc (estring ~loc "-30") []]) ) ] }

let ext =
  Extension.V3.declare "tlb" Extension.Context.module_expr
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension ext

let () = Driver.register_transformation ~rules:[rule] "tlb"

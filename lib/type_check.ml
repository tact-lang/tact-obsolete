open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    open Interpreter.Make (Config)

    open Builtin.Make (Config)

    open Errors

    type type_check_error = TypeError of type_ | NeedFromCall of expr

    class ['s] remover_of_resolved_reference =
      object (self : 's)
        inherit ['s] map as super

        method! visit_ResolvedReference env (_, ex) =
          (self#visit_expr env ex).value

        method! visit_type_ env =
          function
          | ExprType {value = Value (Type t); _} ->
              self#visit_type_ env t
          | ty ->
              super#visit_type_ env ty
      end

    let is_sig_part_of sign1 sign2 ~equal_ty =
      let remover = new remover_of_resolved_reference in
      let sign1 = remover#visit_struct_sig () sign1 in
      let sign2 = remover#visit_struct_sig () sign2 in
      let is_part = ref true in
      List.iter sign2.st_sig_fields ~f:(fun (name2, ty2) ->
          if
            not
              (List.exists sign1.st_sig_fields ~f:(fun (name1, ty1) ->
                   equal_string name1.value name2.value && equal_ty ty1 ty2 ) )
          then is_part := false ) ;
      !is_part

    let is_union_sig_part_of sign1 sign2 =
      let remover = new remover_of_resolved_reference in
      let sign1 = remover#visit_union_sig () sign1 in
      let sign2 = remover#visit_union_sig () sign2 in
      let is_part = ref true in
      List.iter sign2.un_sig_cases ~f:(fun ty2 ->
          if not (List.exists sign1.un_sig_cases ~f:(equal_type_ ty2)) then
            is_part := false ) ;
      !is_part

    class type_checker (errors : _) (functions : _) =
      object (self)
        val mutable fn_returns : type_ option = None

        method check_return_type ~program ~current_bindings actual =
          match fn_returns with
          | Some fn_returns' -> (
            match
              self#check_type actual ~program ~current_bindings
                ~expected:fn_returns'
            with
            | Ok ty ->
                fn_returns <- Some ty ;
                Ok ty
            | v ->
                v )
          | None ->
              ice "Caller should guarantee this"

        method get_fn_returns =
          match fn_returns with
          | Some x ->
              x
          | None ->
              ice "Caller should guarantee this"

        method with_fn_returns
            : 'env 'a. 'env -> type_ -> ('env -> 'a) -> 'a * type_ =
          fun env ty f ->
            let prev = fn_returns in
            fn_returns <- Some ty ;
            let result = f env in
            let new_fn_returns = self#get_fn_returns in
            fn_returns <- prev ;
            (result, new_fn_returns)

        method check_type ~program ~current_bindings ~expected
            ?(actual_ty = None) actual_value =
          let actual =
            Option.value_or_thunk actual_ty ~default:(fun _ ->
                type_of program actual_value )
          in
          let remover = new remover_of_resolved_reference in
          let actual' = remover#visit_type_ () actual in
          let expected' = remover#visit_type_ () expected in
          let is_sig_part_of_call sign_actual sign_expected =
            is_sig_part_of sign_actual sign_expected ~equal_ty:(fun ty1 ty2 ->
                equal_expr ty1 ty2 )
          in
          match expected with
          | HoleType ->
              Ok actual
          | _ when equal_type_ HoleType actual' ->
              Ok expected
          | _ when equal_type_ expected' actual' ->
              Ok actual
          | StructSig sign_expected -> (
              let sign_expected =
                Arena.get program.struct_signs sign_expected
              in
              match actual' with
              | StructType sid ->
                  let s = Program.get_struct program sid in
                  let sign_actual = sig_of_struct s 0 in
                  if is_sig_part_of_call sign_actual sign_expected then
                    Ok (StructType sid)
                  else Error (TypeError expected)
              | ExprType {value = Reference (_, StructSig sid2); _} ->
                  let sign_actual = Arena.get program.struct_signs sid2 in
                  if is_sig_part_of_call sign_actual sign_expected then
                    Ok actual'
                  else Error (TypeError expected)
              | ExprType ({value = FunctionCall _; _} as ex_ty) -> (
                  let ex_ty = type_of program ex_ty in
                  match ex_ty with
                  | StructSig sid2 ->
                      let sign_actual = Arena.get program.struct_signs sid2 in
                      if is_sig_part_of_call sign_actual sign_expected then
                        Ok actual'
                      else Error (TypeError expected)
                  | _ ->
                      Error (TypeError expected) )
              | _ ->
                  Error (TypeError expected) )
          | UnionSig sign_expected -> (
              let sign_expected = Arena.get program.union_signs sign_expected in
              match actual' with
              | UnionType sid ->
                  let s = Program.get_union program sid in
                  let sign_actual = sig_of_union s in
                  if is_union_sig_part_of sign_actual sign_expected then
                    Ok (UnionType sid)
                  else Error (TypeError expected)
              | ExprType {value = Reference (_, UnionSig sid2); _}
              | UnionSig sid2 ->
                  let sign_actual = Arena.get program.union_signs sid2 in
                  if is_union_sig_part_of sign_actual sign_expected then
                    Ok (UnionSig sid2)
                  else Error (TypeError expected)
              | _ ->
                  Error (TypeError expected) )
          | TypeN 0 -> (
            match actual_value.value with
            | ResolvedReference (_, {value = Value (Type (StructSig s)); _})
            | Value (Type (StructSig s)) ->
                Ok (StructSig s)
            | _ -> (
              match type_of program actual_value with
              | (StructSig _ as ty) | (UnionSig _ as ty) ->
                  Ok ty
              | _ ->
                  Error (TypeError expected) ) )
          | (StructType _ as ty) | (UnionType _ as ty) -> (
              let from_intf_ =
                let inter =
                  new interpreter (make_ctx program current_bindings functions)
                    errors actual_value.span (fun _ f -> f)
                in
                Value
                  (inter#interpret_fc
                     ( from_intf,
                       [{value = Value (Type actual); span = actual_value.span}]
                     ) )
              in
              let impl =
                Program.get_uty_details program ty
                |> Option.value_exn
                |> fun f ->
                f.uty_impls
                |> List.find_map ~f:(fun i ->
                       if
                         equal_expr_kind
                           (Value (Type (InterfaceType i.impl_interface)))
                           from_intf_
                       then Some i.impl_methods
                       else None )
                |> Option.bind ~f:List.hd
              in
              match impl with
              | Some (_, m) ->
                  Error
                    (NeedFromCall {value = Value (Function m); span = m.span})
              | _ ->
                  Error (TypeError expected) )
          | InterfaceType v -> (
            match actual_value.value with
            | ResolvedReference (_, {value = Value (Type t); _}) | Value (Type t)
              -> (
              match Program.find_impl_intf program v t with
              | Some _ ->
                  Ok t
              | _ ->
                  Error (TypeError expected) )
            | _ ->
                Error (TypeError expected) )
          | ExprType ex ->
              self#check_type ~expected:(type_of program ex) ~program
                ~current_bindings actual_value
          | _otherwise ->
              Error (TypeError expected)
      end
  end

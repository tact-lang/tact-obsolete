open Base
open Lang_types
open Interpreter
open Builtin
open Errors

type type_check_error = TypeError of type_ | NeedFromCall of expr

class ['s] remover_of_resolved_reference =
  object (self : 's)
    inherit ['s] map

    method! visit_ResolvedReference env (_, ex) = self#visit_expr env ex
  end

let is_sig_part_of sign1 sign2 =
  let remover = new remover_of_resolved_reference in
  let sign1 = remover#visit_struct_sig () sign1 in
  let sign2 = remover#visit_struct_sig () sign2 in
  let is_part = ref true in
  List.iter sign2.st_sig_fields ~f:(fun (name2, ty2) ->
      if
        not
          (List.exists sign1.st_sig_fields ~f:(fun (name1, ty1) ->
               equal_string name1 name2 && equal_expr ty1 ty2 ) )
      then is_part := false ) ;
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
          raise InternalCompilerError

    method get_fn_returns =
      match fn_returns with Some x -> x | None -> raise InternalCompilerError

    method with_fn_returns
        : 'env 'a. 'env -> type_ -> ('env -> 'a) -> 'a * type_ =
      fun env ty f ->
        let prev = fn_returns in
        fn_returns <- Some ty ;
        let result = f env in
        let new_fn_returns = self#get_fn_returns in
        fn_returns <- prev ;
        (result, new_fn_returns)

    method check_type ~program ~current_bindings ~expected actual_value =
      let actual = type_of actual_value in
      let remover = new remover_of_resolved_reference in
      let actual' = remover#visit_type_ () actual in
      let expected' = remover#visit_type_ () expected in
      match expected with
      | HoleType ->
          Ok actual
      | _ when equal_type_ HoleType actual' ->
          Ok expected
      | _ when equal_type_ expected' actual' ->
          Ok actual
      | StructSig sign_expected -> (
          let sign_expected = Arena.get program.struct_signs sign_expected in
          match actual' with
          | StructType sid ->
              let s = Program.get_struct program sid in
              let sign_actual = sig_of_struct s in
              if is_sig_part_of sign_actual sign_expected then
                Ok (StructType sid)
              else Error (TypeError expected)
          | _ ->
              Error (TypeError expected) )
      | TypeN 0 -> (
        match actual' with
        | StructSig s ->
            Ok (StructSig s)
        | _ ->
            Error (TypeError expected) )
      | StructType s -> (
          let from_intf_ =
            let inter =
              new interpreter (program, current_bindings, errors, functions)
                (fun _ _ _ _ f -> f)
            in
            print_sexp (sexp_of_type_ expected) ;
            print_sexp (sexp_of_type_ actual) ;
            Value (inter#interpret_fc (from_intf, [Value (Type actual)]))
          in
          let impl =
            (List.Assoc.find_exn program.structs s ~equal:equal_int)
              .struct_impls
            |> List.find_map ~f:(fun i ->
                   if
                     equal_expr (Value (Type (InterfaceType i.impl_interface)))
                       from_intf_
                   then Some i.impl_methods
                   else None )
            |> Option.bind ~f:List.hd
          in
          match impl with
          | Some (_, m) ->
              Error (NeedFromCall (Value (Function m)))
          | _ ->
              Error (TypeError expected) )
      | UnionType u -> (
          let from_intf_ =
            let inter =
              new interpreter (program, current_bindings, errors, functions)
                (fun _ _ _ _ f -> f)
            in
            Value (inter#interpret_fc (from_intf, [Value (Type actual)]))
          in
          let impl =
            (List.Assoc.find_exn program.unions u ~equal:equal_int).union_impls
            |> List.find_map ~f:(fun i ->
                   if
                     equal_expr (Value (Type (InterfaceType i.impl_interface)))
                       from_intf_
                   then Some i.impl_methods
                   else None )
            |> Option.bind ~f:List.hd
          in
          match impl with
          | Some (_, m) ->
              Error (NeedFromCall (Value (Function m)))
          | _ ->
              Error (TypeError expected) )
      | InterfaceType v -> (
        match actual_value with
        | ResolvedReference (_, Value (Type t)) | Value (Type t) -> (
          match Program.find_impl_intf program v t with
          | Some _ ->
              Ok t
          | _ ->
              Error (TypeError expected) )
        | _ ->
            Error (TypeError expected) )
      | _otherwise ->
          Error (TypeError expected)
  end

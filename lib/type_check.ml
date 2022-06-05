open Base
open Lang_types
open Interpreter
open Builtin
open Errors

type type_check_error = TypeError of expr | NeedFromCall of expr

class type_checker (errors : _) (functions : _) =
  object (self)
    val mutable fn_returns = None

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
      match fn_returns with Some x -> x | None -> Value (Type HoleType)

    method with_fn_returns : 'env 'a. 'env -> expr -> ('env -> 'a) -> 'a =
      fun env ty f ->
        let prev = fn_returns in
        fn_returns <- Some ty ;
        let result = f env in
        fn_returns <- prev ;
        result

    method check_type ~program ~current_bindings ~expected actual =
      match expected with
      | Value (Type HoleType) ->
          Ok actual
      | _ when equal_expr expected actual ->
          Ok actual
      | Value x -> (
          let from_intf_ =
            let inter =
              new interpreter (program, current_bindings, errors, functions)
            in
            Value (inter#interpret_fc (from_intf, [actual]))
          in
          let impl =
            List.find_map program.impls ~f:(fun (s, impls) ->
                match equal_value s x with
                | true ->
                    List.find_map impls ~f:(fun i ->
                        if equal_expr i.impl_interface from_intf_ then
                          Some i.impl_methods
                        else None )
                    |> Option.bind ~f:List.hd
                | false ->
                    None )
          in
          match impl with
          | Some (_, m) ->
              Error (NeedFromCall m)
          | _ ->
              Error (TypeError expected) )
      | _ ->
          Error (TypeError expected)
  end

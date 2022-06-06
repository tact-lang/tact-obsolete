open Base
open Lang_types
open Interpreter
open Builtin
open Errors

type type_check_error = TypeError of type_ | NeedFromCall of expr

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
      match fn_returns with Some x -> x | None -> HoleType

    method with_fn_returns : 'env 'a. 'env -> type_ -> ('env -> 'a) -> 'a =
      fun env ty f ->
        let prev = fn_returns in
        fn_returns <- Some ty ;
        let result = f env in
        fn_returns <- prev ;
        result

    method check_type ~program ~current_bindings ~expected actual =
      match expected with
      | HoleType ->
          Ok actual
      | _ when equal_type_ expected actual ->
          Ok actual
      | x -> (
          let from_intf_ =
            let inter =
              new interpreter (program, current_bindings, errors, functions)
            in
            Value (inter#interpret_fc (from_intf, [Value (Type actual)]))
          in
          let impl =
            List.find_map program.impls ~f:(fun (s, impls) ->
                match equal_value s (Type x) with
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
  end

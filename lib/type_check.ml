open Base
open Lang_types
open Interpreter
open Builtin
open Errors

type type_check_error = TypeError of type_ | NeedFromCall of expr

class type_checker (errors : _) (functions : _) =
  object (self)
    val mutable fn_returns : type_ option = None

    method check_return_type ~program ~current_bindings ?self:(self_ = None)
        actual =
      match fn_returns with
      | Some fn_returns' -> (
        match
          self#check_type actual ~program ~current_bindings ~self:self_
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

    method with_fn_returns
        : 'env 'a. 'env -> type_ -> ('env -> 'a) -> 'a * type_ =
      fun env ty f ->
        let prev = fn_returns in
        fn_returns <- Some ty ;
        let result = f env in
        let new_fn_returns = self#get_fn_returns in
        fn_returns <- prev ;
        (result, new_fn_returns)

    method check_type ~program ~current_bindings ~expected ?self:(self_ = None)
        actual =
      match expected with
      | HoleType ->
          Ok actual
      | SelfType -> (
          if equal_type_ actual SelfType then Ok SelfType
          else
            match self_ with
            | Some (Value (Type t)) ->
                if equal_type_ actual t then Ok t else Error (TypeError SelfType)
            | _ ->
                Error (TypeError SelfType) )
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
            List.find_map (impls_of x) ~f:(fun i ->
                if equal_expr i.impl_interface from_intf_ then
                  Some i.impl_methods
                else None )
            |> Option.bind ~f:List.hd
          in
          match impl with
          | Some (_, m) ->
              Error (NeedFromCall m)
          | _ ->
              Error (TypeError expected) )
  end

open Base

module Make =
functor
  (Syntax : Syntax.T)
  ->
  struct
    open Errors
    include Lang.Make (Syntax)

    class ['s] evaluator ((program, errors) : program * _ errors) =
      object (s : 's)
        inherit ['s] map as super

        method! visit_program env program =
          (* process bindings first *)
          let program' = super#visit_program env {program with stmts = []} in
          (* process statements as a second step *)
          super#visit_program env {program' with stmts = program.stmts}

        method! visit_FunctionCall env ((fn, args), result) =
          match !result with
          | Some t ->
              t
          | None -> (
              let args = s#visit_list s#visit_term env args in
              match fn with
              | Function (BuiltinFn {function_impl; _})
                when are_immediate_arguments args ->
                  function_impl program args
              | Function (Fn {function_params; function_impl = Some impl; _})
                when are_immediate_arguments args
                     && equal (List.length function_params) (List.length args)
                -> (
                  let bindings =
                    List.zip_exn function_params args
                    |> List.map ~f:(fun ((name, _), term) -> (name, term))
                  in
                  let rec interpret ?(return = InvalidTerm) bindings stmts =
                    let e : 's =
                      new evaluator
                        ( {program with bindings = bindings @ program.bindings},
                          errors )
                    in
                    match stmts with
                    | [] ->
                        `Stop return
                    | Invalid :: _ ->
                        `Stop InvalidTerm
                    | Term t :: rest ->
                        let t = eval_term bindings @@ e#visit_term env t in
                        interpret bindings rest ~return:t
                    | Break t :: _ ->
                        interpret bindings [t]
                    | Return t :: _ ->
                        `Stop (eval_term bindings t)
                    | Let let_bindings :: rest ->
                        let let_bindings =
                          List.map let_bindings ~f:(fun (name, binding) ->
                              (name, eval_term bindings binding) )
                        in
                        interpret (let_bindings @ bindings) rest
                  and eval_term bindings term =
                    (new reference_resolver (bindings, errors))#visit_term ()
                      term
                  in
                  match interpret bindings impl with
                  | `Stop term ->
                      result := Some term ;
                      term
                  | _ ->
                      InvalidTerm )
              | _ ->
                  FunctionCall ((fn, args), result) )
      end
  end

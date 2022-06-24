type t = [`NULL | `ISNULL] [@@deriving sexp_of, sexp]

let execute vm = function
  | `NULL ->
      Vm.push Null vm
  | `ISNULL -> (
      let value, vm = Vm.pop vm in
      match value with
      | Null ->
          Vm.push Vm.const_true vm
      | _ ->
          Vm.push Vm.const_false vm )

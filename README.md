# Tact

## Building from source

* Install `opam` using this [guide](https://ocaml.org/learn/tutorials/up_and_running.html)
* Run `opam switch create $(pwd) -y`
* Run `dune build && dune install`

You can now use `tact /path/to/file.tact`

### Updating dependencies

Run `opam install tact --working-dir`

### Running REPL

Run `dune utop`


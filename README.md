# Dependencies:

You'll need OCaml installed as well as the `dune`, `ocamlgraph` and `menhir` packages.
The easiest way to install these is through the [`opam` package manager](https://opam.ocaml.org).

# Building/Running:

```
$ dune build src/optimize.exe
$ _build/default/src/optimize.exe -i <input_file> -o <output_file>
# Or $ dune exec src/optimize-exe -- -i <input_file> -o <output_file>
```

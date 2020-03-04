(lang dune 2.1)
(using menhir 2.0)

(name tigerIR)
(version fe338d4-dirty)
(license ISC)
(authors "Ohad Rau" "Matthew Sklar")
(maintainers "orau3@gatech.edu" "msklar3@gatech.edu")
(generate_opam_files true)

(package
  (name tigerIR)
  (description "TigerIR")
  (depends
    (dune (>= 2.1))
    (ocamlgraph (>= 1.8.8))
    (menhir (>= 2000211))))

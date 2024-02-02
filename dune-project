(lang dune 3.13)

(name ComputationTheory)

(generate_opam_files true)

(source
 (github ofsouzap/ComputationTheory))

(authors "Ofsouzap")

(maintainers "Ofsouzap")

(license MIT)

(package
 (name ComputationTheory)
 (synopsis "Computation Theory stuff code")
 (depends ocaml dune zarith alcotest fmt qcheck qcheck-alcotest))

(lang dune 3.8)
(using coq 0.8)
(generate_opam_files true)

(name Cadeque)
(source (github JulesViennotFrance/VerifiedCatenableDeque))
(license MIT)
(authors "Cambium, Inria")
(maintainers "jules.viennot@gmail.com")
(version 0.1)

(package
  (name Cadeque)
  (allow_empty)
  (depends (ocaml (>= "4.08"))))
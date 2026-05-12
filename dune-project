(lang dune 3.23)
(using rocq 0.13)
(generate_opam_files true)

(name Cadeque)
(source (github JulesViennotFrance/VerifiedCatenableDeque))
(license MIT)
(authors "Cambium, Inria")
(maintainers "jules.viennot@gmail.com")
(version 0.1)

(package
  (name deques)
  (allow_empty)
  (depends (ocaml (>= "4.08")))
)

(package
  (name rocq-deques)
  (depends
    (rocq (= "9.2.0"))
    rocq-aac-tactics
    coq-hammer-tactics
    rocq-equations
  )
)

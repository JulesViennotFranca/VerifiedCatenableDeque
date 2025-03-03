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
  (name deques)
  (allow_empty)
  (depends (ocaml (>= "4.08"))))

(package
  (name coq-deques)
  (depends
    (coq (= "8.19.0"))
    (coq-aac-tactics (>= "8.19.1"))
    (coq-hammer-tactics (>= "1.3.2+8.19"))
    (coq-equations (>= "1.3+8.19"))))

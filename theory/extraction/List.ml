open Datatypes

(** val concat : 'a1 list list -> 'a1 list **)

let rec concat = function
| Coq_nil -> Coq_nil
| Coq_cons (x, l0) -> app x (concat l0)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Coq_nil -> Coq_nil
| Coq_cons (a, t) -> Coq_cons ((f a), (map f t))

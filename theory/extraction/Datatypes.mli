
type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) sum =
| Coq_inl of 'a
| Coq_inr of 'b

type ('a, 'b) prod =
| Coq_pair of 'a * 'b

val fst : ('a1, 'a2) prod -> 'a1

val snd : ('a1, 'a2) prod -> 'a2

type 'a list =
| Coq_nil
| Coq_cons of 'a * 'a list

val app : 'a1 list -> 'a1 list -> 'a1 list

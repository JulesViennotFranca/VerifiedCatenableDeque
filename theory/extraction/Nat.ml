open Datatypes

(** val pred : nat -> nat **)

let pred n = match n with
| O -> n
| S u -> u

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val iter : nat -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let rec iter n f x =
  match n with
  | O -> x
  | S n0 -> f (iter n0 f x)

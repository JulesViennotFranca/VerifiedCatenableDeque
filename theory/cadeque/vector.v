From Coq Require Import Program List.
Import ListNotations.
From Equations Require Import Equations.

(* A type for vectors. *)
Inductive vector (A : Type) : nat -> Type :=
| V0 {u : nat} : vector A u
| V1 {u : nat} : A -> vector A (1 + u)
| V2 {u : nat} : A -> A -> vector A (2 + u)
| V3 {u : nat} : A -> A -> A -> vector A (3 + u)
| V4 {u : nat} : A -> A -> A -> A -> vector A (4 + u)
| V5 {u : nat} : A -> A -> A -> A -> A -> vector A (5 + u)
| V6 {u : nat} : A -> A -> A -> A -> A -> A -> vector A (6 + u).
Arguments V0 {A u}.
Arguments V1 {A u}.
Arguments V2 {A u}.
Arguments V3 {A u}.
Arguments V4 {A u}.
Arguments V5 {A u}.
Arguments V6 {A u}.

Set Equations Transparent.

(* Returns the sequence associated to a vector. *)
Equations vector_seq {A u} : vector A u -> list A :=
vector_seq V0 := [];
vector_seq (V1 a) := [a];
vector_seq (V2 a b) := [a] ++ [b];
vector_seq (V3 a b c) := [a] ++ [b] ++ [c];
vector_seq (V4 a b c d) := [a] ++ [b] ++ [c] ++ [d];
vector_seq (V5 a b c d e) := [a] ++ [b] ++ [c] ++ [d] ++ [e];
vector_seq (V6 a b c d e f) := [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f].

(* Returns the number of elements contained in a vector. *)
Equations vector_size {A u} (v : vector A u) : nat :=
vector_size V0 := 0;
vector_size (V1 _) := 1;
vector_size (V2 _ _) := 2;
vector_size (V3 _ _ _) := 3;
vector_size (V4 _ _ _ _) := 4;
vector_size (V5 _ _ _ _ _) := 5;
vector_size (V6 _ _ _ _ _ _) := 6.

Unset Equations Transparent.

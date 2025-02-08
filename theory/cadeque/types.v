From Cadeque.color Require Import GYOR.
From Cadeque.cadeque Require Import buffer.

(* A level is a natural integer. *)
Definition level := nat.

(* A size is a natural integer. *)
Definition size := nat.

(* A kind is only, left, or right. *)
Inductive kind : Type := only | left | right.

(* An arity is 0, 1, or 2. *)
Notation arity  := nat.
Notation empty  := 0.
Notation single := 1.
Notation pair   := 2.

Derive NoConfusion for kind.

(* Types for general prefixes and suffixes, they are simply other names for
   buffer.t. In the following, prefixes and suffixes will contain stored
   triples. *)

Definition prefix' := buffer.t.
Definition suffix' := buffer.t.

(* A type for the coloring relation of nodes. *)
Inductive node_coloring : size -> size -> arity -> color -> Type :=
  | GN {qp qs a} : node_coloring (3 + qp) (3 + qs) (S a) green
  | YN {qp qs a} : node_coloring (2 + qp) (2 + qs) (S a) yellow
  | ON {qp qs a} : node_coloring (1 + qp) (1 + qs) (S a) orange
  | RN {qp qs a} : node_coloring (0 + qp) (0 + qs) (S a) red
  | EN {qp qs}   : node_coloring (0 + qp) (0 + qs) 0     green.

(* A type for general nodes, in the following, they will contain stored
   triples. *)
Inductive node' (A : Type) : arity -> kind -> color -> Type :=
  | Only_end {q} :
      prefix' A (S q) ->
      node' A 0 only green
  | Only {qp qs a C} :
      node_coloring qp qs (S a) C ->
      prefix' A (5 + qp) ->
      suffix' A (5 + qs) ->
      node' A (S a) only C
  | Left {qp qs a C} :
      node_coloring qp qs a C ->
      prefix' A (5 + qp) ->
      A * A ->
      node' A a left C
  | Right {qp qs a C} :
      node_coloring qp qs a C ->
      A * A ->
      suffix' A (5 + qs) ->
      node' A a right C.
Arguments Only_end {A q}.
Arguments Only {A qp qs a C}.
Arguments Left {A qp qs a C}.
Arguments Right {A qp qs a C}.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> color -> Type :=
  | G {lC rC : color} : regularity green lC    rC
  | R                 : regularity red   green green.

(* The computation of elimination schemes is disabled. Elimination schemes are
   not necessary in the code and their definition takes ar long time to compute
   for the following mutually recursive types. *)
Unset Elimination Schemes.

(* A type for stored triples. *)
Inductive stored (A : Type) : level -> Type :=
  | Ground :
      A -> stored A 0
  | Big {l qp qs a lC rC} :
      prefix' (stored A l) (3 + qp) ->
      chain A (S l) a only lC rC ->
      suffix' (stored A l) (3 + qs) ->
      stored A (S l)
  | Small {l q} :
      suffix' (stored A l) (3 + q) ->
      stored A (S l)

(* A type for bodies. *)
with body (A : Type) : level -> level -> kind -> kind -> Type :=
  | Hole {l k} :
      body A l l k k
  | Single_child {hl tl hk tk y o}:
      node' (stored A hl) 1 hk (Mix NoGreen y o NoRed) ->
      body A (S hl) tl only tk ->
      body A hl tl hk tk
  | Pair_yellow {hl tl hk tk C} :
      node' (stored A hl) 2 hk yellow ->
      body A (S hl) tl left tk ->
      chain A (S hl) single right C C ->
      body A hl tl hk tk
  | Pair_orange {hl tl hk tk} :
      node' (stored A hl) 2 hk orange ->
      chain A (S hl) single left green green ->
      body A (S hl) tl right tk ->
      body A hl tl hk tk

(* A type for packets. *)
with packet (A : Type) : level -> level -> arity -> kind -> color -> Type :=
  | Packet {hl tl a hk tk g r} :
      body A hl tl hk tk ->
      node' (stored A tl) a tk (Mix g NoYellow NoOrange r) ->
      packet A hl (S tl) a hk (Mix g NoYellow NoOrange r)

(* A type for chains. *)
with chain (A : Type) : level -> arity -> kind -> color -> color -> Type :=
  | Empty {l k lC rC} :
      chain A l empty k lC rC
  | Single {hl tl a k C lC rC} :
      regularity C lC rC ->
      packet A hl tl a k C ->
      chain A tl a only lC rC ->
      chain A hl single k C C
  | Pair {l k lC rC} :
      chain A l single left lC lC ->
      chain A l single right rC rC ->
      chain A l pair k lC rC.

Arguments Ground {A}.
Arguments Big {A l qp qs a lC rC}.
Arguments Small {A l q}.

Arguments Hole {A l k}.
Arguments Single_child {A hl tl hk tk y o}.
Arguments Pair_yellow {A hl tl hk tk C}.
Arguments Pair_orange {A hl tl hk tk}.

Arguments Packet {A hl tl a hk tk g r}.

Arguments Empty {A l k lC rC}.
Arguments Single {A hl tl a k C lC rC}.
Arguments Pair {A l k lC rC}.

(* Types for prefixes, suffixes, and nodes containing stored triples. *)

Definition prefix (A : Type) (l : level) := prefix' (stored A l).
Definition suffix (A : Type) (l : level) := suffix' (stored A l).
Definition node   (A : Type) (l : level) := node'   (stored A l).

(* A type for green buffers, buffers of at least eight elements. *)
Inductive green_buffer (A : Type) (l : level) : Type :=
  | Gbuf {q} :
      buffer.t (stored A l) (8 + q) -> green_buffer A l.
Arguments Gbuf {A l q}.

(* A type for stored buffers, buffers of at least three elements. *)
Inductive stored_buffer (A : Type) (l : level) : Type :=
  | Sbuf {q} :
      buffer.t (stored A l) (3 + q) -> stored_buffer A l.
Arguments Sbuf {A l q}.

(* A type for the coloring relation of triples. *)
Inductive triple_coloring : color -> arity -> color -> color -> color -> Type :=
  | GT {ar lC rC} : triple_coloring green  ar      lC    rC    green
  | YT {ar lC rC} : triple_coloring yellow (S ar)  lC    rC    lC
  | OST {C}       : triple_coloring orange single  C     C     C
  | OPT {rC}      : triple_coloring orange pair    green rC    rC
  | RT {ar}       : triple_coloring red    (S ar)  green green red.

(* A type for triples. *)
Inductive triple : Type -> level -> kind -> color -> Type :=
  | Triple {A : Type} {l : level} {a : arity}
           {k : kind} {nC lC rC C : color} :
      triple_coloring nC a lC rC C ->
      node A l a k nC ->
      chain A (S l) a only lC rC ->
      triple A l k C.

(* A type for left or right triples. *)
Inductive left_right_triple : Type -> level -> kind -> color -> Type :=
  | Not_enough {A : Type} {l : level} {k : kind} {C : color} :
      vector (stored A l) 6 ->
      left_right_triple A l k C
  | Ok_lrt {A : Type} {l : level} {k : kind} {C : color} :
      triple A l k C -> left_right_triple A l k C.

(* A type for a tuple of six stored triples. *)
Definition six_stored (A : Type) (l : level) : Type :=
  stored A l * stored A l * stored A l *
  stored A l * stored A l * stored A l.

(* A type for partial triples. *)
Inductive partial_triple : Type -> level -> arity -> kind -> Type :=
  | Zero_element {A : Type} {l : level} {k : kind} :
      partial_triple A l single k
  | Six_elements {A : Type} {l : level} {k : kind} :
      six_stored A l -> partial_triple A l pair k
  | Ok_pt {A : Type} {l : level} {ar : arity} {k : kind} {C : color} :
      triple A l k C -> partial_triple A l ar k.

(* A general sandwich type. *)
Inductive sandwich : Type -> Type -> Type :=
  | Alone {A B : Type} : A -> sandwich A B
  | Sandwich {A B : Type} : A -> B -> A -> sandwich A B.

(* A type for semi-regular cadeques. *)
Inductive semi_cadeque : Type -> level -> Type :=
  | Semi {A : Type} {l : level} {a : arity} {lC rC : color} :
      chain A l a only lC rC -> semi_cadeque A l.

(* A type for cadeques. *)
Inductive cadeque : Type -> Type :=
  | T {A a} : chain A 0 a only green green -> cadeque A.

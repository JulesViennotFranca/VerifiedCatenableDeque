From Deques.color Require Import GYOR.
From Deques.deque Require Import deque.

(* An arity is 0, 1, or 2. *)
Notation arity  := nat.
Notation empty  := 0.
Notation single := 1.
Notation pair   := 2.

(* A kind is only, left, or right. *)
Inductive kind : Type := only | left | right.

(* A level is a natural integer. *)
Definition level := nat.

(* A size is a natural integer. *)
Definition size := nat.

Derive NoConfusion for kind.

(* Types for general prefixes and suffixes, they are simply other names for
   deques. In the following, prefixes and suffixes will contain stored
   triples. *)

Definition prefix' := deque.
Definition suffix' := deque.

(* A type for the coloring relation of nodes. *)
Inductive node_coloring : size -> size -> arity -> color -> Type :=
  | EN {qp qs}    : node_coloring (0 + qp) (0 + qs) 0      green
  | GN {qp qs ar} : node_coloring (3 + qp) (3 + qs) (S ar) green
  | YN {qp qs ar} : node_coloring (2 + qp) (2 + qs) (S ar) yellow
  | ON {qp qs ar} : node_coloring (1 + qp) (1 + qs) (S ar) orange
  | RN {qp qs ar} : node_coloring (0 + qp) (0 + qs) (S ar) red.

(* A type for general nodes, in the following, they will contain stored
   triples. *)
Inductive node' (A : Type) : arity -> kind -> color -> Type :=
  | Only {qp qs ar C} :
      node_coloring qp qs (S ar) C ->
      prefix' A (5 + qp) ->
      suffix' A (5 + qs) ->
      node' A (S ar) only C
  | Only_end {q} :
      prefix' A (S q) ->
      node' A 0 only green
  | Left {qp qs ar C} :
      node_coloring qp qs ar C ->
      prefix' A (5 + qp) ->
      A * A ->
      node' A ar left C
  | Right {qp qs ar C} :
      node_coloring qp qs ar C ->
      A * A ->
      suffix' A (5 + qs) ->
      node' A ar right C.
Arguments Only_end {A q}.
Arguments Only {A qp qs ar C}.
Arguments Left {A qp qs ar C}.
Arguments Right {A qp qs ar C}.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> color -> Type :=
  | G {lC rC} : regularity green lC    rC
  | R         : regularity red   green green.

(* The computation of elimination schemes is disabled. Elimination schemes are
   not necessary in the code and their definition takes ar long time to compute
   for the following mutually recursive types. *)
Unset Elimination Schemes.

(* A type for stored triples. *)
Inductive stored (A : Type) : level -> Type :=
  | Ground :
      A -> stored A 0
  | Big {l qp qs ar lC rC} :
      prefix' (stored A l) (3 + qp) ->
      chain A (S l) ar only lC rC ->
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
  | Packet {hl tl ar hk tk g r} :
      body A hl tl hk tk ->
      node' (stored A tl) ar tk (Mix g NoYellow NoOrange r) ->
      packet A hl (S tl) ar hk (Mix g NoYellow NoOrange r)

(* A type for chains. *)
with chain (A : Type) : level -> arity -> kind -> color -> color -> Type :=
  | Empty {l k lC rC} : chain A l empty k lC rC
  | Single {hl tl ar k C lC rC} :
    regularity C lC rC ->
    packet A hl tl ar k C ->
    chain A tl ar only lC rC ->
    chain A hl single k C C
  | Pair {l k lC rC} :
    chain A l single left lC lC ->
    chain A l single right rC rC ->
    chain A l pair k lC rC.

Arguments Ground {A}.
Arguments Big {A l qp qs ar lC rC}.
Arguments Small {A l q}.

Arguments Hole {A l k}.
Arguments Single_child {A hl tl hk tk y o}.
Arguments Pair_yellow {A hl tl hk tk C}.
Arguments Pair_orange {A hl tl hk tk}.

Arguments Packet {A hl tl ar hk tk g r}.

Arguments Empty {A l k lC rC}.
Arguments Single {A hl tl ar k C lC rC}.
Arguments Pair {A l k lC rC}.

(* Types for prefixes, suffixes, and nodes containing stored triples. *)

Definition prefix (A : Type) (l : level) := prefix' (stored A l).
Definition suffix (A : Type) (l : level) := suffix' (stored A l).
Definition node   (A : Type) (l : level) := node' (stored A l).

(* A type for green buffers, buffers of at least eight elements. *)
Inductive green_buffer (A : Type) (l : level) : Type :=
  | Gbuf {q} : deque (stored A l) (8 + q) -> green_buffer A l.
Arguments Gbuf {A l q}.

(* A type for stored buffers, buffers of at least three elements. *)
Inductive stored_buffer (A : Type) (l : level) : Type :=
  | Sbuf {q} : deque (stored A l) (3 + q) -> stored_buffer A l.
Arguments Sbuf {A l q}.

(* A type for the coloring relation of triples. *)
Inductive triple_coloring : color -> arity -> color -> color -> color -> Type :=
  | GT {ar lC rC} : triple_coloring green  ar      lC    rC    green
  | YT {ar lC rC} : triple_coloring yellow (S ar)  lC    rC    lC
  | OST {C}       : triple_coloring orange single  C     C     C
  | OPT {rC}      : triple_coloring orange pair    green rC    rC
  | RT {ar}       : triple_coloring red    (S ar)  green green red.

(* A type for triples. *)
Inductive triple : Type -> nat -> kind -> color -> Type :=
  | Triple {A l ar k nC lC rC C} :
      triple_coloring nC ar lC rC C ->
      node A l ar k nC ->
      chain A (S l) ar only lC rC ->
      triple A l k C.

(* A type for left or right triples. *)
Inductive left_right_triple : Type -> level -> kind -> color -> Type :=
  | Not_enough {A l k C} :
      vector (stored A l) 6 ->
      left_right_triple A l k C
  | Ok_lrt {A l k C} :
      triple A l k C -> left_right_triple A l k C.

(* A type for a tuple of six stored triples. *)
Definition six_stored (A : Type) (l : level) : Type :=
  stored A l * stored A l * stored A l *
  stored A l * stored A l * stored A l.

(* A type for partial triples. *)
Inductive partial_triple : Type -> level -> arity -> kind -> Type :=
  | Zero_element {A l k} :
      partial_triple A l single k
  | Six_elements {A l k} :
      six_stored A l -> partial_triple A l pair k
  | Ok_pt {A l ar k C} :
      triple A l k C -> partial_triple A l ar k.

(* A general sandwich type. *)
Inductive sandwich : Type -> Type -> Type :=
  | Alone {A B} : A -> sandwich A B
  | Sandwich {A B} : A -> B -> A -> sandwich A B.

(* A type for semi-regular cadeques. *)
Inductive semi_cadeque : Type -> nat -> Type :=
  | Semi {A l ar lC rC} :
      chain A l ar only lC rC -> semi_cadeque A l.

(* A type for cadeques. *)
Inductive cadeque : Type -> Type :=
  | T {A ar} : chain A 0 ar only green green -> cadeque A.

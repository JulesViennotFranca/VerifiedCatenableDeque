From Cadeque.color Require Import GYOR.
From Cadeque.cadeque Require Import buffer.

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
Inductive node_coloring : nat -> nat -> arity -> color -> Type :=
  | GN {qp qs ar : nat} : node_coloring (3 + qp) (3 + qs) (S ar) green
  | YN {qp qs ar : nat} : node_coloring (2 + qp) (2 + qs) (S ar) yellow
  | ON {qp qs ar : nat} : node_coloring (1 + qp) (1 + qs) (S ar) orange
  | RN {qp qs ar : nat} : node_coloring (0 + qp) (0 + qs) (S ar) red
  | EN {qp qs : nat}    : node_coloring (0 + qp) (0 + qs) 0      green.

(* A type for general nodes, in the following, they will contain stored
   triples. *)
Inductive node' (A : Type) : nat -> kind -> color -> Type :=
  | Only_end {q  : nat} : prefix' A (S q) -> node' A 0 only green
  | Only {qp qs a : nat} {C : color} :
    node_coloring qp qs (S a) C -> prefix' A (5 + qp) -> suffix' A (5 + qs) ->
    node' A (S a) only C
  | Left {qp qs a : nat} {C : color} :
    node_coloring qp qs a C -> prefix' A (5 + qp) -> A * A ->
    node' A a left C
  | Right {qp qs a : nat} {C : color} :
    node_coloring qp qs a C -> A * A -> suffix' A (5 + qs) ->
    node' A a right C.
Arguments Only_end {A q}.
Arguments Only {A qp qs a C}.
Arguments Left {A qp qs a C}.
Arguments Right {A qp qs a C}.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> color -> Type :=
  | G {Cl Cr : color} : regularity green Cl    Cr
  | R                 : regularity red   green green.

(* The computation of elimination schemes is disabled. Elimination schemes are
   not necessary in the code and their definition takes ar long time to compute
   for the following mutually recursive types. *)
Unset Elimination Schemes.

(* A type for stored triples. *)
Inductive stored (A : Type) : nat -> Type :=
  | Ground : A -> stored A 0
  | Small {lvl q : nat} :
    suffix' (stored A lvl) (3 + q) ->
    stored A (S lvl)
  | Big {lvl qp qs : nat} {ar : arity} {Cl Cr : color} :
    prefix' (stored A lvl) (3 + qp) ->
    chain A (S lvl) ar only Cl Cr ->
    suffix' (stored A lvl) (3 + qs) ->
    stored A (S lvl)

(* A type for bodies. *)
with body (A : Type) : nat -> nat -> kind -> kind -> Type :=
  | Hole {lvl : nat} {k : kind} : body A lvl lvl k k
  | Single_child {hlvl tlvl : nat} {hk tk : kind} {y o}:
    node' (stored A hlvl) 1 hk (Mix NoGreen y o NoRed) ->
    body A (S hlvl) tlvl only tk ->
    body A hlvl tlvl hk tk
  | Pair_yellow {hlvl tlvl : nat} {hk tk : kind} {C : color} :
    node' (stored A hlvl) 2 hk yellow ->
    body A (S hlvl) tlvl left tk ->
    chain A (S hlvl) single right C C ->
    body A hlvl tlvl hk tk
  | Pair_orange {hlvl tlvl : nat} {hk tk : kind} :
    node' (stored A hlvl) 2 hk orange ->
    chain A (S hlvl) single left green green ->
    body A (S hlvl) tlvl right tk ->
    body A hlvl tlvl hk tk

(* A type for packets. *)
with packet (A : Type) : nat -> nat -> nat -> kind -> color -> Type :=
  | Packet {hlvl tlvl ar : nat} {hk tk : kind} {g r} :
    body A hlvl tlvl hk tk ->
    node' (stored A tlvl) ar tk (Mix g NoYellow NoOrange r) ->
    packet A hlvl (S tlvl) ar hk (Mix g NoYellow NoOrange r)

(* A type for chains. *)
with chain (A : Type) : nat -> arity -> kind -> color -> color -> Type :=
  | Empty {lvl : nat} {k : kind} {Cl Cr : color} : chain A lvl empty k Cl Cr
  | Single {hlvl tlvl : nat} {a : arity} {k : kind} {C Cl Cr : color} :
    regularity C Cl Cr ->
    packet A hlvl tlvl a k C ->
    chain A tlvl a only Cl Cr ->
    chain A hlvl single k C C
  | Pair {lvl : nat} {k : kind} {Cl Cr : color} :
    chain A lvl single left Cl Cl ->
    chain A lvl single right Cr Cr ->
    chain A lvl pair k Cl Cr.

Arguments Ground {A}.
Arguments Small {A lvl q}.
Arguments Big {A lvl qp qs a Cl Cr}.

Arguments Hole {A l k}.
Arguments Single_child {A hl tl hk tk y o}.
Arguments Pair_yellow {A hl tl hk tk C}.
Arguments Pair_orange {A hl tl hk tk}.

Arguments Packet {A hlvl tlvl ar hk tk g r}.

Arguments Empty {A lvl k Cl Cr}.
Arguments Single {A hlvl tlvl a k C Cl Cr}.
Arguments Pair {A lvl k Cl Cr}.

(* Types for prefixes, suffixes, and nodes containing stored triples. *)

Definition prefix (A : Type) (lvl : nat) := prefix' (stored A lvl).
Definition suffix (A : Type) (lvl : nat) := suffix' (stored A lvl).

Definition node (A : Type) (lvl : nat) := node' (stored A lvl).

(* A type for green buffers, buffers of at least eight elements. *)
Inductive green_buffer (A : Type) (l : nat) : Type :=
  | Gbuf {q : nat} :
    buffer.t (stored A lvl) (8 + q) -> green_buffer A lvl.
Arguments Gbuf {A lvl q}.

(* A type for stored buffers, buffers of at least three elements. *)
Inductive stored_buffer (A : Type) (l : nat) : Type :=
  | Sbuf {q : nat} :
    buffer.t (stored A lvl) (3 + q) -> stored_buffer A lvl.
Arguments Sbuf {A lvl q}.

(* A type for the coloring relation of triples. *)
Inductive triple_coloring : color -> arity -> color -> color -> color -> Type :=
  | GT {a Cl Cr} : triple_coloring green  a      Cl    Cr    green
  | YT {a Cl Cr} : triple_coloring yellow (S a)  Cl    Cr    Cl
  | OST {C}      : triple_coloring orange single C     C     C
  | OPT {Cr}     : triple_coloring orange pair   green Cr    Cr
  | RT {a}       : triple_coloring red    (S a)  green green red.

(* A type for triples. *)
Inductive triple : Type -> nat -> kind -> color -> Type :=
  | Triple {A : Type} {lvl : nat} {a : arity}
           {k : kind} {Cnode Cl Cr C : color} :
    triple_coloring Cnode a Cl Cr C ->
    node A lvl a k Cnode ->
    chain A (S lvl) a only Cl Cr ->
    triple A lvl k C.

(* A type for left or right triples. *)
Inductive left_right_triple : Type -> nat -> kind -> color -> Type :=
  | Not_enough {A : Type} {lvl : nat} {k : kind} {C : color} :
    vector (stored A lvl) 6 ->
    left_right_triple A lvl k C
  | Ok_lrt {A : Type} {lvl : nat} {k : kind} {C : color} :
    triple A lvl k C -> left_right_triple A lvl k C.

(* A type for a tuple of six stored triples. *)
Definition six_stored (A : Type) (lvl : nat) : Type :=
  stored A lvl * stored A lvl * stored A lvl *
  stored A lvl * stored A lvl * stored A lvl.

(* A type for partial triples. *)
Inductive partial_triple : Type -> nat -> arity -> kind -> Type :=
  | Zero_element {A : Type} {lvl : nat} {k : kind} :
    partial_triple A lvl single k
  | Six_elements {A : Type} {lvl : nat} {k : kind} :
    six_stored A lvl -> partial_triple A lvl pair k
  | Ok_pt {A : Type} {lvl : nat} {ar : arity} {k : kind} {C : color} :
    triple A lvl k C -> partial_triple A lvl ar k.

(* A general sandwich type. *)
Inductive sandwich : Type -> Type -> Type :=
  | Alone {A B : Type} : A -> sandwich A B
  | Sandwich {A B : Type} : A -> B -> A -> sandwich A B.

(* A type for semi-regular cadeques. *)
Inductive semi_cadeque : Type -> nat -> Type :=
  | Semi {A : Type} {lvl : nat} {a : arity} {Cl Cr : color} :
    chain A lvl a only Cl Cr -> semi_cadeque A lvl.

(* A type for cadeques. *)
Inductive cadeque : Type -> Type :=
  | T {A : Type} {a : arity} : chain A 0 a only green green -> cadeque A.

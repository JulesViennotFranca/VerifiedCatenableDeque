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
Inductive coloring : nat -> nat -> nat -> color -> Type :=
  | Gc {qp qs nc : nat} : coloring (3 + qp) (3 + qs) (S nc) green
  | Yc {qp qs nc : nat} : coloring (2 + qp) (2 + qs) (S nc) yellow
  | Oc {qp qs nc : nat} : coloring (1 + qp) (1 + qs) (S nc) orange
  | Rc {qp qs nc : nat} : coloring (0 + qp) (0 + qs) (S nc) red
  | Ec {qp qs : nat}    : coloring (0 + qp) (0 + qs)    0   green.

(* A type for general nodes, in the following, they will contain stored
   triples. *)
Inductive node' (A : Type) : nat -> kind -> color -> Type :=
  | Only_end {q  : nat} :
      prefix' A (S q) ->
      node' A 0 only green
  | Only {qp qs nc : nat} {C : color} :
      coloring qp qs (S nc) C ->
      prefix' A (5 + qp) ->
      suffix' A (5 + qs) ->
      node' A (S nc) only C
  | Left {qp qs nc : nat} {C : color} :
      coloring qp qs nc C ->
      prefix' A (5 + qp) ->
      suffix' A 2 ->
      node' A nc left C
  | Right {qp qs nc : nat} {C : color} :
      coloring qp qs nc C ->
      prefix' A 2 ->
      suffix' A (5 + qs) ->
      node' A nc right C.

Arguments Only_end {A q}.
Arguments Only {A qp qs nc C}.
Arguments Left {A qp qs nc C}.
Arguments Right {A qp qs nc C}.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> arity -> color -> color -> Type :=
 | G {a Cl Cr} : regularity green  green a      Cl    Cr
 | Y {a Cl Cr} : regularity yellow Cl    (S a)  Cl    Cr
 | OS {C}      : regularity orange C     single C     C
 | OP {Cr}     : regularity orange Cr    pair   green Cr
 | R {a}       : regularity red    red   (S a)  green green.

(* The computation of elimination schemes is disabled. Elimination schemes are
   not necessary in the code and their definition takes a long time to compute
   for the following mutually recursive types. *)
Unset Elimination Schemes.

(* A type for stored triples. *)
Inductive stored_triple (A : Type) : nat -> Type :=
  | Ground : A -> stored_triple A 0
  | Small {l q : nat} :
    suffix' (stored_triple A l) (3 + q) ->
    stored_triple A (S l)
  | Big {l qp qs : nat} {ck : arity} {Cl Cr : color} :
    prefix' (stored_triple A l) (3 + qp) ->
    chain A (S l) ck only Cl Cr ->
    suffix' (stored_triple A l) (3 + qs) ->
    stored_triple A (S l)

(* A type for bodies. *)
with body (A : Type) : nat -> nat -> kind -> kind -> Type :=
  | Hole {l : nat} {k : kind} : body A l l k k
  | Single_child {hl tl : nat} {hk tk : kind} {y o}:
    node' (stored_triple A hl) 1 hk (Mix NoGreen y o NoRed) ->
    body A (S hl) tl only tk ->
    body A hl tl hk tk
  | Pair_yellow {hl tl : nat} {hk tk : kind} {C : color} :
    node' (stored_triple A hl) 2 hk yellow ->
    body A (S hl) tl left tk ->
    chain A (S hl) single right C C ->
    body A hl tl hk tk
  | Pair_orange {hl tl : nat} {hk tk : kind} :
    node' (stored_triple A hl) 2 hk orange ->
    chain A (S hl) single left green green ->
    body A (S hl) tl right tk ->
    body A hl tl hk tk

(* A type for packets. *)
with packet (A : Type) : nat -> nat -> nat -> kind -> color -> Type :=
  | Packet {hl tl nc : nat} {hk tk : kind} {g r} :
    body A hl tl hk tk ->
    node' (stored_triple A tl) nc tk (Mix g NoYellow NoOrange r) ->
    packet A hl (S tl) nc hk (Mix g NoYellow NoOrange r)

(* A type for chains. *)
with chain (A : Type) : nat -> arity -> kind -> color -> color -> Type :=
  | Empty {l : nat} : chain A l empty only green green
  | Single {hl tl : nat} {ck : arity} {k : kind} {C Cl Cr : color} :
    regularity C C ck Cl Cr ->
    packet A hl tl ck k C ->
    chain A tl ck only Cl Cr ->
    chain A hl single k C C
  | Pair {l : nat} {Cl Cr : color} :
    chain A l single left Cl Cl ->
    chain A l single right Cr Cr ->
    chain A l pair only Cl Cr.

Arguments Ground {A}.
Arguments Small {A l q}.
Arguments Big {A l qp qs ck Cl Cr}.

Arguments Hole {A l k}.
Arguments Single_child {A hl tl hk tk y o}.
Arguments Pair_yellow {A hl tl hk tk C}.
Arguments Pair_orange {A hl tl hk tk}.

Arguments Packet {A hl tl nc hk tk g r}.

Arguments Empty {A l}.
Arguments Single {A hl tl ck k C Cl Cr}.
Arguments Pair {A l Cl Cr}.

(* Types for prefixes, suffixes, and nodes containing stored triples. *)

Definition prefix (A : Type) (l : nat) := prefix' (stored_triple A l).
Definition suffix (A : Type) (l : nat) := suffix' (stored_triple A l).

Definition node (A : Type) (l : nat) := node' (stored_triple A l).

(* A type for green buffers, buffers of at least eight elements. *)
Inductive green_buffer (A : Type) (l : nat) : Type :=
  | Gbuf {q : nat} :
    buffer.t (stored_triple A l) (8 + q) -> green_buffer A l.
Arguments Gbuf {A l q}.

(* A type for stored buffers, buffers of at least three elements. *)
Inductive stored_buffer (A : Type) (l : nat) : Type :=
  | Sbuf {q : nat} :
    buffer.t (stored_triple A l) (3 + q) -> stored_buffer A l.
Arguments Sbuf {A l q}.

(* A type for triples. *)
Inductive triple : Type -> nat -> kind -> color -> Type :=
  | Triple {A : Type} {l : nat} {ck : arity}
           {k : kind} {C Cl Cr Cpkt : color} :
    regularity C Cpkt ck Cl Cr ->
    node A l ck k C ->
    chain A (S l) ck only Cl Cr ->
    triple A l k Cpkt.

(* A type for left or right triples. *)
Inductive left_right_triple : Type -> nat -> kind -> color -> Type :=
  | Not_enough {A : Type} {l : nat} {k : kind} :
    vector (stored_triple A l) 6 ->
    left_right_triple A l k green
  | Ok_lrt {A : Type} {l : nat} {k : kind} {Cpkt : color} :
    triple A l k Cpkt -> left_right_triple A l k Cpkt.

(* A type for a tuple of six stored triples. *)
Definition six_stored_triple (A : Type) (l : nat) : Type :=
  stored_triple A l * stored_triple A l * stored_triple A l *
  stored_triple A l * stored_triple A l * stored_triple A l.

(* A type for partial triples. *)
Inductive partial_triple : Type -> nat -> arity -> kind -> Type :=
  | Zero_element {A : Type} {l : nat} {k : kind} :
    partial_triple A l single k
  | Six_elements {A : Type} {l : nat} {k : kind} :
    six_stored_triple A l ->
    partial_triple A l pair k
  | Ok_pt {A : Type} {l : nat} {ck : arity} {k : kind} {C : color} :
    triple A l k C -> partial_triple A l ck k.

(* A general sandwich type. *)
Inductive sandwich : Type -> Type -> Type :=
  | Alone {A B : Type} : A -> sandwich A B
  | Sandwich {A B : Type} : A -> B -> A -> sandwich A B.

(* A type for semi-regular cadeques. *)
Inductive semi_cadeque : Type -> nat -> Type :=
  | Semi {A : Type} {l : nat} {ck : arity} {Cl Cr : color} :
    chain A l ck only Cl Cr -> semi_cadeque A l.

(* A type for cadeques. *)
Inductive cadeque : Type -> Type :=
  | T {A : Type} {ck : arity} : chain A 0 ck only green green -> cadeque A.

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
Inductive regularity :
  color -> color -> arity -> color -> color -> Type :=
 | G {ck Cl Cr} : regularity green  green ck     Cl    Cr
 | Y {ck Cl Cr} : regularity yellow Cl    (S ck) Cl    Cr
 | OS {C}       : regularity orange C     single C     C
 | OP {Cr}      : regularity orange Cr    pair   green Cr
 | R {ck}       : regularity red    red   (S ck) green green.

(* The computation of elimination schemes is disabled. Elimination schemes are
   not necessary in the code and their definition takes a long time to compute
   for the following mutually recursive types. *)
Unset Elimination Schemes.

(* A type for stored triples. *)
Inductive stored_triple (A : Type) : nat -> Type :=
  | Ground : A -> stored_triple A 0
  | Small {lvl q : nat} :
    suffix' (stored_triple A lvl) (3 + q) ->
    stored_triple A (S lvl)
  | Big {lvl qp qs : nat} {ck : arity} {Cl Cr : color} :
    prefix' (stored_triple A lvl) (3 + qp) ->
    chain A (S lvl) ck only Cl Cr ->
    suffix' (stored_triple A lvl) (3 + qs) ->
    stored_triple A (S lvl)

(* A type for bodies. *)
with body (A : Type) : nat -> nat -> kind -> kind -> Type :=
  | Hole {lvl : nat} {k : kind} : body A lvl lvl k k
  | Single_child {hlvl tlvl : nat} {hk tk : kind} {y o}:
    node' (stored_triple A hlvl) 1 hk (Mix NoGreen y o NoRed) ->
    body A (S hlvl) tlvl only tk ->
    body A hlvl tlvl hk tk
  | Pair_yellow {hlvl tlvl : nat} {hk tk : kind} {C : color} :
    node' (stored_triple A hlvl) 2 hk yellow ->
    body A (S hlvl) tlvl left tk ->
    chain A (S hlvl) single right C C ->
    body A hlvl tlvl hk tk
  | Pair_orange {hlvl tlvl : nat} {hk tk : kind} :
    node' (stored_triple A hlvl) 2 hk orange ->
    chain A (S hlvl) single left green green ->
    body A (S hlvl) tlvl right tk ->
    body A hlvl tlvl hk tk

(* A type for packets. *)
with packet (A : Type) : nat -> nat -> nat -> kind -> color -> Type :=
  | Packet {hlvl tlvl nc : nat} {hk tk : kind} {g r} :
    body A hlvl tlvl hk tk ->
    node' (stored_triple A tlvl) nc tk (Mix g NoYellow NoOrange r) ->
    packet A hlvl (S tlvl) nc hk (Mix g NoYellow NoOrange r)

(* A type for chains. *)
with chain (A : Type) : nat -> arity -> kind -> color -> color -> Type :=
  | Empty {lvl : nat} : chain A lvl empty only green green
  | Single {hlvl tlvl : nat} {ck : arity} {k : kind} {C Cl Cr : color} :
    regularity C C ck Cl Cr ->
    packet A hlvl tlvl ck k C ->
    chain A tlvl ck only Cl Cr ->
    chain A hlvl single k C C
  | Pair {lvl : nat} {Cl Cr : color} :
    chain A lvl single left Cl Cl ->
    chain A lvl single right Cr Cr ->
    chain A lvl pair only Cl Cr.

Arguments Ground {A}.
Arguments Small {A lvl q}.
Arguments Big {A lvl qp qs ck Cl Cr}.

Arguments Hole {A lvl k}.
Arguments Single_child {A hlvl tlvl hk tk y o}.
Arguments Pair_yellow {A hlvl tlvl hk tk C}.
Arguments Pair_orange {A hlvl tlvl hk tk}.

Arguments Packet {A hlvl tlvl nc hk tk g r}.

Arguments Empty {A lvl}.
Arguments Single {A hlvl tlvl ck k C Cl Cr}.
Arguments Pair {A lvl Cl Cr}.

(* Types for prefixes, suffixes, and nodes containing stored triples. *)

Definition prefix (A : Type) (lvl : nat) := prefix' (stored_triple A lvl).
Definition suffix (A : Type) (lvl : nat) := suffix' (stored_triple A lvl).

Definition node (A : Type) (lvl : nat) := node' (stored_triple A lvl).

(* A type for green buffers, buffers of at least eight elements. *)
Inductive green_buffer (A : Type) (lvl : nat) : Type :=
  | Gbuf {q : nat} :
    buffer.t (stored_triple A lvl) (8 + q) -> green_buffer A lvl.
Arguments Gbuf {A lvl q}.

(* A type for stored buffers, buffers of at least three elements. *)
Inductive stored_buffer (A : Type) (lvl : nat) : Type :=
  | Sbuf {q : nat} :
    buffer.t (stored_triple A lvl) (3 + q) -> stored_buffer A lvl.
Arguments Sbuf {A lvl q}.

(* A type for triples. *)
Inductive triple : Type -> nat -> kind -> color -> Type :=
  | Triple {A : Type} {lvl : nat} {ck : arity}
           {k : kind} {C Cl Cr Cpkt : color} :
    regularity C Cpkt ck Cl Cr ->
    node A lvl ck k C ->
    chain A (S lvl) ck only Cl Cr ->
    triple A lvl k Cpkt.

(* A type for left or right triples. *)
Inductive left_right_triple : Type -> nat -> kind -> color -> Type :=
  | Not_enough {A : Type} {lvl : nat} {k : kind} :
    vector (stored_triple A lvl) 6 ->
    left_right_triple A lvl k green
  | Ok_lrt {A : Type} {lvl : nat} {k : kind} {Cpkt : color} :
    triple A lvl k Cpkt -> left_right_triple A lvl k Cpkt.

(* A type for a tuple of six stored triples. *)
Definition six_stored_triple (A : Type) (lvl : nat) : Type :=
  stored_triple A lvl * stored_triple A lvl * stored_triple A lvl *
  stored_triple A lvl * stored_triple A lvl * stored_triple A lvl.

(* A type for partial triples. *)
Inductive partial_triple : Type -> nat -> arity -> kind -> Type :=
  | Zero_element {A : Type} {lvl : nat} {k : kind} :
    partial_triple A lvl single k
  | Six_elements {A : Type} {lvl : nat} {k : kind} :
    six_stored_triple A lvl ->
    partial_triple A lvl pair k
  | Ok_pt {A : Type} {lvl : nat} {ck : arity} {k : kind} {C : color} :
    triple A lvl k C -> partial_triple A lvl ck k.

(* A general sandwich type. *)
Inductive sandwich : Type -> Type -> Type :=
  | Alone {A B : Type} : A -> sandwich A B
  | Sandwich {A B : Type} : A -> B -> A -> sandwich A B.

(* A type for semi-regular cadeques. *)
Inductive semi_cadeque : Type -> nat -> Type :=
  | Semi {A : Type} {lvl : nat} {ck : arity} {Cl Cr : color} :
    chain A lvl ck only Cl Cr -> semi_cadeque A lvl.

(* A type for cadeques. *)
Inductive cadeque : Type -> Type :=
  | T {A : Type} {ck : arity} : chain A 0 ck only green green -> cadeque A.

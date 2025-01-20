From Coq Require Import List.
Import ListNotations.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.

From Cadeque.cadeque Require Import buffer types.

(* Sequence + map + concat for non-leveled nodes. *)
Definition node'_cmseq
  {T : Type -> nat -> Type}
  (f : forall A lvl, T A lvl -> list A)
  {A lvlt ar k C} (n : node' (T A lvlt) ar k C) (l : list A) : list A :=
  match n with
  | Only_end p  => buffer.concat_map_seq f p
  | Only  _ p s => buffer.concat_map_seq f p ++ l ++ buffer.concat_map_seq f s
  | Left  _ p (y, z) => buffer.concat_map_seq f p ++ l ++ f _ _ y ++ f _ _ z
  | Right _ (a, b) s => f _ _ a ++ f _ _ b ++ l ++ buffer.concat_map_seq f s
  end.

Set Equations Transparent.

(* Returns the sequence associated to a stored triple. *)
Equations stored_seq A lvl
  (st : stored A lvl) : list A by struct st :=
stored_seq A lvl (Ground a) := [a];
stored_seq A lvl (Small s) := buffer.concat_map_seq stored_seq s;
stored_seq A lvl (Big p child s) :=
  buffer.concat_map_seq stored_seq p ++
  chain_seq child ++
  buffer.concat_map_seq stored_seq s

(* Returns the sequence associated to a body. *)
with body_seq {A hlvl tlvl hk tk} (b : body A hlvl tlvl hk tk) : list A -> list A
by struct b :=
body_seq Hole l := l;
body_seq (Single_child hd b) l :=
  concat_map_node'_seq stored_seq hd (body_seq b l);
body_seq (Pair_yellow hd b cr) l :=
  concat_map_node'_seq stored_seq hd (body_seq b l ++ chain_seq cr);
body_seq (Pair_orange hd cl b) l :=
  concat_map_node'_seq stored_seq hd (chain_seq cl ++ body_seq b l)

(* Returns the sequence associated to a packet. *)
with packet_seq {A hlvl tlvl ar k C} :
  packet A hlvl tlvl ar k C -> list A -> list A :=
packet_seq (Packet b tl) l := body_seq b (concat_map_node'_seq stored_seq tl l)

(* Returns the sequence associated to a chain. *)
with chain_seq {A lvl ar k Cl Cr}
  (c : chain A lvl ar k Cl Cr) : list A by struct c :=
chain_seq Empty := [];
chain_seq (Single _ pkt rest) := packet_seq pkt (chain_seq rest);
chain_seq (Pair cl cr) := chain_seq cl ++ chain_seq cr.

Arguments stored_seq {A lvl}.
Arguments stored_seq {A lvl}.

(* Returns the sequence associated to a buffer containing stored triples. *)
Notation buffer_seq b := (buffer.concat_map_seq (@stored_seq) b).

(* Returns the sequence associated to a prefix containing stored triples. *)
Notation prefix_seq p := (buffer_seq p).

(* Returns the sequence associated to a suffix containing stored triples. *)
Notation suffix_seq s := (buffer_seq s).

(* Returns the sequence associated to a node containing stored triples. *)
Notation node_seq n l := (concat_map_node'_seq (@stored_seq) n l).

(* Returns the sequence associated to a green buffer. *)
Equations green_buffer_seq {A lvl} : green_buffer A lvl -> list A :=
green_buffer_seq (Gbuf b) := buffer_seq b.

(* Returns the sequence associated to a stored buffer. *)
Equations stored_buffer_seq {A lvl} : stored_buffer A lvl -> list A :=
stored_buffer_seq (Sbuf b) := buffer_seq b.

(* Returns the sequence associated to a triple. *)
Equations triple_seq {A lvl k C} : triple A lvl k C -> list A :=
triple_seq (Triple _ hd child) := node_seq hd (chain_seq child).

(* Returns the sequence associated to a left or right triple. *)
Equations lr_triple_seq {A lvl k C} : left_right_triple A lvl k C -> list A :=
lr_triple_seq (Not_enough v) := concat (map stored_seq (vector_seq v));
lr_triple_seq (Not_enough v) := concat (map stored_seq (vector_seq v));
lr_triple_seq (Ok_lrt t) := triple_seq t.

(* Returns the sequence associated to six stored triples. *)
Equations six_stored_seq {A lvl} : six_stored A lvl -> list A :=
six_stored_seq (a1, a2, a3, a4, a5, a6) :=
  stored_seq a1 ++ stored_seq a2 ++ stored_seq a3 ++
  stored_seq a4 ++ stored_seq a5 ++ stored_seq a6.
Equations six_stored_seq {A lvl} : six_stored A lvl -> list A :=
six_stored_seq (a1, a2, a3, a4, a5, a6) :=
  stored_seq a1 ++ stored_seq a2 ++ stored_seq a3 ++
  stored_seq a4 ++ stored_seq a5 ++ stored_seq a6.

(* Returns the sequence associated to a partial triple. *)
Equations pt_triple_seq {A lvl ar k} : partial_triple A lvl ar k -> list A :=
pt_triple_seq Zero_element := [];
pt_triple_seq (Six_elements six) := six_stored_seq six;
pt_triple_seq (Six_elements six) := six_stored_seq six;
pt_triple_seq (Ok_pt t) := triple_seq t.

(* Provided model functions for the types A and B, returns the sequence
   associated to a sandwich. *)
Equations sandwich_seq {A B C : Type} :
  (A -> list C) -> (B -> list C) -> sandwich A B -> list C :=
sandwich_seq A_seq _ (Alone a) := A_seq a;
sandwich_seq A_seq B_seq (Sandwich a m z) := A_seq a ++ B_seq m ++ A_seq z.

(* Returns the sequence associated to a semi_cadeque. *)
Equations semi_cadeque_seq {A lvl} : semi_cadeque A lvl -> list A :=
semi_cadeque_seq (Semi c) := chain_seq c.

(* Returns the sequence associated to a cadeque. *)
Equations cadeque_seq {A : Type} : cadeque A -> list A :=
cadeque_seq (T c) := chain_seq c.

Unset Equations Transparent.

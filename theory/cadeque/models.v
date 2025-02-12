From Coq Require Import List.
Import ListNotations.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.

From Cadeque.cadeque Require Import vector deque types.

(* Sequence + map + concat for non-leveled nodes. *)
Definition node'_cmseq
  {T : Type -> nat -> Type}
  (f : forall A l, T A l -> list A)
  {A lt ar k C} (n : node' (T A lt) ar k C) (l : list A) : list A :=
  match n with
  | Only_end p  => deque_cmseq f p
  | Only  _ p s => deque_cmseq f p ++ l ++ deque_cmseq f s
  | Left  _ p (y, z) => deque_cmseq f p ++ l ++ f _ _ y ++ f _ _ z
  | Right _ (a, b) s => f _ _ a ++ f _ _ b ++ l ++ deque_cmseq f s
  end.

Set Equations Transparent.

(* Returns the sequence associated to a stored triple. *)
Equations stored_seq A l (st : stored A l) : list A
by struct st :=
stored_seq A l (Ground a) :=
  [a];
stored_seq A l (Small s) :=
  deque_cmseq stored_seq s;
stored_seq A l (Big p child s) :=
  deque_cmseq stored_seq p ++
  chain_seq child ++
  deque_cmseq stored_seq s

(* Returns the sequence associated to a body. *)
with body_seq {A hl tl hk tk} (b : body A hl tl hk tk) : list A -> list A
by struct b :=
body_seq Hole accu :=
  accu;
body_seq (Single_child hd b) accu :=
  node'_cmseq stored_seq hd (body_seq b accu);
body_seq (Pair_yellow hd b cr) accu :=
  node'_cmseq stored_seq hd (body_seq b accu ++ chain_seq cr);
body_seq (Pair_orange hd cl b) accu :=
  node'_cmseq stored_seq hd (chain_seq cl ++ body_seq b accu)

(* Returns the sequence associated to a packet. *)
with packet_seq {A hl tl ar k C} : packet A hl tl ar k C -> list A -> list A :=
packet_seq (Packet b tl) accu :=
  body_seq b (node'_cmseq stored_seq tl accu)

(* Returns the sequence associated to a chain. *)
with chain_seq {A l ar k lC rC} (c : chain A l ar k lC rC) : list A
by struct c :=
chain_seq Empty := [];
chain_seq (Single _ pkt rest) := packet_seq pkt (chain_seq rest);
chain_seq (Pair lc rc) := chain_seq lc ++ chain_seq rc.

Arguments stored_seq {A l}.

(* Returns the sequence associated to a prefix containing stored triples. *)
Notation prefix_seq p := (deque_cmseq (@stored_seq) p).

(* Returns the sequence associated to a suffix containing stored triples. *)
Notation suffix_seq s := (deque_cmseq (@stored_seq) s).

(* Returns the sequence associated to a node containing stored triples. *)
Notation node_seq n l := (node'_cmseq (@stored_seq) n l).

(* Returns the sequence associated to a green buffer. *)
Equations green_buffer_seq {A l} : green_buffer A l -> list A :=
green_buffer_seq (Gbuf b) := prefix_seq b.

(* Returns the sequence associated to a stored buffer. *)
Equations stored_buffer_seq {A l} : stored_buffer A l -> list A :=
stored_buffer_seq (Sbuf b) := suffix_seq b.

(* Returns the sequence associated to a triple. *)
Equations triple_seq {A l k C} : triple A l k C -> list A :=
triple_seq (Triple _ hd child) := node_seq hd (chain_seq child).

(* Returns the sequence associated to a left or right triple. *)
Equations lr_triple_seq {A l k C} : left_right_triple A l k C -> list A :=
lr_triple_seq (Not_enough v) := concat (map stored_seq (vector_seq v));
lr_triple_seq (Ok_lrt t) := triple_seq t.

(* Returns the sequence associated to six stored triples. *)
Equations six_stored_seq {A l} : six_stored A l -> list A :=
six_stored_seq (a1, a2, a3, a4, a5, a6) :=
  stored_seq a1 ++ stored_seq a2 ++ stored_seq a3 ++
  stored_seq a4 ++ stored_seq a5 ++ stored_seq a6.

(* Returns the sequence associated to a partial triple. *)
Equations pt_triple_seq {A l ar k} : partial_triple A l ar k -> list A :=
pt_triple_seq Zero_element := [];
pt_triple_seq (Six_elements six) := six_stored_seq six;
pt_triple_seq (Ok_pt t) := triple_seq t.

(* Provided model functions for the types A and B, returns the sequence
   associated to a sandwich. *)
Equations sandwich_seq {A B C : Type} :
  (A -> list C) -> (B -> list C) -> sandwich A B -> list C :=
sandwich_seq A_seq _ (Alone a) := A_seq a;
sandwich_seq A_seq B_seq (Sandwich a m z) := A_seq a ++ B_seq m ++ A_seq z.

(* Returns the sequence associated to a semi_cadeque. *)
Equations semi_cadeque_seq {A l} : semi_cadeque A l -> list A :=
semi_cadeque_seq (Semi c) := chain_seq c.

(* Returns the sequence associated to a cadeque. *)
Equations cadeque_seq {A} : cadeque A -> list A :=
cadeque_seq (T c) := chain_seq c.

Unset Equations Transparent.

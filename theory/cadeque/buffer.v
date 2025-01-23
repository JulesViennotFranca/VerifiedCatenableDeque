From Coq Require Import Program List Lia.
Import ListNotations.
From Equations Require Import Equations.
From Hammer Require Import Tactics.

From Theory.cadeque Require Import deque.

(* +------------------------------------------------------------------------+ *)
(* |                                Vectors                                 | *)
(* +------------------------------------------------------------------------+ *)

(* A type for vectors. *)
Inductive vector (A : Type) : nat -> Type :=
| V0 {n : nat} : vector A n
| V1 {n : nat} : A -> vector A (1 + n)
| V2 {n : nat} : A -> A -> vector A (2 + n)
| V3 {n : nat} : A -> A -> A -> vector A (3 + n)
| V4 {n : nat} : A -> A -> A -> A -> vector A (4 + n)
| V5 {n : nat} : A -> A -> A -> A -> A -> vector A (5 + n)
| V6 {n : nat} : A -> A -> A -> A -> A -> A -> vector A (6 + n).
Arguments V0 {A n}.
Arguments V1 {A n}.
Arguments V2 {A n}.
Arguments V3 {A n}.
Arguments V4 {A n}.
Arguments V5 {A n}.
Arguments V6 {A n}.

Set Equations Transparent.

(* Returns the sequence associated to a vector. *)
Equations vector_seq {A n} : vector A n -> list A :=
vector_seq V0 := [];
vector_seq (V1 a) := [a];
vector_seq (V2 a b) := [a] ++ [b];
vector_seq (V3 a b c) := [a] ++ [b] ++ [c];
vector_seq (V4 a b c d) := [a] ++ [b] ++ [c] ++ [d];
vector_seq (V5 a b c d e) := [a] ++ [b] ++ [c] ++ [d] ++ [e];
vector_seq (V6 a b c d e f) := [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f].

(* Returns the number of elements contained in a vector. *)
Equations vector_size {A n} (v : vector A n) : nat :=
vector_size V0 := 0;
vector_size (V1 _) := 1;
vector_size (V2 _ _) := 2;
vector_size (V3 _ _ _) := 3;
vector_size (V4 _ _ _ _) := 4;
vector_size (V5 _ _ _ _ _) := 5;
vector_size (V6 _ _ _ _ _ _) := 6.

Unset Equations Transparent.

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A type for buffers. *)
Inductive t (A : Type) (q : nat) : Type :=
  | Buffer : deque A q -> t A q.
Arguments Buffer {A q}.

(* This type represent a buffer which size is the maximum of [increment] and
   [quantity].

   In practice, this type is used in cases where [quantity] is greater than
   [increment]. The definition of [pt] ensures that the size of the buffer
   represented is equal to [quantity], while precising that the size is at
   least [increment]. *)
Definition pt (A : Type) (increment : nat) (quantity : nat) :=
    t A (increment + Nat.iter increment Nat.pred quantity).

(* +------------------------------------------------------------------------+ *)
(* |                                 Models                                 | *)
(* +------------------------------------------------------------------------+ *)

(* The [app] function and the singleton list are made opaque. *)
Opaque app.
Definition singleton {A : Type} (a1 : A) : list A := [a1].
Opaque singleton.

(* Sequence + map + concat for buffers. *)
Definition concat_map_seq
  {T : Type -> nat -> Type}
  (f : forall A lvl, T A lvl -> list A)
  {A lvlt q} (b : t (T A lvlt) q) : list A :=
  match b with
  | Buffer d => concat_map_deque_seq f d
  end.

(* Returns the sequence associated to a buffer. *)
Definition seq {A q} (b : t A q) :=
  concat_map_seq (T := fun A _ => A) (fun _ _ a => [a]) (lvlt := 0) b.

(* Ensures the correct behavior of buffers model functions. *)
Lemma correct_concat_map_seq
  [T : Type -> nat -> Type]
  (f : forall A lvl, T A lvl -> list A)
  [A lvlt q] (b : t (T A lvlt) q) :
  concat_map_seq f b = concat (map (f A lvlt) (seq b)).
Proof.
  destruct b. simpl.
  apply correct_concat_map_deque_seq.
Qed.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A hint database of rewrites to be used when trying to automatically resolve
   obligations on buffers generated by [Equations]. *)
#[export] Hint Rewrite <-app_assoc : rbuffer.
#[export] Hint Rewrite app_nil_r : rbuffer.

(* Setting the default tactics for obligations to be [hauto] using the [rlist]
   hint database. *)
#[local] Obligation Tactic := try (cbn; hauto db:rbuffer).

(* The empty deque. *)
Equations empty {A : Type} : { b : t A 0 | seq b = [] } :=
empty with deque.empty => { | ? d := ? Buffer d }.

(* Pushes on a buffer. *)
Equations push {A q} (a1 : A) (b : t A q) :
  { b' : t A (S q) | seq b' = [a1] ++ seq b } :=
push a1 (Buffer d) with deque.push a1 d => { | ? d' := ? Buffer d' }.

(* Injects on a buffer. *)
Equations inject {A q} (b : t A q) (a1 : A) :
  { b' : t A (S q) | seq b' = seq b ++ [a1] } :=
inject (Buffer d) a1 with deque.inject d a1 => { | ? d' := ? Buffer d' }.

(* Pops off a buffer containing at least one element. *)
Equations pop {A q} (b : t A (S q)) :
  { '(a1, b') : A * t A q | seq b = [a1] ++ seq b' } :=
pop (Buffer d) with deque.pop d => { | ? (a1, d') := ? (a1, Buffer d') }.

(* Ejects off a buffer containing at least one element. *)
Equations eject {A q} (b : t A (S q)) :
  { '(b', a1) : t A q * A | seq b = seq b' ++ [a1] } :=
eject (Buffer d) with deque.eject d => { | ? (d', a1) := ? (Buffer d', a1) }.

(* Pushes two elements on a buffer. *)
Equations push2 {A q} (a1 a2 : A) (b : t A q) :
  { b' : t A (S (S q)) | seq b' = [a1] ++ [a2] ++ seq b } :=
push2 a1 a2 b with push a2 b => {
  | ? b' with push a1 b' => {
    | ? b'' := ? b'' } }.

(* Injects two elemets on a buffer. *)
Equations inject2 {A q} (b : t A q) (a2 a1 : A) :
  { b' : t A (S (S q)) | seq b' = seq b ++ [a2] ++ [a1] } :=
inject2 b a2 a1 with inject b a2 => {
  | ? b' with inject b' a1 => {
    | ? b'' := ? b'' } }.

(* Pops two elements off a buffer containing at least two elements. *)
Equations pop2 {A q} (b : t A (S (S q))) :
  { '(a1, a2, b') : A * A * t A q | seq b = [a1] ++ [a2] ++ seq b' } :=
pop2 b with pop b => {
  | ? (a1, b') with pop b' => {
    | ? (a2, b'') := ? (a1, a2, b'') } }.

(* Ejects two elements off a buffer containing at least two elements. *)
Equations eject2 {A q} (b : t A (S (S q))) :
  { '(b', a2, a1) : t A q * A * A | seq b = seq b' ++ [a2] ++ [a1] } :=
eject2 b with eject b => {
  | ? (b', a1) with eject b' => {
    | ? (b'', a2) := ? (b'', a2, a1) } }.

(* Ensures that the sequence associated to a buffer containing no element is
   empty. *)
Lemma empty_buffer [A] (b : t A 0) : seq b = [].
Proof.
  dependent elimination b. simpl.
  dependent elimination d. simpl.
  dependent elimination c; simpl.
  - dependent elimination b. reflexivity.
  - inversion p; subst.
    + dependent elimination r.
    + pose (yellow_size X) as Hpsize.
      destruct Hpsize as [ps Hpsize].
      exfalso; lia.
Qed.

(* The last lemma is added to the hint database. *)
#[export] Hint Rewrite empty_buffer : rbuffer.

(* Returns the two elements of a buffer containing two elements. *)
Equations two {A} (b : t A 2) : { '(a1, a2) : A * A | seq b = [a1] ++ [a2] } :=
two b with pop b => {
  | ? (a1, b') with pop b' => {
    | ? (a2, _) := ? (a1, a2) } }.

(* Returns a buffer containing one element. *)
Equations single {A} (a1 : A) : { b : t A 1 | seq b = [a1] } :=
single a1 with deque.empty => {
  | ? d with deque.push a1 d => {
    | ? d' := ? Buffer d' } }.

(* Returns a buffer containing two elements. *)
Equations pair {A} (a1 a2 : A) : { b : t A 2 | seq b = [a1] ++ [a2] } :=
pair a1 a2 with single a2 => {
  | ? b with push a1 b => { | ? b' := ? b' } }.

(* Pushes three elements on a buffer. *)
Equations push3 {A q} (a1 a2 a3 : A) (b : t A q) :
    { b' : t A (3 + q) | seq b' = [a1] ++ [a2] ++ [a3] ++ seq b } :=
push3 a1 a2 a3 b with push2 a2 a3 b => {
  | ? b' with push a1 b' => { | ? b'' := ? b'' } }.

(* Injects three elements on a buffer. *)
Equations inject3 {A q} (b : t A q) (a3 a2 a1 : A) :
    { b' : t A (3 + q) | seq b' = seq b ++ [a3] ++ [a2] ++ [a1] } :=
inject3 b a3 a2 a1 with inject2 b a3 a2 => {
  | ? b' with inject b' a1 => { | ? b'' := ? b'' } }.

(* Pushes five elements on a buffer. *)
Equations push5 {A q} (a1 a2 a3 a4 a5 : A) (b : t A q) :
  { b' : t A (5 + q) |
    seq b' = [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ seq b } :=
push5 a1 a2 a3 a4 a5 b with push3 a3 a4 a5 b => {
  | ? b' with push2 a1 a2 b' => { | ? b'' := ? b'' } }.

(* Injects five elements on a buffer. *)
Equations inject5 {A q} (b : t A q) (a5 a4 a3 a2 a1 : A) :
  { b' : t A (5 + q) |
    seq b' = seq b ++ [a5] ++ [a4] ++ [a3] ++ [a2] ++ [a1] } :=
inject5 b a5 a4 a3 a2 a1 with inject3 b a5 a4 a3 => {
  | ? b' with inject2 b' a2 a1 => { | ? b'' := ? b'' } }.

(* Pops five elements off a buffer containing at least five elements. *)
Equations pop5 {A q} (b : t A (5 + q)) :
  { '(a1, a2, a3, a4, a5, b') : A * A * A * A * A * t A q |
    seq b = [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ seq b' } :=
pop5 b with pop2 b => {
  | ? (a1, a2, b') with pop2 b' => {
    | ? (a3, a4, b'') with pop b'' => {
      | ? (a5, b''') := ? (a1, a2, a3, a4, a5, b''') } } }.

(* Pushes six elements on a buffer. *)
Equations push6 {A q} (a1 a2 a3 a4 a5 a6 : A) (b : t A q) :
  { b' : t A (6 + q) |
    seq b' = [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ [a6] ++ seq b } :=
push6 a1 a2 a3 a4 a5 a6 b with push5 a2 a3 a4 a5 a6 b => {
  | ? b' with push a1 b' => { | ? b'' := ? b'' } }.

(* Injects six elements on a buffer. *)
Equations inject6 {A q} (b : t A q) (a6 a5 a4 a3 a2 a1 : A) :
  { b' : t A (6 + q) |
    seq b' = seq b ++ [a6] ++ [a5] ++ [a4] ++ [a3] ++ [a2] ++ [a1] } :=
inject6 b a6 a5 a4 a3 a2 a1 with inject5 b a6 a5 a4 a3 a2 => {
  | ? b' with inject b' a1 => { | ? b'' := ? b'' } }.

(* Injects eight elements on a buffer. *)
Equations inject8 {A q} (b : t A q) (a8 a7 a6 a5 a4 a3 a2 a1 : A) :
  { b' : t A (8 + q) |  seq b' =
    seq b ++ [a8] ++ [a7] ++ [a6] ++ [a5] ++ [a4] ++ [a3] ++ [a2] ++ [a1] } :=
inject8 b a8 a7 a6 a5 a4 a3 a2 a1 with inject6 b a8 a7 a6 a5 a4 a3 => {
  | ? b' with inject2 b' a2 a1 => { | ? b'' := ? b'' } }.

(* Pops eight elements off a buffer containing at least eight elements. *)
Equations pop8 {A q} (b : t A (8 + q)) :
  { '(a1, a2, a3, a4, a5, a6, a7, a8, b') :
    A * A * A * A * A * A * A * A * t A q | seq b =
    [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ [a6] ++ [a7] ++ [a8] ++ seq b' } :=
pop8 b with pop b => { | ? (a1, b') with pop5 b' => {
  | ? (a2, a3, a4, a5, a6, b'') with pop2 b'' => {
    | ? (a7, a8, b''') := ? (a1, a2, a3, a4, a5, a6, a7, a8, b''') } } }.

(* Provided the equality of two natural numbers, translates a buffer of size
   the first into a buffer of size the second. *)
Equations translate {A q1 q2} (b : t A q1) :
  q1 = q2 -> { b' : t A q2 | seq b' = seq b } :=
translate b eq_refl := ? b.

(* Pushes a vector on a buffer. *)
Equations push_vector {A n q} (v : vector A n) (b : t A q) :
    { b' : t A (q + vector_size v) | seq b' = vector_seq v ++ seq b } :=
push_vector V0 b with translate b _ => { | ? b' := ? b' };
push_vector (V1 a1) b with push a1 b => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } };
push_vector (V2 a1 a2) b with push2 a1 a2 b => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } };
push_vector (V3 a1 a2 a3) b with push2 a2 a3 b => {
  | ? b' with push a1 b' => { | ? b'' with translate b'' _ => {
    | ? b''' := ? b''' } } };
push_vector (V4 a1 a2 a3 a4) b with push2 a3 a4 b => {
  | ? b' with push2 a1 a2 b' => { | ? b'' with translate b'' _ => {
    | ? b''' := ? b''' } } };
push_vector (V5 a1 a2 a3 a4 a5) b with push5 a1 a2 a3 a4 a5 b => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } };
push_vector (V6 a1 a2 a3 a4 a5 a6) b with push6 a1 a2 a3 a4 a5 a6 b => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } }.

(* Injects a vector on a buffer. *)
Equations inject_vector {A n q} (b : t A q) (v : vector A n) :
    { b' : t A (q + vector_size v) | seq b' = seq b ++ vector_seq v } :=
inject_vector b V0 with translate b _ => { | ? b' := ? b' };
inject_vector b (V1 a1) with inject b a1 => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } };
inject_vector b (V2 a1 a2) with inject2 b a1 a2 => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } };
inject_vector b (V3 a1 a2 a3) with inject2 b a1 a2 => {
  | ? b' with inject b' a3 => { | ? b'' with translate b'' _ => {
    | ? b''' := ? b''' } } };
inject_vector b (V4 a1 a2 a3 a4) with inject2 b a1 a2 => {
  | ? b' with inject2 b' a3 a4 => { | ? b'' with translate b'' _ => {
    | ? b''' := ? b''' } } };
inject_vector b (V5 a1 a2 a3 a4 a5) with inject5 b a1 a2 a3 a4 a5 => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } };
inject_vector b (V6 a1 a2 a3 a4 a5 a6) with inject6 b a1 a2 a3 a4 a5 a6 => {
  | ? b' with translate b' _ => { | ? b'' := ? b'' } }.

(* Pushes a vector then five elements on a buffer. *)
Equations push_5vector {A n q}
  (a1 a2 a3 a4 a5 : A) (vec : vector A n) (b : t A q) :
  { b' : t A (5 + q + vector_size vec) | seq b' =
    [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ vector_seq vec ++ seq b } :=
push_5vector a1 a2 a3 a4 a5 vec b with push_vector vec b => {
  | ? b' with push5 a1 a2 a3 a4 a5 b' => { | ? b'' := ? b'' } }.

(* Injects five elements the a vector on a buffer. *)
Equations inject_5vector {A n q}
  (b : t A q) (a5 a4 a3 a2 a1 : A) (vec : vector A n) :
  { b' : t A (5 + q + vector_size vec) | seq b' =
    seq b ++ [a5] ++ [a4] ++ [a3] ++ [a2] ++ [a1] ++ vector_seq vec } :=
inject_5vector b a5 a4 a3 a2 a1 vec with inject5 b a5 a4 a3 a2 a1 => {
  | ? b' with inject_vector b' vec => { | ? b'' := ? b'' } }.

(* Takes a buffer and returns an option indicating wheter the buffer is empty
   or not. *)
Equations has1 {A q} (b : t A q) :
  { o : option (pt A 1 q) |
    seq b = match o with None => [] | Some b' => seq b' end } :=
has1 (q := 0) _ := ? None;
has1 (q := _) b := ? Some b.

(* Takes a buffer and returns a sum indicating whether the buffer contains less
   or strictly more than two elements. *)
Equations has3 {A q} (b : t A q) :
  { s : sum (vector A 2) (pt A 3 q) |
    seq b = match s with
            | inl vec => vector_seq vec
            | inr b' => seq b'
            end } :=
has3 (q := 0) b := ? inl V0;
has3 (q := 1) b with pop b => { | ? (a1, _) := ? inl (V1 a1) };
has3 (q := 2) b with pop2 b => { | ? (a1, a2, _) := ? inl (V2 a1 a2) };
has3 (q := _) b := ? inr b.

(* Takes a buffer of at least four elements and returns a sum indicating
   whether the buffer contains exactly four or more than five elements. *)
Equations has5 {A q} (b : t A (4 + q)) :
  { s : sum (A * A * A * A) (pt A 5 (4 + q)) |
    seq b = match s with
            | inl (a1, a2, a3, a4) => [a1] ++ [a2] ++ [a3] ++ [a4]
            | inr b' => seq b'
            end } :=
has5 (q := 0) b with pop2 b => {
  | ? (a1, a2, b') with two b' => {
    | ? (a3, a4) := ? inl (a1, a2, a3, a4) } };
has5 (q := _) b := ? inr b.

(* Takes a buffer of at least one element and returns a sum indicating whether
   the buffer contains less or strictly more than six elements. *)
Equations has7 {A q} (b : t A (S q)) :
  { s : sum (vector A 6) (pt A 7 (S q)) |
    seq b = match s with
            | inl vec => vector_seq vec
            | inr b' => seq b'
            end } :=
has7 (q := 0) b with pop b => { | ? (a1, _) := ? inl (V1 a1) };
has7 (q := 1) b with pop2 b => { | ? (a1, a2, _) := ? inl (V2 a1 a2) };
has7 (q := 2) b with pop2 b => {
  | ? (a1, a2, b') with pop b' => { | ? (a3, _) := ? inl (V3 a1 a2 a3) } };
has7 (q := 3) b with pop2 b => {
  | ? (a1, a2, b') with pop2 b' => {
    | ? (a3, a4, _) := ? inl (V4 a1 a2 a3 a4) } };
has7 (q := 4) b with pop5 b => {
  | ? (a1, a2, a3, a4, a5, _) := ? inl (V5 a1 a2 a3 a4 a5) };
has7 (q := 5) b with pop5 b => {
  | ? (a1, a2, a3, a4, a5, b') with pop b' => {
    | ? (a6, _) := ? inl (V6 a1 a2 a3 a4 a5 a6) } };
has7 (q := _) b := ? inr b.

(* Takes a buffer of at least five elements and returns a sum indicating
   whether the buffer contains less or strictly more than seven elements. *)
Equations has8 {A q} (b : t A (5 + q)) :
  { s : sum (A * A * A * A * A * vector A 2) (pt A 8 (5 + q)) |
    seq b = match s with
            | inl (a1, a2, a3, a4, a5, vec) =>
              [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ vector_seq vec
            | inr b' => seq b'
            end } :=
has8 (q := 0) b with pop5 b => {
  | ? (a1, a2, a3, a4, a5, _) := ? inl (a1, a2, a3, a4, a5, V0) };
has8 (q := 1) b with pop5 b => {
  | ? (a1, a2, a3, a4, a5, b') with pop b' => {
    | ? (a6, _) := ? inl (a1, a2, a3, a4, a5, V1 a6) } };
has8 (q := 2) b with pop5 b => {
  | ? (a1, a2, a3, a4, a5, b') with two b' => {
    | ? (a6, a7) := ? inl (a1, a2, a3, a4, a5, V2 a6 a7) } };
has8 (q := _) b := ? inr b.

(* Takes a buffer of at least three elements and returns its first three
   elements along with a sum indicating whether the remaining buffer contains
   less or strictly more than three elements. *)
Equations has3p {A q} (b : t A (3 + q)) :
  { p : (A * A * A) * sum (vector A 2) (pt A 3 q) |
    let '((a1, a2, a3), s) := p in
    seq b = [a1] ++ [a2] ++ [a3] ++ match s with
                            | inl vec => vector_seq vec
                            | inr b' => seq b'
                            end } :=
has3p b with pop2 b => { | ? (a1, a2, b') with pop b' => {
  | ? (a3, b'') with has3 b'' => {
    | ? s := ? ((a1, a2, a3), s) } } }.

(* Takes a buffer of at least three elements and returns its last three
  elements along with a sum indicating whether the remaining buffer contains
  less or strictly more than three elements. *)
Equations has3s {A q} (b : t A (3 + q)) :
  { p : sum (vector A 2) (pt A 3 q) * (A * A * A) |
    let '(s, (a3, a2, a1)) := p in
    seq b = match s with
            | inl vec => vector_seq vec
            | inr b' => seq b'
            end ++ [a3] ++ [a2] ++ [a1] } :=
has3s b with eject2 b => { | ? (b', a2, a1) with eject b' => {
  | ? (b'', a3) with has3 b'' => {
    | ? s := ? (s, (a3, a2, a1)) } } }.

(* Takes a buffer of at least eight elements and returns a sum indicating
   whether the buffer contains less or strictly more than 10 elements. *)
Equations has3p8 {A q} (b : t A (8 + q)) :
  {s : sum (A * A * A * A * A * A * A * A * vector A 2)
           (t A 3 * pt A 8 (5 + q)) |
    seq b = match s with
            | inl (a1, a2, a3, a4, a5, a6, a7, a8, vec) =>
              [a1] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ [a6] ++ [a7] ++ [a8] ++
              vector_seq vec
            | inr (b3, b8) => seq b3 ++ seq b8
            end } :=
has3p8 (q := 0) b with pop8 b => {
  | ? (a1, a2, a3, a4, a5, a6, a7, a8, _) :=
    ? inl (a1, a2, a3, a4, a5, a6, a7, a8, V0) };
has3p8 (q := 1) b with pop8 b => {
  | ? (a1, a2, a3, a4, a5, a6, a7, a8, b') with pop b' => {
    | ? (a9, _) := ? inl (a1, a2, a3, a4, a5, a6, a7, a8, V1 a9) } };
has3p8 (q := 2) b with pop8 b => {
  | ? (a1, a2, a3, a4, a5, a6, a7, a8, b') with pop2 b' => {
    | ? (a9, a10, _) := ? inl (a1, a2, a3, a4, a5, a6, a7, a8, V2 a9 a10) } };
has3p8 (q := _) b with pop2 b => { | ? (a1, a2, b') with pop b' => {
    | ? (a3, b8) with single a3 => { | ? b1 with push2 a1 a2 b1 => {
      | ? b3 := ? inr (b3, b8) } } } }.
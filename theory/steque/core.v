From Coq Require Import Program List Lia.
Import ListNotations.
From Equations Require Import Equations.
From Hammer Require Import Tactics.
From AAC_tactics Require Import AAC.
From AAC_tactics Require Import Instances.
Import Instances.Lists.

From Theory.color Require Import GYR.
From Theory.steque Require Import deque.

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A type for unleveld suffix buffers. *)
Notation suffix' A := (deque A).

(* A type for unleveled prefix buffers that need to have at least two
   elements. *)
Inductive prefix' (A : Type) : color -> Type :=
  | P2 : A -> A -> prefix' A red
  | P3 : A -> A -> A -> prefix' A yellow
  | P4 : A -> A -> A -> A -> deque A -> prefix' A green.
Arguments P2 {A}.
Arguments P3 {A}.
Arguments P4 {A}.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> color -> Type :=
  | G {g r} : regularity green (Mix g NoYellow r) green
  | Y : regularity yellow green yellow
  | O : regularity yellow   red orange
  | R : regularity    red green    red.

(* A type for pairs. *)
Inductive pair (A : Type) : nat -> Type :=
  | Ground : A -> pair A 0
  | Pair {lvl : nat} {C1 C2 : color} :
      prefix' (pair A lvl) C1 ->
      chain A (S lvl) C2 ->
      pair A (S lvl)

(* A type for packets. *)
with packet (A : Type) : nat -> nat -> color -> Type :=
  | Hole {lvl : nat} : packet A lvl lvl uncolored
  | Packet {lvl hlvl : nat} {y : yellow_hue} {C : color} :
      prefix' (pair A lvl) C ->
      packet A (S lvl) hlvl (Mix NoGreen y NoRed) ->
      suffix' (pair A lvl) ->
      packet A lvl hlvl C

(* A type for chains. *)
with chain (A : Type) : nat -> color -> Type :=
  | Ending {lvl : nat} : suffix' (pair A lvl) -> chain A lvl green
  | Chain {lvl hlvl : nat} {C1 C2 C3 : color} :
      regularity C1 C2 C3 ->
      packet A lvl hlvl C1 ->
      chain A hlvl C2 ->
      chain A lvl C3.

Arguments Ground {A}.
Arguments Pair {A lvl C1 C2}.

Arguments Hole {A lvl}.
Arguments Packet {A lvl hlvl y C}.

Arguments Ending {A lvl}.
Arguments Chain {A lvl hlvl C1 C2 C3}.

(* Types for leveled prefix and suffix buffers. *)
Notation suffix A lvl := (suffix' (pair A lvl)).
Notation prefix A lvl := (prefix' (pair A lvl)).

(* A type for green or yellow prefixes. *)
Inductive gy_prefix A lvl :=
  | GY_pre {g y} : prefix A lvl (Mix g y NoRed) -> gy_prefix A lvl.
Arguments GY_pre {A lvl g y}.

(* A type for green or yellow chains. *)
Inductive gy_chain A lvl :=
  | GY_chn {g y} : chain A lvl (Mix g y NoRed) -> gy_chain A lvl.
Arguments GY_chn {A lvl g y}.

(* A type for the triple representation of a steque. The steque is either a
   deque or a triple made of a prefix, a child steque and a suffix. *)
Inductive triple (A : Type) (lvl : nat) : Type :=
  | Small : suffix A lvl -> triple A lvl
  | Triple {C1 C2 : color} :
      prefix A lvl C1 -> chain A (S lvl) C2 -> suffix A lvl -> triple A lvl.
Arguments Small {A lvl}.
Arguments Triple {A lvl C1 C2}.

(* A type for semi-regular steques. *)
Inductive semi_steque (A : Type) (lvl : nat) : Type :=
  | Semi {C : color} : chain A lvl C -> semi_steque A lvl.
Arguments Semi {A lvl C}.

(* A type for regular steques. *)
Inductive steque (A : Type) : Type :=
  | T {g y} : chain A 0 (Mix g y NoRed) -> steque A.
Arguments T {A g y}.

(* +------------------------------------------------------------------------+ *)
(* |                                 Models                                 | *)
(* +------------------------------------------------------------------------+ *)

(* Model functions are transparent. *)
Set Equations Transparent.

(* The [app] function and the singleton list are made opaque. *)
Opaque app.
Definition singleton {A : Type} (x : A) : list A := [x].
Opaque singleton.

(* Sequence + map + concat function for unleveled prefixes. *)
Definition concat_map_prefix'_seq
  {T : Type -> nat -> Type}
  (f : forall A lvl, T A lvl -> list A)
  {A lvlt C} (p : prefix' (T A lvlt) C) : list A :=
  match p with
  | P2 a1 a2 => f A lvlt a1 ++ f A lvlt a2
  | P3 a1 a2 a3 => f A lvlt a1 ++ f A lvlt a2 ++ f A lvlt a3
  | P4 a1 a2 a3 a4 d =>
      f A lvlt a1 ++ f A lvlt a2 ++ f A lvlt a3 ++ f A lvlt a4 ++
      concat_map_deque_seq f d
  end.

(* Returns the sequence associated to a pair. *)
Equations pair_seq A lvl (p : pair A lvl) : list A by struct p :=
pair_seq A lvl (Ground a) := [a];
pair_seq A lvl (Pair p c) := concat_map_prefix'_seq pair_seq p ++ chain_seq c

(* Returns the sequence associated to a packet. *)
with packet_seq {A lvl hlvl C}
  (pkt : packet A lvl hlvl C) : list A -> list A by struct pkt :=
packet_seq Hole l := l;
packet_seq (Packet p pkt s) l :=
  concat_map_prefix'_seq pair_seq p ++
  packet_seq pkt l ++
  concat_map_deque_seq pair_seq s

(* Returns the sequence associated to a chain. *)
with chain_seq {A lvl C} (c : chain A lvl C) : list A by struct c :=
chain_seq (Ending d) := concat_map_deque_seq pair_seq d;
chain_seq (Chain _ pkt c) := packet_seq pkt (chain_seq c).

Arguments pair_seq {A lvl}.

(* Returns the sequence associated to a leveled prefix. *)
Notation prefix_seq p := (concat_map_prefix'_seq (@pair_seq) p).

(* Returns the sequence associated to a leveled suffix. *)
Notation suffix_seq := (concat_map_deque_seq (@pair_seq)).

(* Returns the sequence associated to a green or yellow prefix. *)
Equations gy_prefix_seq {A lvl} : gy_prefix A lvl -> list A :=
gy_prefix_seq (GY_pre p) := prefix_seq p.

(* Returns the sequence associated to a green or yellow chain. *)
Equations gy_chain_seq {A lvl} : gy_chain A lvl -> list A :=
gy_chain_seq (GY_chn c) := chain_seq c.

(* Returns the sequence associated to a triple. *)
Equations triple_seq {A lvl} : triple A lvl -> list A :=
triple_seq (Small s) := suffix_seq s;
triple_seq (Triple p child s) :=
  prefix_seq p ++ chain_seq child ++ suffix_seq s.

(* Returns the sequence associated to a semi-regular steque. *)
Equations semi_steque_seq {A lvl} : semi_steque A lvl -> list A :=
semi_steque_seq (Semi c) := chain_seq c.

(* returns the sequence associated to a regular steque. *)
Equations steque_seq {A} : steque A -> list A :=
steque_seq (T c) := chain_seq c.

Unset Equations Transparent.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Notation for dependent types hiding the property on [x]. *)
Notation "? x" := (@exist _ _ x _) (at level 100).

(* A hint database of rewrites to be used when trying to automatically resolve
   obligations generated by [Equations]. *)
#[export] Hint Rewrite <-app_assoc : rlist.
#[export] Hint Rewrite app_nil_r : rlist.
#[export] Hint Rewrite app_nil_l : rlist.
#[export] Hint Rewrite map_app : rlist.
#[export] Hint Rewrite concat_app : rlist.

#[export] Hint Rewrite correct_concat_map_deque_seq : rlist.

(* Setting the default tactics for obligations to be [hauto] using the [rlist]
   hint database. *)
#[local] Obligation Tactic := try (cbn; hauto db:rlist).

(* The empty chain. *)
Equations empty_chain {A lvl} :
  { c : chain A lvl green | chain_seq c = [] } :=
empty_chain with deque.empty => { | ? d := ? Ending d }.

(* Takes a green prefix [p], a child chain [child] and a suffix [s] and
   returns the green chain (p, child, s). *)
Equations make_green {A lvl C}
  (p : prefix A lvl green)
  (c : chain A (S lvl) C)
  (s : suffix A lvl) :
  { c' : chain A lvl green |
      chain_seq c' = prefix_seq p ++ chain_seq c ++ suffix_seq s } :=
make_green p (Ending d) s := ? Chain G (Packet p Hole s) (Ending d);
make_green p (Chain G pkt c) s := ? Chain G (Packet p Hole s) (Chain G pkt c);
make_green p (Chain Y pkt c) s := ? Chain G (Packet p pkt s) c;
make_green p (Chain O pkt c) s := ? Chain G (Packet p pkt s) c;
make_green p (Chain R pkt c) s := ? Chain G (Packet p Hole s) (Chain R pkt c).

(* Takes a yellow prefix [p], a green or yellow child chain [child] and a
   suffix [s] and returns the yellow chain (p, child, s). *)
Equations make_yellow {A lvl g y}
  (p : prefix A lvl yellow)
  (c : chain A (S lvl) (Mix g y NoRed))
  (s : suffix A lvl) :
  { c' : chain A lvl yellow |
      chain_seq c' = prefix_seq p ++ chain_seq c ++ suffix_seq s } :=
make_yellow p (Ending d) s := ? Chain Y (Packet p Hole s) (Ending d);
make_yellow p (Chain G pkt c) s := ? Chain Y (Packet p Hole s) (Chain G pkt c);
make_yellow p (Chain Y pkt c) s := ? Chain Y (Packet p pkt s) c.

(* Takes a yellow prefix [p], a child chain [child] and a suffix [s] and
   returns the semi-regular steque (p, child, s).
   The hidden color of the output steque is either yellow or orange. *)
Equations make_yellorange {A lvl C}
  (p : prefix A lvl yellow)
  (c : chain A (S lvl) C)
  (s : suffix A lvl) :
  { sst : semi_steque A lvl |
      semi_steque_seq sst = prefix_seq p ++ chain_seq c ++ suffix_seq s } :=
make_yellorange p (Ending d) s := ? Semi (Chain Y (Packet p Hole s) (Ending d));
make_yellorange p (Chain G pkt c) s :=
  ? Semi (Chain Y (Packet p Hole s) (Chain G pkt c));
make_yellorange p (Chain Y pkt c) s := ? Semi (Chain Y (Packet p pkt s) c);
make_yellorange p (Chain O pkt c) s := ? Semi (Chain O (Packet p pkt s) c);
make_yellorange p (Chain R pkt c) s :=
  ? Semi (Chain O (Packet p Hole s) (Chain R pkt c)).

(* Takes a red prefix [p], a child steque [child] and a suffix [s] and returns
   the red chain (p, child, s). *)
Equations make_red {A lvl g y}
  (p : prefix A lvl red)
  (c : chain A (S lvl) (Mix g y NoRed))
  (s : suffix A lvl) :
  { c' : chain A lvl red |
      chain_seq c' = prefix_seq p ++ chain_seq c ++ suffix_seq s } :=
make_red p (Ending d) s := ? Chain R (Packet p Hole s) (Ending d);
make_red p (Chain G pkt c) s := ? Chain R (Packet p Hole s) (Chain G pkt c);
make_red p (Chain Y pkt c) s := ? Chain R (Packet p pkt s) c.

(* Takes a packet [pkt] and a green or red chain [c], and returns a
   semi-regular steque made of [pkt] followed by [c]. *)
Equations build_on_green_or_red {A lvl hlvl g1 y1 g2 r2}
  (pkt : packet A lvl hlvl (Mix g1 y1 NoRed))
  (c : chain A hlvl (Mix g2 NoYellow r2)) :
  { c' : semi_steque A lvl |
    semi_steque_seq c' = packet_seq pkt (chain_seq c) } :=
build_on_green_or_red Hole c := ? Semi c;
build_on_green_or_red (Packet (P3 a1 a2 a3) pkt s) (Ending d) :=
  ? Semi (Chain Y (Packet (P3 a1 a2 a3) pkt s) (Ending d));
build_on_green_or_red (Packet (P3 a1 a2 a3) pkt s) (Chain G pkt' c') :=
  ? Semi (Chain Y (Packet (P3 a1 a2 a3) pkt s) (Chain G pkt' c'));
build_on_green_or_red (Packet (P3 a1 a2 a3) pkt s) (Chain R pkt' c') :=
  ? Semi (Chain O (Packet (P3 a1 a2 a3) pkt s) (Chain R pkt' c'));
build_on_green_or_red (Packet (P4 a1 a2 a3 a4 d) pkt s) c :=
  ? Semi (Chain G (Packet (P4 a1 a2 a3 a4 d) pkt s) c).

(* Takes a packet [pkt] and a green chain [c], and returns a regular steque
   made of [pkt] followed by [c]. *)
Equations build_on_green {A lvl hlvl y}
  (pkt : packet A lvl hlvl (Mix NoGreen y NoRed))
  (c : chain A hlvl green) :
  { c' : gy_chain A lvl |
    gy_chain_seq c' = packet_seq pkt (chain_seq c) } :=
build_on_green Hole c := ?  GY_chn c;
build_on_green (Packet (P3 a1 a2 a3) pkt s) c :=
  ? GY_chn (Chain Y (Packet (P3 a1 a2 a3) pkt s) c).

(* Takes a yellow packet [pkt] and a red chain [c], and returns a semi-regular
   steque made of [pkt] followed by [c]. *)
Equations build_on_red {A lvl hlvl y}
  (pkt : packet A lvl hlvl (Mix NoGreen y NoRed))
  ( c : chain A hlvl red) :
  { c' : semi_steque A lvl |
    semi_steque_seq c' = packet_seq pkt (chain_seq c) } :=
build_on_red Hole c := ? Semi c;
build_on_red (Packet (P3 a1 a2 a3) pkt s) c :=
  ? Semi (Chain O (Packet (P3 a1 a2 a3) pkt s) c).

(* Pushes on a green prefix. *)
Equations green_prefix_push {A lvl}
  (x : pair A lvl)
  (p : prefix A lvl green) :
  { p' : prefix A lvl green | prefix_seq p' = pair_seq x ++ prefix_seq p } :=
green_prefix_push x (P4 a1 a2 a3 a4 d) with deque.push a4 d => {
    | ? d' := ? P4 x a1 a2 a3 d' }.

(* Pushes on a yellow prefix. *)
Equations yellow_prefix_push {A lvl}
  (x : pair A lvl)
  (p : prefix A lvl yellow) :
  { p' : prefix A lvl green | prefix_seq p' = pair_seq x ++ prefix_seq p } :=
yellow_prefix_push x (P3 a1 a2 a3) with deque.empty => {
    | ? d := ? P4 x a1 a2 a3 d }.

(* Pushes on a red prefix. *)
Equations red_prefix_push {A lvl}
  (x : pair A lvl)
  (p : prefix A lvl red) :
  { p' : prefix A lvl yellow | prefix_seq p' = pair_seq x ++ prefix_seq p } :=
red_prefix_push x (P2 a1 a2) := ? P3 x a1 a2.

(* Pushes tow elements on a prefix. *)
Equations prefix_push_two {A lvl C}
  (x y : pair A lvl)
  (p : prefix A lvl C) :
  { p' : prefix A lvl green |
      prefix_seq p' = pair_seq x ++ pair_seq y ++ prefix_seq p } :=
prefix_push_two x y (P2 a1 a2) with deque.empty => {
  | ? d := ? P4 x y a1 a2 d };
prefix_push_two x y (P3 a1 a2 a3) with deque.empty => {
  | ? d with deque.push a3 d => {
    | ? d1 := ? P4 x y a1 a2 d1 } };
prefix_push_two x y (P4 a1 a2 a3 a4 d) with deque.push a4 d => {
  | ? d1 with deque.push a3 d1 => {
    | ? d2 := ? P4 x y a1 a2 d2 } }.

(* Pushes on a chain and returns a steque. *)
Equations chain_push {A lvl C}
  (x : pair A lvl)
  (c : chain A lvl C) :
  { c' : gy_chain A lvl |
      gy_chain_seq c' = pair_seq x ++ chain_seq c } :=
chain_push x (Ending d) with deque.push x d => {
    | ? d' := ? GY_chn (Ending d') };
chain_push x (Chain G (Packet p pkt s) c) with green_prefix_push x p => {
    | ? p' := ? GY_chn (Chain G (Packet p' pkt s) c) };
chain_push x (Chain Y (Packet p pkt s) c) with yellow_prefix_push x p => {
    | ? p' := ? GY_chn (Chain G (Packet p' pkt s) c) };
chain_push x (Chain O (Packet p pkt s) c) with yellow_prefix_push x p => {
    | ? p' := ? GY_chn (Chain G (Packet p' pkt s) c) };
chain_push x (Chain R (Packet p pkt s) c) with red_prefix_push x p => {
    | ? p' := ? GY_chn (Chain Y (Packet p' pkt s) c) }.

(* Injects on a chain. *)
Equations chain_inject {A lvl C}
  (c : chain A lvl C)
  (x : pair A lvl) :
  { c' : chain A lvl C |
      chain_seq c' = chain_seq c ++ pair_seq x } :=
chain_inject (Ending d) x with deque.inject d x => {
    | ? d' := ? Ending d' };
chain_inject (Chain G (Packet p pkt s) c) x with deque.inject s x => {
  | ? s' := ? Chain G (Packet p pkt s') c };
chain_inject (Chain Y (Packet p pkt s) c) x with deque.inject s x => {
  | ? s' := ? Chain Y (Packet p pkt s') c };
chain_inject (Chain O (Packet p pkt s) c) x with deque.inject s x => {
  | ? s' := ? Chain O (Packet p pkt s') c };
chain_inject (Chain R (Packet p pkt s) c) x with deque.inject s x => {
  | ? s' := ? Chain R (Packet p pkt s') c }.

(* Pushes on a semi-regular steque. *)
Equations semi_push {A lvl} (x : pair A lvl) (sst : semi_steque A lvl) :
  { sst' : semi_steque A lvl |
    semi_steque_seq sst' = pair_seq x ++ semi_steque_seq sst } :=
semi_push x (Semi c) with chain_push x c => { | ? GY_chn c' := ? Semi c' }.

(* Takes a child chain [child], a suffix [s] and a semi-regular steque [sst]
   and merges the element in the suffix either in [sst] or in [child] depending
   on the number of elements contained in the suffix. *)
Equations remove_suffix {A lvl C}
  (child : chain A (S lvl) C)
  (s : suffix A lvl)
  (sst : semi_steque A lvl) :
  { '(child', sst') : chain A (S lvl) C * semi_steque A lvl |
      chain_seq child' ++ semi_steque_seq sst' =
          chain_seq child ++ suffix_seq s ++ semi_steque_seq sst } :=
remove_suffix child s sst with deque.pop s => {
  | ? None => ? (child, sst);
  | ? Some (a1, s1) with deque.pop s1 => {
    | ? None with semi_push a1 sst => {
      | ? sst' := ? (child, sst') };
    | ? Some (a2, s2) with deque.pop s2, empty_chain => {
      | ? None, ? c with chain_inject child (Pair (P2 a1 a2) c) => {
        | ? child' := ? (child', sst) };
      | ? Some (a3, s3), ? c with deque.pop s3 => {
        | ? None with chain_inject child (Pair (P3 a1 a2 a3) c) => {
          | ? child' := ? (child', sst) };
        | ? Some (a4, s4)
          with chain_inject child (Pair (P4 a1 a2 a3 a4 s4) c) => {
            | ? child' := ? (child', sst) } } } } }.

(* Returns the triple corresponding to a chain. *)
Equations triple_of_semi {A lvl} (sst : semi_steque A lvl) :
  { t : triple A lvl | triple_seq t = semi_steque_seq sst } :=
triple_of_semi (Semi (Ending d)) := ? Small d;
triple_of_semi (Semi (Chain G (Packet p pkt s) c))
  with build_on_green_or_red pkt c => { | ? Semi child := ? Triple p child s };
triple_of_semi (Semi (Chain Y (Packet p pkt s) c))
  with build_on_green pkt c => { | ? GY_chn child := ? Triple p child s };
triple_of_semi (Semi (Chain O (Packet p pkt s) c))
  with build_on_red pkt c => { | ? Semi child := ? Triple p child s };
triple_of_semi (Semi (Chain R (Packet p pkt s) c))
  with build_on_green pkt c => { | ? GY_chn child := ? Triple p child s }.

(* Joins a child chain [child], a suffix [s] and a semi-regular steque [sst].
   Computes case 1 of concatenating two steques:
   s1 = (_, child, s) and s2 = sst. *)
Equations join {A lvl C}
  (child : chain A (S lvl) C)
  (s : suffix A lvl)
  (sst : semi_steque A lvl) :
  { '(child', s') : chain A (S lvl) C * suffix A lvl |
      chain_seq child' ++ suffix_seq s' =
          chain_seq child ++ suffix_seq s ++ semi_steque_seq sst } :=
join child s sst with remove_suffix child s sst => {
  | ? (child1, sst2) with triple_of_semi sst2 => {
    | ? Small s2 := ? (child1, s2);
    | ? Triple p2 child2 s2 with chain_inject child1 (Pair p2 child2) => {
      | ? child3 := ? (child3, s2) } } }.

(* Concatenates two semi-regular steques. *)
Equations semi_concat {A lvl} (sst1 sst2 : semi_steque A lvl) :
  { sst3 : semi_steque A lvl |
    semi_steque_seq sst3 = semi_steque_seq sst1 ++ semi_steque_seq sst2 } :=
semi_concat (Semi (Ending d)) sst2 with deque.pop d => {
  | ? None := ? sst2;
  | ? Some (a1, d1) with deque.pop d1 => {
    | ? None with semi_push a1 sst2 => { | ? sst3 := ? sst3 };
    | ? Some (a2, d2) with deque.pop d2 => {
      | ? None with semi_push a2 sst2 => {
        | ? sst2' with semi_push a1 sst2' => { | ? sst3 := ? sst3 } };
      | ? Some (a3, d3) with deque.pop d3 => {
        | ? None with semi_push a3 sst2 => {
          | ? sst2' with semi_push a2 sst2' => {
            | ? sst2'' with semi_push a1 sst2'' => { | ? sst3 := ? sst3 } } };
        | ? Some (a4, d4) with triple_of_semi sst2, empty_chain => {
          | ? Small s3, ? c :=
            ? Semi (Chain G (Packet (P4 a1 a2 a3 a4 d4) Hole s3) c);
          | ? Triple p child s3, ? c with chain_push (Pair p c) child => {
            | ? GY_chn child3 with make_green (P4 a1 a2 a3 a4 d4) child3 s3 => {
              | ? c3 := ? Semi c3 } } } } } } };
semi_concat (Semi (Chain G (Packet p3 pkt s) c)) sst2
  with build_on_green_or_red pkt c => {
    | ? Semi child1 with join child1 s sst2 => {
        | ? (child3, s3) with make_green p3 child3 s3 => {
          | ? c3 := ? Semi c3 } } };
semi_concat (Semi (Chain Y (Packet p3 pkt s) c)) sst2
  with build_on_green pkt c => {
    | ? GY_chn child1 with join child1 s sst2 => {
        | ? (child3, s3) with make_yellow p3 child3 s3 => {
          | ? c3 := ? Semi c3 } } };
semi_concat (Semi (Chain O (Packet p3 pkt s) c)) sst2
  with build_on_red pkt c => {
    | ? Semi child1 with join child1 s sst2 => {
        | ? (child3, s3) with make_yellorange p3 child3 s3 => {
          | ? sst3 := ? sst3 } } };
semi_concat (Semi (Chain R (Packet p3 pkt s) c)) sst2
  with build_on_green pkt c => {
    | ? GY_chn child1 with join child1 s sst2 => {
        | ? (child3, s3) with make_red p3 child3 s3 => {
          | ? c3 := ? Semi c3 } } }.

(* Pops off a green prefix. *)
Equations green_prefix_pop {A lvl} (p : prefix A lvl green) :
    { '(x, p') : pair A lvl * gy_prefix A lvl |
        prefix_seq p = pair_seq x ++ gy_prefix_seq p' } :=
green_prefix_pop (P4 a1 a2 a3 a4 d) with deque.pop d => {
  | ? None := ? (a1, GY_pre (P3 a2 a3 a4));
  | ? Some (a5, d') := ? (a1, GY_pre (P4 a2 a3 a4 a5 d')) }.

(* Makes a red chain green. *)
Equations green_of_red {A lvl} (c : chain A lvl red) :
    { c' : chain A lvl green | chain_seq c' = chain_seq c } :=
green_of_red (Chain R (Packet (P2 a1 a2) Hole s) (Ending d))
  with deque.pop d => {
  | ? None with deque.push a2 s => { | ? s1 with deque.push a1 s1 => {
    | ? s2 := ? Ending s2 } };
  | ? Some (Pair p stored, d')
    with prefix_push_two a1 a2 p,
         semi_concat (Semi stored) (Semi (Ending d')) => {
      | ? p', ? Semi child with make_green p' child s => { | ? c' := ? c' } } };
green_of_red (Chain R (Packet (P2 a1 a2) Hole s)
                      (Chain G (Packet p2 pkt s2) c))
  with green_prefix_pop p2 => {
    | ? (Pair p stored, GY_pre p2') with prefix_push_two a1 a2 p => {
      | ? p' with build_on_green_or_red (Packet p2' pkt s2) c => {
        | ? child with semi_concat (Semi stored) child => {
          | ? Semi child' with make_green p' child' s => {
            | ? c' := ? c' } } } } };
green_of_red (Chain R (Packet (P2 a1 a2)
                      (Packet (P3 (Pair p stored) a4 a5) pkt s2) s) c)
  with prefix_push_two a1 a2 p => { | ? p'
    with semi_concat (Semi stored)
                     (Semi (Chain R (Packet (P2 a4 a5) pkt s2) c)) => {
      | ? Semi child' with make_green p' child' s => {
        | ? c' := ? c' } } }.

(* Makes a green or red chain green. *)
Equations ensure_green {A lvl g r} (c : chain A lvl (Mix g NoYellow r)) :
    { c' : chain A lvl green | chain_seq c' = chain_seq c } :=
ensure_green (Ending s) := ? Ending s;
ensure_green (Chain G pkt c) := ? Chain G pkt c  ;
ensure_green (Chain R pkt c) with green_of_red (Chain R pkt c) => {
  | ? c' := ? c' }.

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(* The empty steque. *)
Equations empty {A} : { s : steque A | steque_seq s = [] } :=
empty with empty_chain => { | ? c := ? T c }.

(* Pushes on a steque. *)
Equations push {A} (x : A) (s : steque A) :
  { s' : steque A | steque_seq s' = [x] ++ steque_seq s } :=
push x (T c) with chain_push (Ground x) c => { | ? GY_chn c' := ? T c' }.

(* Injects on a steque. *)
Equations inject {A} (s : steque A) (x : A) :
  { s' : steque A | steque_seq s' = steque_seq s ++ [x] } :=
inject (T c) x with chain_inject c (Ground x) => { | ? c' := ? T c' }.

(* Pops off a steque. *)
Equations pop {A} (s : steque A) :
    { o : option (A * steque A) |
        match o with
        | None => steque_seq s = []
        | Some (x, s') => steque_seq s = [x] ++ steque_seq s' end } :=
pop (T (Ending d)) with deque.pop d => {
  | ? None := ? None;
  | ? Some (Ground x, d') := ? Some (x, T (Ending d')) };
pop (T (Chain G (Packet p pkt s) c)) with green_prefix_pop p => {
  | ? (Ground x, GY_pre (P4 a1 a2 a3 a4 d)) :=
    ? Some (x, T (Chain G (Packet (P4 a1 a2 a3 a4 d) pkt s) c));
  | ? (Ground x, GY_pre (P3 a1 a2 a3)) with ensure_green c => {
    | ? c' := ? Some (x, T (Chain Y (Packet (P3 a1 a2 a3) pkt s) c')) } };
pop (T (Chain Y (Packet (P3 (Ground x) a1 a2) pkt s) c))
  with green_of_red (Chain R (Packet (P2 a1 a2) pkt s) c) => {
    | ? c' := ? Some (x, T c') }.

(* Concatenates two steques. *)
Equations concat {A} (s1 s2 : steque A) :
  { s3 : steque A | steque_seq s3 = steque_seq s1 ++ steque_seq s2 } :=
concat (T (Ending d)) (T c2) with deque.pop d => {
  | ? None := ? T c2;
  | ? Some (Ground a1, d1) with deque.pop d1 => {
    | ? None with push a1 (T c2) => { | ? sst3 := ? sst3 };
    | ? Some (Ground a2, d2) with deque.pop d2 => {
      | ? None with push a2 (T c2) => {
        | ? T c2' with push a1 (T c2') => { | ? sst3 := ? sst3 } };
      | ? Some (Ground a3, d3) with deque.pop d3 => {
        | ? None with push a3 (T c2) => {
          | ? T c2' with push a2 (T c2') => {
            | ? T c2'' with push a1 (T c2'') => { | ? sst3 := ? sst3 } } };
        | ? Some (a4, d4) with triple_of_semi (Semi c2), empty_chain => {
          | ? Small s3, ? c :=
            let p := P4 (Ground a1) (Ground a2) (Ground a3) a4 d4 in
            ? T (Chain G (Packet p Hole s3) c);
          | ? Triple p child s3, ? c with chain_push (Pair p c) child => {
            | ? GY_chn child3
              with make_green (P4 (Ground a1) (Ground a2) (Ground a3) a4 d4)
                              child3
                              s3 => {
              | ? c3 := ? T c3 } } } } } } };
concat (T (Chain G (Packet p3 pkt s) c)) (T c2)
  with build_on_green_or_red pkt c => {
    | ? Semi child1 with join child1 s (Semi c2) => {
        | ? (child3, s3) with make_green p3 child3 s3 => {
          | ? c3 := ? T c3 } } };
concat (T (Chain Y (Packet p3 pkt s) c)) (T c2)
  with build_on_green pkt c => {
    | ? GY_chn child1 with join child1 s (Semi c2) => {
        | ? (child3, s3) with make_yellow p3 child3 s3 => {
          | ? c3 := ? T c3 } } }.

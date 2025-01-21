From Coq Require Import List.
Import ListNotations.
From Hammer Require Import Tactics.

From Cadeque.cadeque Require Import buffer types models core.

(* Hint databases of rewrites to be used when trying to automatically resolve
   obligations on lists generated by [Equations]. *)
#[export] Hint Rewrite app_nil_l : rlist.

(* Setting the default tactics for obligations to be [hauto] using the [rlist]
   hint database. *)
#[local] Obligation Tactic := try hauto db:rlist.

Module D.

(* The empty cadeque. *)
Equations empty {A : Type} : { d : cadeque A | cadeque_seq d = [] } :=
empty := ? T Empty.

(* Pushes on a cadeque. *)
Equations push {A : Type} (x : A) (d : cadeque A) :
  { d' : cadeque A | cadeque_seq d' = [x] ++ cadeque_seq d } :=
push x (T c) with semi_push (Ground x) (Semi c) => {
  | ? sd with regularize sd => { | ? d := ? d } }.

(* Injects on a cadeque. *)
Equations inject {A : Type} (d : cadeque A) (x : A) :
  { d' : cadeque A | cadeque_seq d' = cadeque_seq d ++ [x] } :=
inject (T c) x with semi_inject (Semi c) (Ground x) => {
  | ? sd with regularize sd => { | ? d := ? d } }.

(* Pops from a cadeque. *)
Equations pop {A : Type} (d : cadeque A) :
  { o : option (A * cadeque A) |
    cadeque_seq d = match o with
    | None => []
    | Some (x, d') => [x] ++ cadeque_seq d'
    end } :=
pop (T Empty) := ? None;
pop (T chain) with pop_green chain => {
  | ? (Ground x, Semi chain1) with ensure_green chain1 => {
    | ? chain2 := ? Some (x, T chain2) } }.

(* Ejects from a cadeque. *)
Equations eject {A : Type} (d : cadeque A) :
  { o : option (cadeque A * A) |
    cadeque_seq d = match o with
    | None => []
    | Some (d', x) => cadeque_seq d' ++ [x]
    end } :=
eject (T Empty) := ? None;
eject (T chain) with eject_green chain => {
  | ? (Semi chain1, Ground x) with ensure_green chain1 => {
    | ? chain2 := ? Some (T chain2, x) } }.

(* Concatenates two cadeques. *)
Equations concat {A : Type} (d1 d2 : cadeque A) :
  { d3 : cadeque A | cadeque_seq d3 = cadeque_seq d1 ++ cadeque_seq d2 } :=
concat (T c1) (T c2) with semi_concat (Semi c1) (Semi c2) => {
  | ? sd with regularize sd => { | ? d := ? d } }.

End D.

(* Extract the code to OCaml. *)
(* Separate Extraction D models. *)

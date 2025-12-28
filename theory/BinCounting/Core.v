From Coq Require Import Lia.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.

From Deques.Color Require Import GYR.

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A type for packets. *)
Inductive packet : color -> Type :=
  | Hole       : packet uncolored
  | GDigit {y} : packet (Mix NoGreen y NoRed) -> packet green
  | YDigit {y} : packet (Mix NoGreen y NoRed) -> packet yellow
  | RDigit {y} : packet (Mix NoGreen y NoRed) -> packet red.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> Type :=
  | G {g r} : regularity green  (Mix g NoYellow r)
  | Y       : regularity yellow green
  | R       : regularity red    green.

(* A type for chains. *)
Inductive chain : color -> Type :=
  | Empty : chain green
  | Chain {C1 C2 : color} :
    regularity C1 C2 -> packet C1 -> chain C2 -> chain C1.

(* A type for numbers. *)
Inductive number : Type :=
  | T {g y} : chain (Mix g y NoRed) -> number.

(* +------------------------------------------------------------------------+ *)
(* |                                 Models                                 | *)
(* +------------------------------------------------------------------------+ *)

(* Model functions are transparent. *)
Set Equations Transparent.

(* Returns the natural number associated to a packet, provided the natural
   number associated to its hole. *)
Equations packet_nat {C : color} : packet C -> nat -> nat :=
packet_nat Hole n := n;
packet_nat (GDigit pkt) n := 0 + 2 * packet_nat pkt n;
packet_nat (YDigit pkt) n := 1 + 2 * packet_nat pkt n;
packet_nat (RDigit pkt) n := 2 + 2 * packet_nat pkt n.

(* Returns the natural number associated to a chain. *)
Equations chain_nat {C : color} : chain C -> nat :=
chain_nat Empty := 0;
chain_nat (Chain _ pkt c) := packet_nat pkt (chain_nat c).

(* Returns the natural number associated to a number. *)
Equations number_nat : number -> nat :=
number_nat (T c) := chain_nat c.

Unset Equations Transparent.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Notation for dependent types hiding the property on [x]. *)
Notation "? x" := (@exist _ _ x _) (at level 100).

(* The following attempt at defining [green_of_red] fails, because it is
   an ordinary Rocq [Definition], and it is incomplete: not every case is
   covered. The cases that we have omitted are dead, but Rocq cannot see
   it. The error message is:

   Non exhaustive pattern-matching: no clause found for pattern Chain G _ _ *)

Fail Definition green_of_red (c : chain red) : chain green :=
  match c with
  | Chain R (RDigit Hole) Empty =>
      Chain G (GDigit (YDigit Hole)) Empty
  | Chain R (RDigit Hole) (Chain G (GDigit body) c) =>
      Chain G (GDigit (YDigit body)) c
  | Chain R (RDigit (YDigit body)) c =>
      Chain G (GDigit Hole) (Chain R (RDigit body) c)
  end.

(* To avoid this problem, we use Equations. Equations assumes that a missing
   branch must be dead. If it can prove automatically that a branch is dead
   then it does not warn about the absence of this branch. If it is unable to
   prove that a branch is dead then it generates a proof obligation which the
   user must explicitly address. The tactic that is implicitly used by
   Equations can be chosen via the command [Obligation Tactic]. For more
   information, see the Equations tutorial:
   https://rocq-prover.github.io/platform-docs/equations/tutorial_obligations.html *)

(* Our second definition of [green_of_red] uses Equations. This time, Equations
   is able to automatically prove that the missing branches are dead, so it does
   not complain about them, and the definition is accepted. *)

(* In this definition, we do not annotate [green_of_red] with a postcondition;
   that is, we do not state, a priori, that the chain [green_of_red c]
   represents the same natural number as the chain [c]. Instead, we prove
   this fact a posteriori, in the lemma [green_of_red_correct]. The proof
   is verbose because it must inspect the definition of [green_of_red]. *)

Module NoPostcondition.

  (* [green_of_red] turns a red chain into a green chain. *)
  Equations green_of_red : chain red -> chain green :=
  green_of_red (Chain R (RDigit Hole) Empty) :=
    Chain G (GDigit (YDigit Hole)) Empty;
  green_of_red (Chain R (RDigit Hole) (Chain G (GDigit body) c)) :=
    Chain G (GDigit (YDigit body)) c;
  green_of_red (Chain R (RDigit (YDigit body)) c) :=
    Chain G (GDigit Hole) (Chain R (RDigit body) c).

  (* A red chain [c] and the chain [green_of_red c] represent the
     same natural number. *)
  Lemma green_of_red_correct (c : chain red) :
    chain_nat (green_of_red c) = chain_nat c.
  Proof.
    dependent elimination c.
    dependent elimination r.
    dependent elimination p.
    dependent elimination p1.
    - dependent elimination c.
      + simp green_of_red.
        reflexivity.
      + dependent elimination r.
        dependent elimination p.
        simp green_of_red. simpl.
        lia.
    - simp green_of_red.
      simpl.
      lia.
  Qed.

End NoPostcondition.

(* Our third and last definition of [green_of_red] also uses Equations. This
   time, we annotate [green_of_red] with the postcondition
   [chain_nat c' = chain_nat c]: that is, we claim, a priori, that the chain
   [green_of_red c] represents the same natural number as the chain [c].
   This gives rise to a number of proof obligations, all of which are
   automatically proved by Equations. *)

Equations green_of_red (c : chain red) :
  { c' : chain green | chain_nat c' = chain_nat c } :=
green_of_red (Chain R (RDigit Hole) Empty) :=
  ? Chain G (GDigit (YDigit Hole)) Empty;
green_of_red (Chain R (RDigit Hole) (Chain G (GDigit body) c)) :=
  ? Chain G (GDigit (YDigit body)) c;
green_of_red (Chain R (RDigit (YDigit body)) c) :=
  ? Chain G (GDigit Hole) (Chain R (RDigit body) c).

(* [ensure_green] turns a green or red chain into a green chain. *)
Equations ensure_green {g r} (c : chain (Mix g NoYellow r)) :
  { c' : chain green | chain_nat c' = chain_nat c } :=
ensure_green Empty := ? Empty;
ensure_green (Chain G pkt c) := ? Chain G pkt c;
ensure_green (Chain R pkt c) with green_of_red (Chain R pkt c) => {
  | ? c' := ? c' }.

(* +------------------------------------------------------------------------+ *)
(* |                               Operation                                | *)
(* +------------------------------------------------------------------------+ *)

(* [succ n] returns the successor of the number [n]. *)

(* Again, we define [succ] using Equations, and we state its correctness
   property [number_nat n' = S (number_nat n)] as part of its type.

   Somewhat miraculously perhaps, all of the type-checking obligations and proof
   obligations are automatically proved, so we have nothing left to prove by
   hand. The system automatically checks that all cases are covered and that a
   correct result is returned in each case.

   One of the reasons why full automation is achieved here is that our
   hypotheses and goals are simple; they are equations between colors and
   equations between natural numbers. Another reason is that at every use of
   [ensure_green], the postcondition of [ensure_green] is automatically
   introduced into the context by Equations, and can then be exploited in the
   proof. *)

Equations succ (n : number) :
  { n' : number | number_nat n' = S (number_nat n) } :=
succ (T Empty) :=
  ? T (Chain Y (YDigit Hole) Empty);
succ (T (Chain G (GDigit body) c)) with ensure_green c => {
  | ? c' := ? T (Chain Y (YDigit body) c') };
succ (T (Chain Y (YDigit body) c))
  with ensure_green (Chain R (RDigit body) c) => { | ? c' := ? T c' }.

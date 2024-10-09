open Classes
open Datatypes
open GYOR
open Buffer
(* # mod # -> commented open Core *)
(* # mod # -> commented open Models *)
open Types

type __ = Obj.t

module D :
 sig
  val empty : 'a1 cadeque

  type 'a empty_graph =
  | Coq_empty_graph_equation_1

  val empty_graph_rect : (__ -> 'a1) -> 'a2 cadeque -> 'a2 empty_graph -> 'a1

  val empty_graph_correct : 'a1 empty_graph

  val empty_elim : (__ -> 'a1) -> 'a1

  val coq_FunctionalElimination_empty : (__ -> __) -> __

  val coq_FunctionalInduction_empty :
    (__ -> __ cadeque) coq_FunctionalInduction

  val ground_push_clause_1 : 'a1 stored_triple -> 'a1 chain -> 'a1 cadeque

  val ground_push_clause_2 :
    'a1 stored_triple -> nat -> nat -> color -> color -> regularity -> 'a1
    packet -> 'a1 chain -> 'a1 chain -> 'a1 cadeque

  val ground_push_clause_2_1 :
    'a1 stored_triple -> 'a1 chain -> 'a1 chain -> 'a1 chain -> 'a1 cadeque

  val ground_push : 'a1 stored_triple -> 'a1 cadeque -> 'a1 cadeque

  type 'a ground_push_graph =
  | Coq_ground_push_graph_refinement_1 of 'a stored_triple
     * 'a ground_push_clause_1_graph
  | Coq_ground_push_graph_refinement_2 of 'a stored_triple * nat * nat
     * color * color * regularity * 'a packet * 'a chain
     * 'a ground_push_clause_2_graph
  | Coq_ground_push_graph_refinement_3 of 'a stored_triple * 'a chain
     * 'a chain * 'a ground_push_clause_2_1_graph
  and 'a ground_push_clause_1_graph =
  | Coq_ground_push_clause_1_graph_equation_1 of 'a stored_triple * 'a chain
  and 'a ground_push_clause_2_graph =
  | Coq_ground_push_clause_2_graph_equation_1 of 'a stored_triple * nat *
     nat * color * color * regularity * 'a packet * 'a chain * 'a chain
  and 'a ground_push_clause_2_1_graph =
  | Coq_ground_push_clause_2_1_graph_equation_1 of 'a stored_triple
     * 'a chain * 'a chain * 'a chain

  val ground_push_clause_2_1_graph_mut :
    (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> __ stored_triple -> nat -> nat -> color -> color -> regularity
    -> __ packet -> __ chain -> __ ground_push_clause_2_graph -> 'a3 -> 'a1)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
    ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple ->
    __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat -> color
    -> color -> regularity -> __ packet -> __ chain -> __ chain -> __ -> 'a3)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain -> __ ->
    'a4) -> 'a5 stored_triple -> 'a5 chain -> 'a5 chain -> 'a5 chain -> 'a5
    cadeque -> 'a5 ground_push_clause_2_1_graph -> 'a4

  val ground_push_clause_2_graph_mut :
    (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> __ stored_triple -> nat -> nat -> color -> color -> regularity
    -> __ packet -> __ chain -> __ ground_push_clause_2_graph -> 'a3 -> 'a1)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
    ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple ->
    __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat -> color
    -> color -> regularity -> __ packet -> __ chain -> __ chain -> __ -> 'a3)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain -> __ ->
    'a4) -> 'a5 stored_triple -> nat -> nat -> color -> color -> regularity
    -> 'a5 packet -> 'a5 chain -> 'a5 chain -> 'a5 cadeque -> 'a5
    ground_push_clause_2_graph -> 'a3

  val ground_push_clause_1_graph_mut :
    (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> __ stored_triple -> nat -> nat -> color -> color -> regularity
    -> __ packet -> __ chain -> __ ground_push_clause_2_graph -> 'a3 -> 'a1)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
    ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple ->
    __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat -> color
    -> color -> regularity -> __ packet -> __ chain -> __ chain -> __ -> 'a3)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain -> __ ->
    'a4) -> 'a5 stored_triple -> 'a5 chain -> 'a5 cadeque -> 'a5
    ground_push_clause_1_graph -> 'a2

  val ground_push_graph_mut :
    (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> __ stored_triple -> nat -> nat -> color -> color -> regularity
    -> __ packet -> __ chain -> __ ground_push_clause_2_graph -> 'a3 -> 'a1)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
    ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple ->
    __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat -> color
    -> color -> regularity -> __ packet -> __ chain -> __ chain -> __ -> 'a3)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain -> __ ->
    'a4) -> 'a5 stored_triple -> 'a5 cadeque -> 'a5 cadeque -> 'a5
    ground_push_graph -> 'a1

  val ground_push_graph_rect :
    (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> __ stored_triple -> nat -> nat -> color -> color -> regularity
    -> __ packet -> __ chain -> __ ground_push_clause_2_graph -> 'a3 -> 'a1)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
    ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple ->
    __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat -> color
    -> color -> regularity -> __ packet -> __ chain -> __ chain -> __ -> 'a3)
    -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain -> __ ->
    'a4) -> 'a5 stored_triple -> 'a5 cadeque -> 'a5 cadeque -> 'a5
    ground_push_graph -> 'a1

  val ground_push_graph_correct :
    'a1 stored_triple -> 'a1 cadeque -> 'a1 ground_push_graph

  val ground_push_elim :
    (__ -> __ stored_triple -> __ chain -> __ -> __ -> 'a1) -> (__ -> __
    stored_triple -> nat -> nat -> color -> color -> regularity -> __ packet
    -> __ chain -> __ chain -> __ -> __ -> 'a1) -> (__ -> __ stored_triple ->
    __ chain -> __ chain -> __ chain -> __ -> __ -> 'a1) -> 'a2 stored_triple
    -> 'a2 cadeque -> 'a1

  val coq_FunctionalElimination_ground_push :
    (__ -> __ stored_triple -> __ chain -> __ -> __ -> __) -> (__ -> __
    stored_triple -> nat -> nat -> color -> color -> regularity -> __ packet
    -> __ chain -> __ chain -> __ -> __ -> __) -> (__ -> __ stored_triple ->
    __ chain -> __ chain -> __ chain -> __ -> __ -> __) -> __ stored_triple
    -> __ cadeque -> __

  val coq_FunctionalInduction_ground_push :
    (__ -> __ stored_triple -> __ cadeque -> __ cadeque)
    coq_FunctionalInduction

  val ground_inject_clause_1 : 'a1 stored_triple -> 'a1 chain -> 'a1 cadeque

  val ground_inject_clause_2 :
    nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain ->
    'a1 stored_triple -> 'a1 chain -> 'a1 cadeque

  val ground_inject_clause_2_1 :
    'a1 chain -> 'a1 chain -> 'a1 stored_triple -> 'a1 chain -> 'a1 cadeque

  val ground_inject : 'a1 cadeque -> 'a1 stored_triple -> 'a1 cadeque

  type 'a ground_inject_graph =
  | Coq_ground_inject_graph_refinement_1 of 'a stored_triple
     * 'a ground_inject_clause_1_graph
  | Coq_ground_inject_graph_refinement_2 of nat * nat * color * color
     * regularity * 'a packet * 'a chain * 'a stored_triple
     * 'a ground_inject_clause_2_graph
  | Coq_ground_inject_graph_refinement_3 of 'a chain * 'a chain
     * 'a stored_triple * 'a ground_inject_clause_2_1_graph
  and 'a ground_inject_clause_1_graph =
  | Coq_ground_inject_clause_1_graph_equation_1 of 'a stored_triple * 'a chain
  and 'a ground_inject_clause_2_graph =
  | Coq_ground_inject_clause_2_graph_equation_1 of nat * nat * color *
     color * regularity * 'a packet * 'a chain * 'a stored_triple * 'a chain
  and 'a ground_inject_clause_2_1_graph =
  | Coq_ground_inject_clause_2_1_graph_equation_1 of 'a chain * 'a chain
     * 'a stored_triple * 'a chain

  val ground_inject_clause_2_1_graph_mut :
    (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet -> __
    chain -> __ stored_triple -> __ ground_inject_clause_2_graph -> 'a3 ->
    'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
    ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
    -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color -> color ->
    regularity -> __ packet -> __ chain -> __ stored_triple -> __ chain -> __
    -> 'a3) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __ chain ->
    __ -> 'a4) -> 'a5 chain -> 'a5 chain -> 'a5 stored_triple -> 'a5 chain ->
    'a5 cadeque -> 'a5 ground_inject_clause_2_1_graph -> 'a4

  val ground_inject_clause_2_graph_mut :
    (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet -> __
    chain -> __ stored_triple -> __ ground_inject_clause_2_graph -> 'a3 ->
    'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
    ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
    -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color -> color ->
    regularity -> __ packet -> __ chain -> __ stored_triple -> __ chain -> __
    -> 'a3) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __ chain ->
    __ -> 'a4) -> nat -> nat -> color -> color -> regularity -> 'a5 packet ->
    'a5 chain -> 'a5 stored_triple -> 'a5 chain -> 'a5 cadeque -> 'a5
    ground_inject_clause_2_graph -> 'a3

  val ground_inject_clause_1_graph_mut :
    (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet -> __
    chain -> __ stored_triple -> __ ground_inject_clause_2_graph -> 'a3 ->
    'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
    ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
    -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color -> color ->
    regularity -> __ packet -> __ chain -> __ stored_triple -> __ chain -> __
    -> 'a3) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __ chain ->
    __ -> 'a4) -> 'a5 stored_triple -> 'a5 chain -> 'a5 cadeque -> 'a5
    ground_inject_clause_1_graph -> 'a2

  val ground_inject_graph_mut :
    (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet -> __
    chain -> __ stored_triple -> __ ground_inject_clause_2_graph -> 'a3 ->
    'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
    ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
    -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color -> color ->
    regularity -> __ packet -> __ chain -> __ stored_triple -> __ chain -> __
    -> 'a3) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __ chain ->
    __ -> 'a4) -> 'a5 cadeque -> 'a5 stored_triple -> 'a5 cadeque -> 'a5
    ground_inject_graph -> 'a1

  val ground_inject_graph_rect :
    (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 -> 'a1)
    -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet -> __
    chain -> __ stored_triple -> __ ground_inject_clause_2_graph -> 'a3 ->
    'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
    ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
    -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color -> color ->
    regularity -> __ packet -> __ chain -> __ stored_triple -> __ chain -> __
    -> 'a3) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __ chain ->
    __ -> 'a4) -> 'a5 cadeque -> 'a5 stored_triple -> 'a5 cadeque -> 'a5
    ground_inject_graph -> 'a1

  val ground_inject_graph_correct :
    'a1 cadeque -> 'a1 stored_triple -> 'a1 ground_inject_graph

  val ground_inject_elim :
    (__ -> __ stored_triple -> __ chain -> __ -> __ -> 'a1) -> (__ -> nat ->
    nat -> color -> color -> regularity -> __ packet -> __ chain -> __
    stored_triple -> __ chain -> __ -> __ -> 'a1) -> (__ -> __ chain -> __
    chain -> __ stored_triple -> __ chain -> __ -> __ -> 'a1) -> 'a2 cadeque
    -> 'a2 stored_triple -> 'a1

  val coq_FunctionalElimination_ground_inject :
    (__ -> __ stored_triple -> __ chain -> __ -> __ -> __) -> (__ -> nat ->
    nat -> color -> color -> regularity -> __ packet -> __ chain -> __
    stored_triple -> __ chain -> __ -> __ -> __) -> (__ -> __ chain -> __
    chain -> __ stored_triple -> __ chain -> __ -> __ -> __) -> __ cadeque ->
    __ stored_triple -> __

  val coq_FunctionalInduction_ground_inject :
    (__ -> __ cadeque -> __ stored_triple -> __ cadeque)
    coq_FunctionalInduction

  val push : 'a1 -> 'a1 cadeque -> 'a1 cadeque

  type 'a push_graph =
  | Coq_push_graph_equation_1 of 'a * 'a cadeque

  val push_graph_rect :
    (__ -> __ -> __ cadeque -> 'a1) -> 'a2 -> 'a2 cadeque -> 'a2 cadeque ->
    'a2 push_graph -> 'a1

  val push_graph_correct : 'a1 -> 'a1 cadeque -> 'a1 push_graph

  val push_elim : (__ -> __ -> __ cadeque -> 'a1) -> 'a2 -> 'a2 cadeque -> 'a1

  val coq_FunctionalElimination_push :
    (__ -> __ -> __ cadeque -> __) -> __ -> __ cadeque -> __

  val coq_FunctionalInduction_push :
    (__ -> __ -> __ cadeque -> __ cadeque) coq_FunctionalInduction

  val inject : 'a1 cadeque -> 'a1 -> 'a1 cadeque

  type 'a inject_graph =
  | Coq_inject_graph_equation_1 of 'a cadeque * 'a

  val inject_graph_rect :
    (__ -> __ cadeque -> __ -> 'a1) -> 'a2 cadeque -> 'a2 -> 'a2 cadeque ->
    'a2 inject_graph -> 'a1

  val inject_graph_correct : 'a1 cadeque -> 'a1 -> 'a1 inject_graph

  val inject_elim :
    (__ -> __ cadeque -> __ -> 'a1) -> 'a2 cadeque -> 'a2 -> 'a1

  val coq_FunctionalElimination_inject :
    (__ -> __ cadeque -> __ -> __) -> __ cadeque -> __ -> __

  val coq_FunctionalInduction_inject :
    (__ -> __ cadeque -> __ -> __ cadeque) coq_FunctionalInduction

  val pop_clause_2_clause_1 :
    nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain ->
    'a1 -> nat -> color -> color -> 'a1 chain -> 'a1 chain -> ('a1, 'a1
    cadeque) prod option

  val pop_clause_2 :
    nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain ->
    ('a1 stored_triple, 'a1 semi_cadeque) prod -> ('a1, 'a1 cadeque) prod
    option

  val pop_clause_2_1_clause_1 :
    'a1 chain -> 'a1 chain -> 'a1 -> nat -> color -> color -> 'a1 chain ->
    'a1 chain -> ('a1, 'a1 cadeque) prod option

  val pop_clause_2_1 :
    'a1 chain -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) prod ->
    ('a1, 'a1 cadeque) prod option

  val pop : 'a1 cadeque -> ('a1, 'a1 cadeque) prod option

  type 'a pop_graph =
  | Coq_pop_graph_equation_1
  | Coq_pop_graph_refinement_2 of nat * nat * color * color * regularity
     * 'a packet * 'a chain * 'a pop_clause_2_graph
  | Coq_pop_graph_refinement_3 of 'a chain * 'a chain
     * 'a pop_clause_2_1_graph
  and 'a pop_clause_2_graph =
  | Coq_pop_clause_2_graph_refinement_1 of nat * nat * color * color
     * regularity * 'a packet * 'a chain * 'a * nat * color * color
     * 'a chain * 'a pop_clause_2_clause_1_graph
  and 'a0 pop_clause_2_clause_1_graph =
  | Coq_pop_clause_2_clause_1_graph_equation_1 of nat * nat * color *
     color * regularity * 'a0 packet * 'a0 chain * 'a0 * nat * color *
     color * 'a0 chain * 'a0 chain
  and 'a pop_clause_2_1_graph =
  | Coq_pop_clause_2_1_graph_refinement_1 of 'a chain * 'a chain * 'a *
     nat * color * color * 'a chain * 'a pop_clause_2_1_clause_1_graph
  and 'a0 pop_clause_2_1_clause_1_graph =
  | Coq_pop_clause_2_1_clause_1_graph_equation_1 of 'a0 chain * 'a0 chain
     * 'a0 * nat * color * color * 'a0 chain * 'a0 chain

  val pop_clause_2_1_clause_1_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> nat
    -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ -> __ pop_clause_2_clause_1_graph
    -> 'a3 -> 'a2) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __ -> nat -> color
    -> color -> __ chain -> __ -> __ pop_clause_2_1_clause_1_graph -> 'a5 ->
    'a4) -> (__ -> __ chain -> __ chain -> __ -> nat -> color -> color -> __
    chain -> __ chain -> __ -> __ -> 'a5) -> 'a6 chain -> 'a6 chain -> 'a6 ->
    nat -> color -> color -> 'a6 chain -> 'a6 chain -> ('a6, 'a6 cadeque)
    prod option -> 'a6 pop_clause_2_1_clause_1_graph -> 'a5

  val pop_clause_2_1_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> nat
    -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ -> __ pop_clause_2_clause_1_graph
    -> 'a3 -> 'a2) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __ -> nat -> color
    -> color -> __ chain -> __ -> __ pop_clause_2_1_clause_1_graph -> 'a5 ->
    'a4) -> (__ -> __ chain -> __ chain -> __ -> nat -> color -> color -> __
    chain -> __ chain -> __ -> __ -> 'a5) -> 'a6 chain -> 'a6 chain -> ('a6
    stored_triple, 'a6 semi_cadeque) prod -> ('a6, 'a6 cadeque) prod option
    -> 'a6 pop_clause_2_1_graph -> 'a4

  val pop_clause_2_clause_1_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> nat
    -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ -> __ pop_clause_2_clause_1_graph
    -> 'a3 -> 'a2) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __ -> nat -> color
    -> color -> __ chain -> __ -> __ pop_clause_2_1_clause_1_graph -> 'a5 ->
    'a4) -> (__ -> __ chain -> __ chain -> __ -> nat -> color -> color -> __
    chain -> __ chain -> __ -> __ -> 'a5) -> nat -> nat -> color -> color ->
    regularity -> 'a6 packet -> 'a6 chain -> 'a6 -> nat -> color -> color ->
    'a6 chain -> 'a6 chain -> ('a6, 'a6 cadeque) prod option -> 'a6
    pop_clause_2_clause_1_graph -> 'a3

  val pop_clause_2_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> nat
    -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ -> __ pop_clause_2_clause_1_graph
    -> 'a3 -> 'a2) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __ -> nat -> color
    -> color -> __ chain -> __ -> __ pop_clause_2_1_clause_1_graph -> 'a5 ->
    'a4) -> (__ -> __ chain -> __ chain -> __ -> nat -> color -> color -> __
    chain -> __ chain -> __ -> __ -> 'a5) -> nat -> nat -> color -> color ->
    regularity -> 'a6 packet -> 'a6 chain -> ('a6 stored_triple, 'a6
    semi_cadeque) prod -> ('a6, 'a6 cadeque) prod option -> 'a6
    pop_clause_2_graph -> 'a2

  val pop_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> nat
    -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ -> __ pop_clause_2_clause_1_graph
    -> 'a3 -> 'a2) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __ -> nat -> color
    -> color -> __ chain -> __ -> __ pop_clause_2_1_clause_1_graph -> 'a5 ->
    'a4) -> (__ -> __ chain -> __ chain -> __ -> nat -> color -> color -> __
    chain -> __ chain -> __ -> __ -> 'a5) -> 'a6 cadeque -> ('a6, 'a6
    cadeque) prod option -> 'a6 pop_graph -> 'a1

  val pop_graph_rect :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> nat
    -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ -> __ pop_clause_2_clause_1_graph
    -> 'a3 -> 'a2) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __ -> nat -> color
    -> color -> __ chain -> __ -> __ pop_clause_2_1_clause_1_graph -> 'a5 ->
    'a4) -> (__ -> __ chain -> __ chain -> __ -> nat -> color -> color -> __
    chain -> __ chain -> __ -> __ -> 'a5) -> 'a6 cadeque -> ('a6, 'a6
    cadeque) prod option -> 'a6 pop_graph -> 'a1

  val pop_graph_correct : 'a1 cadeque -> 'a1 pop_graph

  val pop_elim :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> __ -> __ -> 'a1) -> (__ -> __ chain -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __ -> __ ->
    'a1) -> 'a2 cadeque -> 'a1

  val coq_FunctionalElimination_pop :
    (__ -> __) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain
    -> __ -> __ -> __ -> __ -> __) -> (__ -> __ chain -> __ chain -> __ ->
    nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __ -> __ ->
    __) -> __ cadeque -> __

  val coq_FunctionalInduction_pop :
    (__ -> __ cadeque -> (__, __ cadeque) prod option) coq_FunctionalInduction

  val eject_clause_2_clause_1 :
    nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain ->
    nat -> color -> color -> 'a1 chain -> 'a1 chain -> 'a1 -> ('a1 cadeque,
    'a1) prod option

  val eject_clause_2 :
    nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain ->
    ('a1 semi_cadeque, 'a1 stored_triple) prod -> ('a1 cadeque, 'a1) prod
    option

  val eject_clause_2_1_clause_1 :
    'a1 chain -> 'a1 chain -> nat -> color -> color -> 'a1 chain -> 'a1 chain
    -> 'a1 -> ('a1 cadeque, 'a1) prod option

  val eject_clause_2_1 :
    'a1 chain -> 'a1 chain -> ('a1 semi_cadeque, 'a1 stored_triple) prod ->
    ('a1 cadeque, 'a1) prod option

  val eject : 'a1 cadeque -> ('a1 cadeque, 'a1) prod option

  type 'a eject_graph =
  | Coq_eject_graph_equation_1
  | Coq_eject_graph_refinement_2 of nat * nat * color * color * regularity
     * 'a packet * 'a chain * 'a eject_clause_2_graph
  | Coq_eject_graph_refinement_3 of 'a chain * 'a chain
     * 'a eject_clause_2_1_graph
  and 'a eject_clause_2_graph =
  | Coq_eject_clause_2_graph_refinement_1 of nat * nat * color * color
     * regularity * 'a packet * 'a chain * nat * color * color * 'a chain
     * 'a * 'a eject_clause_2_clause_1_graph
  and 'a0 eject_clause_2_clause_1_graph =
  | Coq_eject_clause_2_clause_1_graph_equation_1 of nat * nat * color *
     color * regularity * 'a0 packet * 'a0 chain * nat * color * color
     * 'a0 chain * 'a0 chain * 'a0
  and 'a eject_clause_2_1_graph =
  | Coq_eject_clause_2_1_graph_refinement_1 of 'a chain * 'a chain *
     nat * color * color * 'a chain * 'a * 'a eject_clause_2_1_clause_1_graph
  and 'a0 eject_clause_2_1_clause_1_graph =
  | Coq_eject_clause_2_1_clause_1_graph_equation_1 of 'a0 chain * 'a0 chain
     * nat * color * color * 'a0 chain * 'a0 chain * 'a0

  val eject_clause_2_1_clause_1_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
    nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
    nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
    color -> color -> regularity -> __ packet -> __ chain -> nat -> color ->
    color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
    chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain -> __
    chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
    -> 'a5) -> 'a6 chain -> 'a6 chain -> nat -> color -> color -> 'a6 chain
    -> 'a6 chain -> 'a6 -> ('a6 cadeque, 'a6) prod option -> 'a6
    eject_clause_2_1_clause_1_graph -> 'a5

  val eject_clause_2_1_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
    nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
    nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
    color -> color -> regularity -> __ packet -> __ chain -> nat -> color ->
    color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
    chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain -> __
    chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
    -> 'a5) -> 'a6 chain -> 'a6 chain -> ('a6 semi_cadeque, 'a6
    stored_triple) prod -> ('a6 cadeque, 'a6) prod option -> 'a6
    eject_clause_2_1_graph -> 'a4

  val eject_clause_2_clause_1_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
    nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
    nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
    color -> color -> regularity -> __ packet -> __ chain -> nat -> color ->
    color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
    chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain -> __
    chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
    -> 'a5) -> nat -> nat -> color -> color -> regularity -> 'a6 packet ->
    'a6 chain -> nat -> color -> color -> 'a6 chain -> 'a6 chain -> 'a6 ->
    ('a6 cadeque, 'a6) prod option -> 'a6 eject_clause_2_clause_1_graph -> 'a3

  val eject_clause_2_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
    nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
    nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
    color -> color -> regularity -> __ packet -> __ chain -> nat -> color ->
    color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
    chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain -> __
    chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
    -> 'a5) -> nat -> nat -> color -> color -> regularity -> 'a6 packet ->
    'a6 chain -> ('a6 semi_cadeque, 'a6 stored_triple) prod -> ('a6 cadeque,
    'a6) prod option -> 'a6 eject_clause_2_graph -> 'a2

  val eject_graph_mut :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
    nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
    nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
    color -> color -> regularity -> __ packet -> __ chain -> nat -> color ->
    color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
    chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain -> __
    chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
    -> 'a5) -> 'a6 cadeque -> ('a6 cadeque, 'a6) prod option -> 'a6
    eject_graph -> 'a1

  val eject_graph_rect :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
    chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
    nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
    nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
    color -> color -> regularity -> __ packet -> __ chain -> nat -> color ->
    color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
    chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ -> __
    eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain -> __
    chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
    -> 'a5) -> 'a6 cadeque -> ('a6 cadeque, 'a6) prod option -> 'a6
    eject_graph -> 'a1

  val eject_graph_correct : 'a1 cadeque -> 'a1 eject_graph

  val eject_elim :
    (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> nat -> color -> color -> __ chain -> __ chain -> __
    -> __ -> __ -> __ -> __ -> 'a1) -> (__ -> __ chain -> __ chain -> nat ->
    color -> color -> __ chain -> __ chain -> __ -> __ -> __ -> __ -> __ ->
    'a1) -> 'a2 cadeque -> 'a1

  val coq_FunctionalElimination_eject :
    (__ -> __) -> (__ -> nat -> nat -> color -> color -> regularity -> __
    packet -> __ chain -> nat -> color -> color -> __ chain -> __ chain -> __
    -> __ -> __ -> __ -> __ -> __) -> (__ -> __ chain -> __ chain -> nat ->
    color -> color -> __ chain -> __ chain -> __ -> __ -> __ -> __ -> __ ->
    __) -> __ cadeque -> __

  val coq_FunctionalInduction_eject :
    (__ -> __ cadeque -> (__ cadeque, __) prod option) coq_FunctionalInduction

  val concat_clause_1_clause_1 :
    nat -> 'a1 chain -> 'a1 stored_triple vector -> nat -> 'a1 chain -> 'a1
    cadeque -> 'a1 cadeque

  val concat_clause_1_clause_2_clause_1 :
    nat -> 'a1 chain -> 'a1 triple -> nat -> 'a1 chain -> 'a1 stored_triple
    vector -> 'a1 cadeque -> 'a1 cadeque

  val concat_clause_1_clause_2_clause_2_clause_1 :
    nat -> 'a1 chain -> 'a1 triple -> 'a1 chain -> nat -> 'a1 chain -> 'a1
    triple -> 'a1 chain -> 'a1 cadeque

  val concat_clause_1_clause_2_clause_2 :
    nat -> 'a1 chain -> 'a1 triple -> 'a1 chain -> nat -> 'a1 chain -> 'a1
    triple -> 'a1 cadeque

  val concat_clause_1_clause_2 :
    nat -> 'a1 chain -> 'a1 triple -> nat -> 'a1 chain -> 'a1
    left_right_triple -> 'a1 cadeque

  val concat_clause_1 :
    nat -> 'a1 chain -> 'a1 left_right_triple -> nat -> 'a1 chain -> 'a1
    cadeque

  val concat : 'a1 cadeque -> 'a1 cadeque -> 'a1 cadeque

  type 'a concat_graph =
  | Coq_concat_graph_refinement_1 of nat * 'a chain * nat * 'a chain
     * 'a concat_clause_1_graph
  and 'a concat_clause_1_graph =
  | Coq_concat_clause_1_graph_refinement_1 of nat * 'a chain
     * 'a stored_triple vector * nat * 'a chain
     * 'a concat_clause_1_clause_1_graph
  | Coq_concat_clause_1_graph_refinement_2 of nat * 'a chain * 'a triple
     * nat * 'a chain * 'a concat_clause_1_clause_2_graph
  and 'a0 concat_clause_1_clause_1_graph =
  | Coq_concat_clause_1_clause_1_graph_equation_1 of nat * 'a0 chain
     * 'a0 stored_triple vector * nat * 'a0 chain * 'a0 cadeque
  and 'a1 concat_clause_1_clause_2_graph =
  | Coq_concat_clause_1_clause_2_graph_refinement_1 of nat * 'a1 chain
     * 'a1 triple * nat * 'a1 chain * 'a1 stored_triple vector
     * 'a1 concat_clause_1_clause_2_clause_1_graph
  | Coq_concat_clause_1_clause_2_graph_refinement_2 of nat * 'a1 chain
     * 'a1 triple * nat * 'a1 chain * 'a1 triple
     * 'a1 concat_clause_1_clause_2_clause_2_graph
  and 'a concat_clause_1_clause_2_clause_1_graph =
  | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 of nat * 'a chain
     * 'a triple * nat * 'a chain * 'a stored_triple vector * 'a cadeque
  and 'a0 concat_clause_1_clause_2_clause_2_graph =
  | Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 of nat
     * 'a0 chain * 'a0 triple * 'a0 chain * nat * 'a0 chain * 'a0 triple
     * 'a0 concat_clause_1_clause_2_clause_2_clause_1_graph
  and 'a0 concat_clause_1_clause_2_clause_2_clause_1_graph =
  | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 of
     nat * 'a0 chain * 'a0 triple * 'a0 chain * nat * 'a0 chain * 'a0 triple
     * 'a0 chain

  val concat_clause_1_clause_2_clause_2_clause_1_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain -> 'a8
    triple -> 'a8 chain -> nat -> 'a8 chain -> 'a8 triple -> 'a8 chain -> 'a8
    cadeque -> 'a8 concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7

  val concat_clause_1_clause_2_clause_2_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain -> 'a8
    triple -> 'a8 chain -> nat -> 'a8 chain -> 'a8 triple -> 'a8 cadeque ->
    'a8 concat_clause_1_clause_2_clause_2_graph -> 'a6

  val concat_clause_1_clause_2_clause_1_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain -> 'a8
    triple -> nat -> 'a8 chain -> 'a8 stored_triple vector -> 'a8 cadeque ->
    'a8 cadeque -> 'a8 concat_clause_1_clause_2_clause_1_graph -> 'a5

  val concat_clause_1_clause_2_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain -> 'a8
    triple -> nat -> 'a8 chain -> 'a8 left_right_triple -> 'a8 cadeque -> 'a8
    concat_clause_1_clause_2_graph -> 'a4

  val concat_clause_1_clause_1_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain -> 'a8
    stored_triple vector -> nat -> 'a8 chain -> 'a8 cadeque -> 'a8 cadeque ->
    'a8 concat_clause_1_clause_1_graph -> 'a3

  val concat_clause_1_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain -> 'a8
    left_right_triple -> nat -> 'a8 chain -> 'a8 cadeque -> 'a8
    concat_clause_1_graph -> 'a2

  val concat_graph_mut :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> 'a8 cadeque -> 'a8
    cadeque -> 'a8 cadeque -> 'a8 concat_graph -> 'a1

  val concat_graph_rect :
    (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph ->
    'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector -> __ ->
    nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 -> 'a2) ->
    (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain -> __
    concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque -> __
    -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain
    -> __ stored_triple vector -> __ -> __
    concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
    __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector
    -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain -> __ triple
    -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
    concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__ ->
    nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> 'a7) -> 'a8 cadeque -> 'a8
    cadeque -> 'a8 cadeque -> 'a8 concat_graph -> 'a1

  val concat_graph_correct : 'a1 cadeque -> 'a1 cadeque -> 'a1 concat_graph

  val concat_elim :
    (__ -> nat -> __ chain -> __ stored_triple vector -> __ -> nat -> __
    chain -> __ cadeque -> __ -> __ -> __ -> 'a1) -> (__ -> nat -> __ chain
    -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector -> __
    cadeque -> __ -> __ -> __ -> __ -> __ -> 'a1) -> (__ -> nat -> __ chain
    -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain -> __ triple ->
    __ chain -> __ -> __ -> __ -> __ -> __ -> __ -> 'a1) -> 'a2 cadeque ->
    'a2 cadeque -> 'a1

  val coq_FunctionalElimination_concat :
    (__ -> nat -> __ chain -> __ stored_triple vector -> __ -> nat -> __
    chain -> __ cadeque -> __ -> __ -> __ -> __) -> (__ -> nat -> __ chain ->
    __ triple -> __ -> nat -> __ chain -> __ stored_triple vector -> __
    cadeque -> __ -> __ -> __ -> __ -> __ -> __) -> (__ -> nat -> __ chain ->
    __ triple -> __ chain -> __ -> __ -> nat -> __ chain -> __ triple -> __
    chain -> __ -> __ -> __ -> __ -> __ -> __ -> __) -> __ cadeque -> __
    cadeque -> __

  val coq_FunctionalInduction_concat :
    (__ -> __ cadeque -> __ cadeque -> __ cadeque) coq_FunctionalInduction
 end

open Classes
open Datatypes
open GYOR
(* # mod # -> commented open List *)
(* # mod # -> commented open Nat *)
open Buffer
open Types

type __ = Obj.t

val concat_map_node'_seq :
  (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nkind -> color -> 'a1 node'
  -> 'a2 list -> 'a2 list

val stored_triple_seq_functional :
  (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind ->
  nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
  nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat ->
  nkind -> color -> color -> __ chain -> __ list) -> nat -> 'a1 stored_triple
  -> 'a1 list

val body_seq_functional :
  (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind ->
  nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
  nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat ->
  nkind -> color -> color -> __ chain -> __ list) -> nat -> nat -> nkind ->
  nkind -> 'a1 body -> 'a1 list -> 'a1 list

val packet_seq_functional :
  (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind ->
  nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
  nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat ->
  nkind -> color -> color -> __ chain -> __ list) -> nat -> nat -> nat ->
  nkind -> color -> 'a1 packet -> 'a1 list -> 'a1 list

val chain_seq_functional :
  (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind ->
  nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
  nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat ->
  nkind -> color -> color -> __ chain -> __ list) -> nat -> nat -> nkind ->
  color -> color -> 'a1 chain -> 'a1 list

val stored_triple_seq : nat -> 'a1 stored_triple -> 'a1 list

val body_seq :
  nat -> nat -> nkind -> nkind -> 'a1 body -> 'a1 list -> 'a1 list

val packet_seq :
  nat -> nat -> nat -> nkind -> color -> 'a1 packet -> 'a1 list -> 'a1 list

val chain_seq : nat -> nat -> nkind -> color -> color -> 'a1 chain -> 'a1 list

type 'a stored_triple_seq_graph =
| Coq_stored_triple_seq_graph_equation_1 of 'a
| Coq_stored_triple_seq_graph_equation_2 of nat * nat
   * 'a stored_triple suffix'
| Coq_stored_triple_seq_graph_equation_3 of nat * nat * nat * nat * color
   * color * 'a stored_triple prefix' * 'a chain * 'a stored_triple suffix'
   * 'a chain_seq_graph
and 'a body_seq_graph =
| Coq_body_seq_graph_equation_1 of nat * nkind * 'a list
| Coq_body_seq_graph_equation_2 of nat * nat * nkind * nkind * yellow_hue
   * orange_hue * 'a stored_triple node' * 'a body * 'a list
   * 'a body_seq_graph
| Coq_body_seq_graph_equation_3 of nat * nat * nkind * nkind * color
   * 'a stored_triple node' * 'a body * 'a chain * 'a list
   * 'a body_seq_graph * 'a chain_seq_graph
| Coq_body_seq_graph_equation_4 of nat * nat * nkind * nkind
   * 'a stored_triple node' * 'a chain * 'a body * 'a list
   * 'a chain_seq_graph * 'a body_seq_graph
and 'a packet_seq_graph =
| Coq_packet_seq_graph_equation_1 of nat * nat * nat * nkind * green_hue
   * red_hue * nkind * 'a body * 'a stored_triple node' * 'a list
   * 'a body_seq_graph
and 'a chain_seq_graph =
| Coq_chain_seq_graph_equation_1 of nat
| Coq_chain_seq_graph_equation_2 of nat * nkind * color * nat * nat *
   color * color * regularity * 'a packet * 'a chain * 'a chain_seq_graph
   * 'a packet_seq_graph
| Coq_chain_seq_graph_equation_3 of nat * color * color * 'a chain *
   'a chain * 'a chain_seq_graph * 'a chain_seq_graph

val chain_seq_graph_mut :
  (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' -> 'a1)
  -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> __ chain_seq_graph ->
  'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> yellow_hue -> orange_hue -> __ stored_triple node' ->
  __ body -> __ list -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> color -> __ stored_triple node' -> __ body -> __ chain
  -> __ list -> __ body_seq_graph -> 'a2 -> __ chain_seq_graph -> 'a4 -> 'a2)
  -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
  chain -> __ body -> __ list -> __ chain_seq_graph -> 'a4 -> __
  body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat -> nat -> nkind ->
  green_hue -> red_hue -> nkind -> __ body -> __ stored_triple node' -> __
  list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__ -> nat -> 'a4) -> (__ ->
  nat -> nkind -> color -> nat -> nat -> color -> color -> regularity -> __
  packet -> __ chain -> __ chain_seq_graph -> 'a4 -> __ packet_seq_graph ->
  'a3 -> 'a4) -> (__ -> nat -> color -> color -> __ chain -> __ chain -> __
  chain_seq_graph -> 'a4 -> __ chain_seq_graph -> 'a4 -> 'a4) -> nat -> nat
  -> nkind -> color -> color -> 'a5 chain -> 'a5 list -> 'a5 chain_seq_graph
  -> 'a4

val packet_seq_graph_mut :
  (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' -> 'a1)
  -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> __ chain_seq_graph ->
  'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> yellow_hue -> orange_hue -> __ stored_triple node' ->
  __ body -> __ list -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> color -> __ stored_triple node' -> __ body -> __ chain
  -> __ list -> __ body_seq_graph -> 'a2 -> __ chain_seq_graph -> 'a4 -> 'a2)
  -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
  chain -> __ body -> __ list -> __ chain_seq_graph -> 'a4 -> __
  body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat -> nat -> nkind ->
  green_hue -> red_hue -> nkind -> __ body -> __ stored_triple node' -> __
  list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__ -> nat -> 'a4) -> (__ ->
  nat -> nkind -> color -> nat -> nat -> color -> color -> regularity -> __
  packet -> __ chain -> __ chain_seq_graph -> 'a4 -> __ packet_seq_graph ->
  'a3 -> 'a4) -> (__ -> nat -> color -> color -> __ chain -> __ chain -> __
  chain_seq_graph -> 'a4 -> __ chain_seq_graph -> 'a4 -> 'a4) -> nat -> nat
  -> nat -> nkind -> color -> 'a5 packet -> 'a5 list -> 'a5 list -> 'a5
  packet_seq_graph -> 'a3

val body_seq_graph_mut :
  (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' -> 'a1)
  -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> __ chain_seq_graph ->
  'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> yellow_hue -> orange_hue -> __ stored_triple node' ->
  __ body -> __ list -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> color -> __ stored_triple node' -> __ body -> __ chain
  -> __ list -> __ body_seq_graph -> 'a2 -> __ chain_seq_graph -> 'a4 -> 'a2)
  -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
  chain -> __ body -> __ list -> __ chain_seq_graph -> 'a4 -> __
  body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat -> nat -> nkind ->
  green_hue -> red_hue -> nkind -> __ body -> __ stored_triple node' -> __
  list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__ -> nat -> 'a4) -> (__ ->
  nat -> nkind -> color -> nat -> nat -> color -> color -> regularity -> __
  packet -> __ chain -> __ chain_seq_graph -> 'a4 -> __ packet_seq_graph ->
  'a3 -> 'a4) -> (__ -> nat -> color -> color -> __ chain -> __ chain -> __
  chain_seq_graph -> 'a4 -> __ chain_seq_graph -> 'a4 -> 'a4) -> nat -> nat
  -> nkind -> nkind -> 'a5 body -> 'a5 list -> 'a5 list -> 'a5 body_seq_graph
  -> 'a2

val stored_triple_seq_graph_mut :
  (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' -> 'a1)
  -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> __ chain_seq_graph ->
  'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> yellow_hue -> orange_hue -> __ stored_triple node' ->
  __ body -> __ list -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> color -> __ stored_triple node' -> __ body -> __ chain
  -> __ list -> __ body_seq_graph -> 'a2 -> __ chain_seq_graph -> 'a4 -> 'a2)
  -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
  chain -> __ body -> __ list -> __ chain_seq_graph -> 'a4 -> __
  body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat -> nat -> nkind ->
  green_hue -> red_hue -> nkind -> __ body -> __ stored_triple node' -> __
  list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__ -> nat -> 'a4) -> (__ ->
  nat -> nkind -> color -> nat -> nat -> color -> color -> regularity -> __
  packet -> __ chain -> __ chain_seq_graph -> 'a4 -> __ packet_seq_graph ->
  'a3 -> 'a4) -> (__ -> nat -> color -> color -> __ chain -> __ chain -> __
  chain_seq_graph -> 'a4 -> __ chain_seq_graph -> 'a4 -> 'a4) -> nat -> 'a5
  stored_triple -> 'a5 list -> 'a5 stored_triple_seq_graph -> 'a1

val stored_triple_seq_graph_rect :
  (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' -> 'a1)
  -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> __ chain_seq_graph ->
  'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> yellow_hue -> orange_hue -> __ stored_triple node' ->
  __ body -> __ list -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat
  -> nkind -> nkind -> color -> __ stored_triple node' -> __ body -> __ chain
  -> __ list -> __ body_seq_graph -> 'a2 -> __ chain_seq_graph -> 'a4 -> 'a2)
  -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
  chain -> __ body -> __ list -> __ chain_seq_graph -> 'a4 -> __
  body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat -> nat -> nat -> nkind ->
  green_hue -> red_hue -> nkind -> __ body -> __ stored_triple node' -> __
  list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__ -> nat -> 'a4) -> (__ ->
  nat -> nkind -> color -> nat -> nat -> color -> color -> regularity -> __
  packet -> __ chain -> __ chain_seq_graph -> 'a4 -> __ packet_seq_graph ->
  'a3 -> 'a4) -> (__ -> nat -> color -> color -> __ chain -> __ chain -> __
  chain_seq_graph -> 'a4 -> __ chain_seq_graph -> 'a4 -> 'a4) -> (__ -> nat
  -> __ stored_triple -> __ list -> __ stored_triple_seq_graph -> 'a1, (__ ->
  nat -> nat -> nkind -> nkind -> __ body -> __ list -> __ list -> __
  body_seq_graph -> 'a2, (__ -> nat -> nat -> nat -> nkind -> color -> __
  packet -> __ list -> __ list -> __ packet_seq_graph -> 'a3, __ -> nat ->
  nat -> nkind -> color -> color -> __ chain -> __ list -> __ chain_seq_graph
  -> 'a4) prod) prod) prod

val stored_triple_seq_graph_correct :
  (__ -> nat -> __ stored_triple -> __ stored_triple_seq_graph, (__ -> nat ->
  nat -> nkind -> nkind -> __ body -> __ list -> __ body_seq_graph, (__ ->
  nat -> nat -> nat -> nkind -> color -> __ packet -> __ list -> __
  packet_seq_graph, __ -> nat -> nat -> nkind -> color -> color -> __ chain
  -> __ chain_seq_graph) prod) prod) prod

val stored_triple_seq_elim :
  (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' -> 'a1)
  -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> 'a4 -> 'a1) -> (__ ->
  nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind ->
  yellow_hue -> orange_hue -> __ stored_triple node' -> __ body -> __ list ->
  'a2 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __
  stored_triple node' -> __ body -> __ chain -> __ list -> 'a2 -> 'a4 -> 'a2)
  -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
  chain -> __ body -> __ list -> 'a4 -> 'a2 -> 'a2) -> (__ -> nat -> nat ->
  nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
  stored_triple node' -> __ list -> 'a2 -> 'a3) -> (__ -> nat -> 'a4) -> (__
  -> nat -> nkind -> color -> nat -> nat -> color -> color -> regularity ->
  __ packet -> __ chain -> 'a4 -> 'a3 -> 'a4) -> (__ -> nat -> color -> color
  -> __ chain -> __ chain -> 'a4 -> 'a4 -> 'a4) -> (__ -> nat -> __
  stored_triple -> 'a1, (__ -> nat -> nat -> nkind -> nkind -> __ body -> __
  list -> 'a2, (__ -> nat -> nat -> nat -> nkind -> color -> __ packet -> __
  list -> 'a3, __ -> nat -> nat -> nkind -> color -> color -> __ chain ->
  'a4) prod) prod) prod

val coq_FunctionalElimination_stored_triple_seq :
  (__ -> __ -> __) -> (__ -> nat -> nat -> __ stored_triple suffix' -> __) ->
  (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
  prefix' -> __ chain -> __ stored_triple suffix' -> __ -> __) -> (__ -> nat
  -> nkind -> __ list -> __) -> (__ -> nat -> nat -> nkind -> nkind ->
  yellow_hue -> orange_hue -> __ stored_triple node' -> __ body -> __ list ->
  __ -> __) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __
  stored_triple node' -> __ body -> __ chain -> __ list -> __ -> __ -> __) ->
  (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __ chain
  -> __ body -> __ list -> __ -> __ -> __) -> (__ -> nat -> nat -> nat ->
  nkind -> green_hue -> red_hue -> nkind -> __ body -> __ stored_triple node'
  -> __ list -> __ -> __) -> (__ -> nat -> __) -> (__ -> nat -> nkind ->
  color -> nat -> nat -> color -> color -> regularity -> __ packet -> __
  chain -> __ -> __ -> __) -> (__ -> nat -> color -> color -> __ chain -> __
  chain -> __ -> __ -> __) -> (__ -> nat -> __ stored_triple -> __, (__ ->
  nat -> nat -> nkind -> nkind -> __ body -> __ list -> __, (__ -> nat -> nat
  -> nat -> nkind -> color -> __ packet -> __ list -> __, __ -> nat -> nat ->
  nkind -> color -> color -> __ chain -> __) prod) prod) prod

val coq_FunctionalInduction_stored_triple_seq :
  (__ -> nat -> __ stored_triple -> __ list) coq_FunctionalInduction

val green_buffer_seq : nat -> 'a1 green_buffer -> 'a1 list

type 'a green_buffer_seq_graph =
| Coq_green_buffer_seq_graph_equation_1 of nat * nat * 'a stored_triple t

val green_buffer_seq_graph_rect :
  (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2 green_buffer
  -> 'a2 list -> 'a2 green_buffer_seq_graph -> 'a1

val green_buffer_seq_graph_correct :
  nat -> 'a1 green_buffer -> 'a1 green_buffer_seq_graph

val green_buffer_seq_elim :
  (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2 green_buffer
  -> 'a1

val coq_FunctionalElimination_green_buffer_seq :
  (__ -> nat -> nat -> __ stored_triple t -> __) -> nat -> __ green_buffer ->
  __

val coq_FunctionalInduction_green_buffer_seq :
  (__ -> nat -> __ green_buffer -> __ list) coq_FunctionalInduction

val stored_buffer_seq : nat -> 'a1 stored_buffer -> 'a1 list

type 'a stored_buffer_seq_graph =
| Coq_stored_buffer_seq_graph_equation_1 of nat * nat * 'a stored_triple t

val stored_buffer_seq_graph_rect :
  (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2 stored_buffer
  -> 'a2 list -> 'a2 stored_buffer_seq_graph -> 'a1

val stored_buffer_seq_graph_correct :
  nat -> 'a1 stored_buffer -> 'a1 stored_buffer_seq_graph

val stored_buffer_seq_elim :
  (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2 stored_buffer
  -> 'a1

val coq_FunctionalElimination_stored_buffer_seq :
  (__ -> nat -> nat -> __ stored_triple t -> __) -> nat -> __ stored_buffer
  -> __

val coq_FunctionalInduction_stored_buffer_seq :
  (__ -> nat -> __ stored_buffer -> __ list) coq_FunctionalInduction

val triple_seq : nat -> nkind -> color -> 'a1 triple -> 'a1 list

type 'a triple_seq_graph =
| Coq_triple_seq_graph_equation_1 of nat * nkind * color * nat * color
   * color * color * regularity * 'a node * 'a chain

val triple_seq_graph_rect :
  (__ -> nat -> nkind -> color -> nat -> color -> color -> color ->
  regularity -> __ node -> __ chain -> 'a1) -> nat -> nkind -> color -> 'a2
  triple -> 'a2 list -> 'a2 triple_seq_graph -> 'a1

val triple_seq_graph_correct :
  nat -> nkind -> color -> 'a1 triple -> 'a1 triple_seq_graph

val triple_seq_elim :
  (__ -> nat -> nkind -> color -> nat -> color -> color -> color ->
  regularity -> __ node -> __ chain -> 'a1) -> nat -> nkind -> color -> 'a2
  triple -> 'a1

val coq_FunctionalElimination_triple_seq :
  (__ -> nat -> nkind -> color -> nat -> color -> color -> color ->
  regularity -> __ node -> __ chain -> __) -> nat -> nkind -> color -> __
  triple -> __

val coq_FunctionalInduction_triple_seq :
  (__ -> nat -> nkind -> color -> __ triple -> __ list)
  coq_FunctionalInduction

val lr_triple_seq : nat -> nkind -> color -> 'a1 left_right_triple -> 'a1 list

type 'a lr_triple_seq_graph =
| Coq_lr_triple_seq_graph_equation_1 of nat * nkind * 'a stored_triple vector
| Coq_lr_triple_seq_graph_equation_2 of nat * nkind * color * 'a triple

val lr_triple_seq_graph_rect :
  (__ -> nat -> nkind -> __ stored_triple vector -> 'a1) -> (__ -> nat ->
  nkind -> color -> __ triple -> 'a1) -> nat -> nkind -> color -> 'a2
  left_right_triple -> 'a2 list -> 'a2 lr_triple_seq_graph -> 'a1

val lr_triple_seq_graph_correct :
  nat -> nkind -> color -> 'a1 left_right_triple -> 'a1 lr_triple_seq_graph

val lr_triple_seq_elim :
  (__ -> nat -> nkind -> __ stored_triple vector -> 'a1) -> (__ -> nat ->
  nkind -> color -> __ triple -> 'a1) -> nat -> nkind -> color -> 'a2
  left_right_triple -> 'a1

val coq_FunctionalElimination_lr_triple_seq :
  (__ -> nat -> nkind -> __ stored_triple vector -> __) -> (__ -> nat ->
  nkind -> color -> __ triple -> __) -> nat -> nkind -> color -> __
  left_right_triple -> __

val coq_FunctionalInduction_lr_triple_seq :
  (__ -> nat -> nkind -> color -> __ left_right_triple -> __ list)
  coq_FunctionalInduction

val six_stored_triple_seq : nat -> 'a1 six_stored_triple -> 'a1 list

type 'a six_stored_triple_seq_graph =
| Coq_six_stored_triple_seq_graph_equation_1 of nat * 'a stored_triple
   * 'a stored_triple * 'a stored_triple * 'a stored_triple
   * 'a stored_triple * 'a stored_triple

val six_stored_triple_seq_graph_rect :
  (__ -> nat -> __ stored_triple -> __ stored_triple -> __ stored_triple ->
  __ stored_triple -> __ stored_triple -> __ stored_triple -> 'a1) -> nat ->
  'a2 six_stored_triple -> 'a2 list -> 'a2 six_stored_triple_seq_graph -> 'a1

val six_stored_triple_seq_graph_correct :
  nat -> 'a1 six_stored_triple -> 'a1 six_stored_triple_seq_graph

val six_stored_triple_seq_elim :
  (__ -> nat -> __ stored_triple -> __ stored_triple -> __ stored_triple ->
  __ stored_triple -> __ stored_triple -> __ stored_triple -> 'a1) -> nat ->
  'a2 six_stored_triple -> 'a1

val coq_FunctionalElimination_six_stored_triple_seq :
  (__ -> nat -> __ stored_triple -> __ stored_triple -> __ stored_triple ->
  __ stored_triple -> __ stored_triple -> __ stored_triple -> __) -> nat ->
  __ six_stored_triple -> __

val coq_FunctionalInduction_six_stored_triple_seq :
  (__ -> nat -> __ six_stored_triple -> __ list) coq_FunctionalInduction

val pt_triple_seq : nat -> nat -> nkind -> 'a1 partial_triple -> 'a1 list

type 'a pt_triple_seq_graph =
| Coq_pt_triple_seq_graph_equation_1 of nat * nkind
| Coq_pt_triple_seq_graph_equation_2 of nat * nkind * 'a six_stored_triple
| Coq_pt_triple_seq_graph_equation_3 of nat * nat * nkind * color * 'a triple

val pt_triple_seq_graph_rect :
  (__ -> nat -> nkind -> 'a1) -> (__ -> nat -> nkind -> __ six_stored_triple
  -> 'a1) -> (__ -> nat -> nat -> nkind -> color -> __ triple -> 'a1) -> nat
  -> nat -> nkind -> 'a2 partial_triple -> 'a2 list -> 'a2
  pt_triple_seq_graph -> 'a1

val pt_triple_seq_graph_correct :
  nat -> nat -> nkind -> 'a1 partial_triple -> 'a1 pt_triple_seq_graph

val pt_triple_seq_elim :
  (__ -> nat -> nkind -> 'a1) -> (__ -> nat -> nkind -> __ six_stored_triple
  -> 'a1) -> (__ -> nat -> nat -> nkind -> color -> __ triple -> 'a1) -> nat
  -> nat -> nkind -> 'a2 partial_triple -> 'a1

val coq_FunctionalElimination_pt_triple_seq :
  (__ -> nat -> nkind -> __) -> (__ -> nat -> nkind -> __ six_stored_triple
  -> __) -> (__ -> nat -> nat -> nkind -> color -> __ triple -> __) -> nat ->
  nat -> nkind -> __ partial_triple -> __

val coq_FunctionalInduction_pt_triple_seq :
  (__ -> nat -> nat -> nkind -> __ partial_triple -> __ list)
  coq_FunctionalInduction

val sandwich_seq :
  ('a1 -> 'a3 list) -> ('a2 -> 'a3 list) -> ('a1, 'a2) sandwich -> 'a3 list

type ('a, 'b, 'c) sandwich_seq_graph =
| Coq_sandwich_seq_graph_equation_1 of ('a -> 'c list) * ('b -> 'c list) * 'a
| Coq_sandwich_seq_graph_equation_2 of ('a -> 'c list) * ('b -> 'c list) *
   'a * 'b * 'a

val sandwich_seq_graph_rect :
  (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> 'a1) -> (__
  -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __ -> __ -> 'a1)
  -> ('a2 -> 'a4 list) -> ('a3 -> 'a4 list) -> ('a2, 'a3) sandwich -> 'a4
  list -> ('a2, 'a3, 'a4) sandwich_seq_graph -> 'a1

val sandwich_seq_graph_correct :
  ('a1 -> 'a3 list) -> ('a2 -> 'a3 list) -> ('a1, 'a2) sandwich -> ('a1, 'a2,
  'a3) sandwich_seq_graph

val sandwich_seq_elim :
  (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> 'a1) -> (__
  -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __ -> __ -> 'a1)
  -> ('a2 -> 'a4 list) -> ('a3 -> 'a4 list) -> ('a2, 'a3) sandwich -> 'a1

val coq_FunctionalElimination_sandwich_seq :
  (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __) -> (__
  -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __ -> __ -> __)
  -> (__ -> __ list) -> (__ -> __ list) -> (__, __) sandwich -> __

val coq_FunctionalInduction_sandwich_seq :
  (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> (__, __) sandwich
  -> __ list) coq_FunctionalInduction

val semi_cadeque_seq : nat -> 'a1 semi_cadeque -> 'a1 list

type 'a semi_cadeque_seq_graph =
| Coq_semi_cadeque_seq_graph_equation_1 of nat * nat * color * color
   * 'a chain

val semi_cadeque_seq_graph_rect :
  (__ -> nat -> nat -> color -> color -> __ chain -> 'a1) -> nat -> 'a2
  semi_cadeque -> 'a2 list -> 'a2 semi_cadeque_seq_graph -> 'a1

val semi_cadeque_seq_graph_correct :
  nat -> 'a1 semi_cadeque -> 'a1 semi_cadeque_seq_graph

val semi_cadeque_seq_elim :
  (__ -> nat -> nat -> color -> color -> __ chain -> 'a1) -> nat -> 'a2
  semi_cadeque -> 'a1

val coq_FunctionalElimination_semi_cadeque_seq :
  (__ -> nat -> nat -> color -> color -> __ chain -> __) -> nat -> __
  semi_cadeque -> __

val coq_FunctionalInduction_semi_cadeque_seq :
  (__ -> nat -> __ semi_cadeque -> __ list) coq_FunctionalInduction

val cadeque_seq : 'a1 cadeque -> 'a1 list

type 'a cadeque_seq_graph =
| Coq_cadeque_seq_graph_equation_1 of nat * 'a chain

val cadeque_seq_graph_rect :
  (__ -> nat -> __ chain -> 'a1) -> 'a2 cadeque -> 'a2 list -> 'a2
  cadeque_seq_graph -> 'a1

val cadeque_seq_graph_correct : 'a1 cadeque -> 'a1 cadeque_seq_graph

val cadeque_seq_elim : (__ -> nat -> __ chain -> 'a1) -> 'a2 cadeque -> 'a1

val coq_FunctionalElimination_cadeque_seq :
  (__ -> nat -> __ chain -> __) -> __ cadeque -> __

val coq_FunctionalInduction_cadeque_seq :
  (__ -> __ cadeque -> __ list) coq_FunctionalInduction

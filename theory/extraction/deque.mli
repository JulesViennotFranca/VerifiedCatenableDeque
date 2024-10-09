open Datatypes
open GYR
(* # mod # -> commented open Nat *)

type __ = Obj.t

type 'a prodN =
| Coq_prodZ of 'a
| Coq_prodS of nat * 'a prodN * 'a prodN

type 'a optionN =
| NoneN
| SomeN of 'a prodN

type 'a buffer =
| B0
| B1 of yellow_hue * red_hue * 'a prodN
| B2 of green_hue * yellow_hue * red_hue * 'a prodN * 'a prodN
| B3 of green_hue * yellow_hue * red_hue * 'a prodN * 'a prodN * 'a prodN
| B4 of yellow_hue * red_hue * 'a prodN * 'a prodN * 'a prodN * 'a prodN
| B5 of 'a prodN * 'a prodN * 'a prodN * 'a prodN * 'a prodN

type 'a packet =
| Hole of nat
| Packet of nat * nat * nat * nat * nat * color * yellow_hue * 'a buffer
   * 'a packet * 'a buffer

type regularity =
| G of green_hue * red_hue
| Y
| R

type 'a chain =
| Ending of nat * color * 'a buffer
| Chain of nat * nat * nat * color * color * regularity * 'a packet * 'a chain

type 'a decompose =
| Underflow of nat * 'a optionN
| Ok of nat * 'a buffer
| Overflow of nat * 'a buffer * 'a prodN

type 'a sandwich =
| Alone of nat * 'a optionN
| Sandwich of nat * color * 'a prodN * 'a buffer * 'a prodN

type 'a deque =
| T of green_hue * yellow_hue * 'a chain

val concat_map_prodN_seq :
  (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> 'a1 prodN -> 'a2 list

val concat_map_buffer_seq :
  (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nat -> color -> 'a1 buffer
  -> 'a2 list

val concat_map_packet_seq :
  (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nat -> nat -> nat -> color
  -> 'a1 packet -> 'a2 list -> 'a2 list

val concat_map_chain_seq :
  (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nat -> color -> 'a1 chain ->
  'a2 list

val concat_map_deque_seq :
  (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> 'a1 deque -> 'a2 list

val empty : 'a1 deque

val green_push : nat -> nat -> 'a1 prodN -> 'a1 buffer -> 'a1 buffer

val green_inject : nat -> nat -> 'a1 buffer -> 'a1 prodN -> 'a1 buffer

val green_pop : nat -> nat -> 'a1 buffer -> ('a1 prodN, 'a1 buffer) prod

val green_eject : nat -> nat -> 'a1 buffer -> ('a1 buffer, 'a1 prodN) prod

val yellow_push : nat -> nat -> 'a1 prodN -> 'a1 buffer -> 'a1 buffer

val yellow_inject : nat -> nat -> 'a1 buffer -> 'a1 prodN -> 'a1 buffer

val yellow_pop : nat -> nat -> 'a1 buffer -> ('a1 prodN, 'a1 buffer) prod

val yellow_eject : nat -> nat -> 'a1 buffer -> ('a1 buffer, 'a1 prodN) prod

val buffer_push_obligations_obligation_1 : yellow_hue

val buffer_push_obligations_obligation_2 : red_hue

val buffer_push_obligations_obligation_4 : green_hue

val buffer_push_obligations_obligation_5 : yellow_hue -> yellow_hue

val buffer_push_obligations_obligation_6 : red_hue -> red_hue

val buffer_push_obligations_obligation_8 : green_hue -> green_hue

val buffer_push_obligations_obligation_9 : yellow_hue -> yellow_hue

val buffer_push_obligations_obligation_10 : red_hue -> red_hue

val buffer_push_obligations_obligation_12 : yellow_hue -> yellow_hue

val buffer_push_obligations_obligation_13 : red_hue -> red_hue

val buffer_push : nat -> nat -> color -> 'a1 prodN -> 'a1 buffer -> 'a1 chain

val buffer_inject_obligations_obligation_1 : yellow_hue

val buffer_inject_obligations_obligation_2 : red_hue

val buffer_inject_obligations_obligation_4 : green_hue

val buffer_inject_obligations_obligation_5 : yellow_hue -> yellow_hue

val buffer_inject_obligations_obligation_6 : red_hue -> red_hue

val buffer_inject_obligations_obligation_8 : green_hue -> green_hue

val buffer_inject_obligations_obligation_9 : yellow_hue -> yellow_hue

val buffer_inject_obligations_obligation_10 : red_hue -> red_hue

val buffer_inject_obligations_obligation_12 : yellow_hue -> yellow_hue

val buffer_inject_obligations_obligation_13 : red_hue -> red_hue

val buffer_inject :
  nat -> nat -> color -> 'a1 buffer -> 'a1 prodN -> 'a1 chain

val buffer_pop :
  nat -> nat -> color -> 'a1 buffer -> ('a1 prodN, 'a1 buffer) prod option

val buffer_eject :
  nat -> nat -> color -> 'a1 buffer -> ('a1 buffer, 'a1 prodN) prod option

val prefix_rot :
  nat -> nat -> color -> 'a1 prodN -> 'a1 buffer -> ('a1 buffer, 'a1 prodN)
  prod

val suffix_rot :
  nat -> nat -> color -> 'a1 buffer -> 'a1 prodN -> ('a1 prodN, 'a1 buffer)
  prod

val prefix23 : nat -> nat -> 'a1 optionN -> 'a1 prodN -> 'a1 buffer

val suffix23 : nat -> nat -> 'a1 prodN -> 'a1 optionN -> 'a1 buffer

val suffix12 : nat -> nat -> 'a1 prodN -> 'a1 optionN -> 'a1 buffer

val prefix_decompose : nat -> nat -> color -> 'a1 buffer -> 'a1 decompose

val suffix_decompose : nat -> nat -> color -> 'a1 buffer -> 'a1 decompose

val buffer_unsandwich_obligations_obligation_4 : yellow_hue -> yellow_hue

val buffer_unsandwich_obligations_obligation_5 : red_hue -> red_hue

val buffer_unsandwich_obligations_obligation_7 : green_hue

val buffer_unsandwich_obligations_obligation_8 : yellow_hue -> yellow_hue

val buffer_unsandwich_obligations_obligation_9 : red_hue -> red_hue

val buffer_unsandwich_obligations_obligation_11 : green_hue

val buffer_unsandwich_obligations_obligation_12 : yellow_hue

val buffer_unsandwich_obligations_obligation_13 : red_hue

val buffer_unsandwich : nat -> nat -> color -> 'a1 buffer -> 'a1 sandwich

val buffer_halve :
  nat -> nat -> color -> 'a1 buffer -> ('a1 optionN, 'a1 buffer) prod

val to_yellow :
  nat -> nat -> green_hue -> yellow_hue -> 'a1 buffer -> 'a1 buffer

val to_red : nat -> nat -> color -> 'a1 buffer -> 'a1 buffer

val green_prefix_concat :
  nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer, 'a1
  buffer) prod

val green_suffix_concat :
  nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer, 'a1
  buffer) prod

val yellow_prefix_concat :
  nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer, 'a1
  buffer) prod

val yellow_suffix_concat :
  nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer, 'a1
  buffer) prod

val green_opt_push_obligations_obligation_2 : green_hue

val green_opt_push_obligations_obligation_3 : yellow_hue

val green_opt_push_obligations_obligation_4 : red_hue

val green_opt_push_obligations_obligation_6 : yellow_hue

val green_opt_push_obligations_obligation_7 : red_hue

val green_opt_push :
  nat -> nat -> nat -> 'a1 optionN -> 'a1 buffer -> 'a1 chain

val green_opt_inject_obligations_obligation_2 : green_hue

val green_opt_inject_obligations_obligation_3 : yellow_hue

val green_opt_inject_obligations_obligation_4 : red_hue

val green_opt_inject_obligations_obligation_6 : yellow_hue

val green_opt_inject_obligations_obligation_7 : red_hue

val green_opt_inject :
  nat -> nat -> nat -> 'a1 buffer -> 'a1 optionN -> 'a1 chain

val chain_of_opt3_obligations_obligation_2 : yellow_hue

val chain_of_opt3_obligations_obligation_3 : red_hue

val chain_of_opt3_obligations_obligation_5 : green_hue

val chain_of_opt3_obligations_obligation_6 : yellow_hue

val chain_of_opt3_obligations_obligation_7 : red_hue

val chain_of_opt3_obligations_obligation_9 : green_hue

val chain_of_opt3_obligations_obligation_10 : yellow_hue

val chain_of_opt3_obligations_obligation_11 : red_hue

val chain_of_opt3_obligations_obligation_13 : yellow_hue

val chain_of_opt3_obligations_obligation_14 : red_hue

val chain_of_opt3_obligations_obligation_16 : green_hue

val chain_of_opt3_obligations_obligation_17 : yellow_hue

val chain_of_opt3_obligations_obligation_18 : red_hue

val chain_of_opt3_obligations_obligation_20 : green_hue

val chain_of_opt3_obligations_obligation_21 : yellow_hue

val chain_of_opt3_obligations_obligation_22 : red_hue

val chain_of_opt3_obligations_obligation_24 : yellow_hue

val chain_of_opt3_obligations_obligation_25 : red_hue

val chain_of_opt3 :
  nat -> nat -> nat -> nat -> 'a1 optionN -> 'a1 optionN -> 'a1 optionN ->
  'a1 chain

val translate : nat -> nat -> nat -> color -> 'a1 chain -> 'a1 chain

val make_small :
  nat -> nat -> nat -> nat -> color -> color -> color -> 'a1 buffer -> 'a1
  buffer -> 'a1 buffer -> 'a1 chain

val green_of_red : nat -> nat -> 'a1 chain -> 'a1 chain

val ensure_green :
  nat -> nat -> green_hue -> red_hue -> 'a1 chain -> 'a1 chain

val make_yellow :
  nat -> nat -> nat -> nat -> nat -> nat -> green_hue -> yellow_hue ->
  yellow_hue -> green_hue -> yellow_hue -> green_hue -> red_hue -> 'a1 buffer
  -> 'a1 packet -> 'a1 buffer -> 'a1 chain -> 'a1 deque

val make_red :
  nat -> nat -> nat -> nat -> nat -> nat -> color -> yellow_hue -> color ->
  'a1 buffer -> 'a1 packet -> 'a1 buffer -> 'a1 chain -> 'a1 deque

val push : nat -> 'a1 -> 'a1 deque -> 'a1 deque

val inject : nat -> 'a1 deque -> 'a1 -> 'a1 deque

val option_pop : nat -> 'a1 deque -> ('a1, 'a1 deque) prod option

val pop_obligations_obligation_2 : nat -> 'a1 deque -> ('a1, 'a1 deque) prod

val pop : nat -> 'a1 deque -> ('a1, 'a1 deque) prod

val option_eject : nat -> 'a1 deque -> ('a1 deque, 'a1) prod option

val eject_obligations_obligation_2 : nat -> 'a1 deque -> ('a1 deque, 'a1) prod

val eject : nat -> 'a1 deque -> ('a1 deque, 'a1) prod

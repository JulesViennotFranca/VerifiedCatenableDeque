open Datatypes
(* # mod # -> commented open EqDec *)
open GYOR
(* # mod # -> commented open Nat *)
(* # mod # -> commented open Buffer *)
(* # mod # -> commented open Models *)
open Types

val push_left_node_obligations_obligation_1 : nat -> nat

val push_left_node_obligations_obligation_3 : nat -> nat

val push_left_node_obligations_obligation_5 : nat -> nat

val push_left_node_obligations_obligation_7 : nat -> nat

val push_left_node_obligations_obligation_9 : nat -> nat

val push_left_node :
  nat -> nat -> color -> 'a1 stored_triple -> 'a1 node -> 'a1 node

val inject_right_node_obligations_obligation_1 : nat -> nat

val inject_right_node_obligations_obligation_3 : nat -> nat

val inject_right_node_obligations_obligation_5 : nat -> nat

val inject_right_node_obligations_obligation_7 : nat -> nat

val inject_right_node_obligations_obligation_9 : nat -> nat

val inject_right_node :
  nat -> nat -> color -> 'a1 node -> 'a1 stored_triple -> 'a1 node

val push_only_node :
  nat -> nat -> color -> 'a1 stored_triple -> 'a1 node -> 'a1 node

val inject_only_node :
  nat -> nat -> color -> 'a1 node -> 'a1 stored_triple -> 'a1 node

val push_left_packet :
  nat -> nat -> nat -> color -> 'a1 stored_triple -> 'a1 packet -> 'a1 packet

val inject_right_packet :
  nat -> nat -> nat -> color -> 'a1 packet -> 'a1 stored_triple -> 'a1 packet

val push_only_packet :
  nat -> nat -> nat -> color -> 'a1 stored_triple -> 'a1 packet -> 'a1 packet

val inject_only_packet :
  nat -> nat -> nat -> color -> 'a1 packet -> 'a1 stored_triple -> 'a1 packet

val single_node : nat -> 'a1 stored_triple -> 'a1 node

val single_packet : nat -> 'a1 stored_triple -> 'a1 packet

val single_chain : nat -> 'a1 stored_triple -> 'a1 chain

val push_left_chain :
  nat -> color -> 'a1 stored_triple -> 'a1 chain -> 'a1 chain

val inject_right_chain :
  nat -> color -> 'a1 chain -> 'a1 stored_triple -> 'a1 chain

val push_ne_chain :
  nat -> nat -> color -> color -> 'a1 stored_triple -> 'a1 chain -> 'a1 chain

val inject_ne_chain :
  nat -> nat -> color -> color -> 'a1 chain -> 'a1 stored_triple -> 'a1 chain

val semi_push :
  nat -> 'a1 stored_triple -> 'a1 semi_cadeque -> 'a1 semi_cadeque

val semi_inject :
  nat -> 'a1 semi_cadeque -> 'a1 stored_triple -> 'a1 semi_cadeque

val to_reg :
  nat -> nkind -> yellow_hue -> orange_hue -> color -> 'a1 node -> regularity

val triple_of_chain : nat -> nkind -> color -> 'a1 chain -> 'a1 triple

val chain_of_triple : nat -> nkind -> color -> 'a1 triple -> 'a1 chain

val left_of_only_obligations_obligation_2 : nat -> nat

val left_of_only : nat -> color -> 'a1 triple -> 'a1 left_right_triple

val right_of_only_obligations_obligation_2 : nat -> nat

val right_of_only : nat -> color -> 'a1 triple -> 'a1 left_right_triple

val make_stored_suffix :
  nat -> nat -> nat -> nat -> color -> color -> 'a1 suffix -> 'a1 prefix ->
  'a1 chain -> 'a1 suffix -> ('a1 stored_triple, 'a1 suffix) prod

val make_prefix_stored :
  nat -> nat -> nat -> nat -> color -> color -> 'a1 prefix -> 'a1 chain ->
  'a1 suffix -> 'a1 prefix -> ('a1 prefix, 'a1 stored_triple) prod

val stored_of_right :
  nat -> nat -> color -> 'a1 suffix -> 'a1 triple -> ('a1 stored_triple, 'a1
  suffix) prod

val stored_of_left :
  nat -> nat -> color -> 'a1 triple -> 'a1 prefix -> ('a1 prefix, 'a1
  stored_triple) prod

val left_of_pair_obligations_obligation_2 : nat -> nat

val left_of_pair :
  nat -> color -> color -> 'a1 triple -> 'a1 triple -> 'a1 triple

val right_of_pair_obligations_obligation_2 : nat -> nat

val right_of_pair :
  nat -> color -> color -> 'a1 triple -> 'a1 triple -> 'a1 triple

val make_left :
  nat -> nat -> color -> color -> 'a1 chain -> 'a1 left_right_triple

val make_right :
  nat -> nat -> color -> color -> 'a1 chain -> 'a1 left_right_triple

val concat_semi :
  nat -> 'a1 semi_cadeque -> 'a1 semi_cadeque -> 'a1 semi_cadeque

val orange : nat -> nat -> color -> 'a1 chain -> regularity

val pop_left_green_obligations_obligation_1 : nat -> nat

val pop_left_green_obligations_obligation_4 : nat -> nat

val pop_left_green_obligations_obligation_6 : nat -> nat

val pop_left_green_obligations_obligation_8 : nat -> nat

val pop_left_green_obligations_obligation_10 : nat -> nat

val pop_left_green :
  nat -> 'a1 triple -> ('a1 stored_triple, 'a1 partial_triple) prod

val eject_right_green_obligations_obligation_1 : nat -> nat

val eject_right_green_obligations_obligation_4 : nat -> nat

val eject_right_green_obligations_obligation_6 : nat -> nat

val eject_right_green_obligations_obligation_8 : nat -> nat

val eject_right_green_obligations_obligation_10 : nat -> nat

val eject_right_green :
  nat -> 'a1 triple -> ('a1 partial_triple, 'a1 stored_triple) prod

val pop_only_green :
  nat -> 'a1 triple -> ('a1 stored_triple, 'a1 partial_triple) prod

val eject_only_green :
  nat -> 'a1 triple -> ('a1 partial_triple, 'a1 stored_triple) prod

val sandwich_only_green :
  nat -> 'a1 triple -> ('a1 stored_triple, 'a1 partial_triple) sandwich

val adapt_to_prefix :
  nat -> nat -> nat -> nat -> color -> coloring -> coloring

val only_of_right :
  nat -> color -> 'a1 six_stored_triple -> 'a1 triple -> 'a1 triple

val adapt_to_suffix :
  nat -> nat -> nat -> nat -> color -> coloring -> coloring

val only_of_left :
  nat -> color -> 'a1 triple -> 'a1 six_stored_triple -> 'a1 triple

val pop_pair_green :
  nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) prod

val eject_pair_green :
  nat -> 'a1 chain -> ('a1 semi_cadeque, 'a1 stored_triple) prod

val sandwich_pair_green :
  nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) sandwich

val pop_green :
  nat -> nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) prod

val eject_green :
  nat -> nat -> 'a1 chain -> ('a1 semi_cadeque, 'a1 stored_triple) prod

val sandwich_green :
  nat -> nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) sandwich

val make_green_prefix :
  nat -> nat -> nat -> 'a1 prefix -> 'a1 prefix -> 'a1 semi_cadeque -> ('a1
  green_buffer, 'a1 semi_cadeque) prod

val make_green_suffix :
  nat -> nat -> nat -> 'a1 semi_cadeque -> 'a1 suffix -> 'a1 suffix -> ('a1
  semi_cadeque, 'a1 green_buffer) prod

val extract_prefix :
  nat -> 'a1 stored_triple -> 'a1 semi_cadeque -> ('a1 stored_buffer, 'a1
  semi_cadeque) prod

val extract_suffix :
  nat -> 'a1 semi_cadeque -> 'a1 stored_triple -> ('a1 semi_cadeque, 'a1
  stored_buffer) prod

val ensure_green_prefix :
  nat -> nat -> nat -> 'a1 prefix -> 'a1 chain -> ('a1 green_buffer, 'a1
  semi_cadeque) prod

val ensure_green_suffix :
  nat -> nat -> nat -> 'a1 chain -> 'a1 suffix -> ('a1 semi_cadeque, 'a1
  green_buffer) prod

val green_of_red_left_obligations_obligation_1 : nat -> nat

val green_of_red_left_obligations_obligation_3 : nat -> nat

val green_of_red_left_obligations_obligation_5 : nat -> nat

val green_of_red_left :
  nat -> nat -> nkind -> nat -> 'a1 body -> 'a1 node -> 'a1 chain -> 'a1 chain

val green_of_red_right_obligations_obligation_1 : nat -> nat

val green_of_red_right_obligations_obligation_3 : nat -> nat

val green_of_red_right_obligations_obligation_5 : nat -> nat

val green_of_red_right :
  nat -> nat -> nkind -> nat -> 'a1 body -> 'a1 node -> 'a1 chain -> 'a1 chain

val make_green_only :
  nat -> nat -> nkind -> nat -> nat -> 'a1 body -> 'a1 prefix -> 'a1
  semi_cadeque -> 'a1 prefix -> 'a1 chain

val green_of_red_only :
  nat -> nat -> nkind -> nat -> 'a1 body -> 'a1 node -> 'a1 chain -> 'a1 chain

val ensure_green_obligations_obligation_4 : nat -> nat

val ensure_green_obligations_obligation_6 : nat -> nat

val ensure_green :
  nat -> nat -> nkind -> color -> color -> 'a1 chain -> 'a1 chain

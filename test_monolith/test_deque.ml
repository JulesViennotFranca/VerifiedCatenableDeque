open Deques
open Monolith
open Test_support.Specs

(* Our reference implementation: lists *)
module R = Test_support.Reference

(* We use small integers for values in the sequence *)

let value = lt 16

(* Our main abstract type: deques of values *)

let deque =
  declare_abstract_type ()

(* Print the following prelude when showing a failing test scenario. *)
let () = dprintf "\
          open Deques
"

(* Declare the operations. *)

let () =
  Monolith.override_exn_eq (fun eq exn1 exn2 ->
    match exn1, exn2 with
    | Failure _, Failure _
    | Invalid_argument _, Invalid_argument _ -> true
    | _, _ -> eq exn1 exn2
  )

let () =
  declare "Deque.push" (value ^> deque ^> deque) R.push Deque.push;
  declare "Deque.inject" (deque ^> value ^> deque) R.inject Deque.inject;
  declare "Deque.pop" (deque ^> option (value *** deque)) R.pop Deque.pop;
  declare "Deque.eject" (deque ^> option (deque *** value)) R.eject Deque.eject;
  declare "Deque.length" (deque ^> int) R.length Deque.length;

  declare "Deque.hd" (deque ^!> value) R.hd Deque.hd;
  declare "Deque.dh" (deque ^!> value) R.dh Deque.dh;
  declare "Deque.tl" (deque ^!> deque) R.tl Deque.tl;
  declare "Deque.lt" (deque ^!> deque) R.lt Deque.lt;
  declare "Deque.nth" (deque ^>> fun l -> list_index l ^!> value) R.nth Deque.nth;
  declare "Deque.nth_opt" (deque ^>> fun l -> list_index l ^!> option value) R.nth_opt Deque.nth_opt;

  declare "Deque.is_empty" (deque ^> bool) R.is_empty Deque.is_empty;
  declare "Deque.empty" deque R.empty Deque.empty;
  declare "Deque.singleton" (value ^> deque) R.singleton Deque.singleton;

  declare "Deque.make" (list_size ^> value ^!> deque) R.make Deque.make;
  declare "Deque.init" (init (list value ^> deque)) R.init Deque.init;
  declare "Deque.rev" (deque ^> deque) R.rev Deque.rev;

  declare "Deque.(=)" (deque ^> deque ^> bool) R.(=) Deque.(=);
  (* TODO: equal, compare *)

  declare "Deque.append" (deque ^> deque ^> deque) R.append Deque.append;
  declare "Deque.rev_append" (deque ^> deque ^> deque) R.rev_append Deque.rev_append;
  (* TODO: concat *)

  declare "Deque.iter" (iter (deque ^> list value)) R.iter Deque.iter;
  (* TODO: iteri, map, mapi, rev_map, filter_map, concat_map, fold_left_map, fold_right_map *)
  declare "Deque.fold_left" (foldl (deque ^> list value)) R.fold_left Deque.fold_left;
  declare "Deque.fold_right" (foldr (deque ^> list value)) R.fold_right Deque.fold_right;
  (* TODO: iter2, map2, rev_map2, fold_left2, fold_right2 *)
  (* TODO: fold_all, exists, for_all2, exists2 *)

  declare "Deque.mem" (value ^> deque ^> bool) R.mem Deque.mem;
  declare "Deque.memq" (value ^> deque ^> bool) R.memq Deque.memq;
  (* TODO: find, find_opt, find_map, filter, find_all, filteri, partition *)
  (* TODO: assoc, assoc_opt, assq, assq_opt, mem_assoc, mem_assq *)
  (* TODO: split, combine *)

  declare "Deque.to_array" (deque ^> naive_array value) R.to_array Deque.to_array;
  declare "Deque.of_array" (naive_array value ^> deque) R.of_array Deque.of_array;
  declare "Deque.to_list" (deque ^> list value) R.to_list Deque.to_list;
  declare "Deque.of_list" (list value ^> deque) R.of_list Deque.of_list;
  (* TODO: conversions from/to Seq.t *)
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

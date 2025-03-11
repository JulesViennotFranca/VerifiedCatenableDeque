open Deques
open Monolith
open Test_support.Specs

(* Our reference implementation: lists *)
module R = Test_support.Reference

(* We draw small integers as values in the sequence *)

let value = lt 16

(* We have one abstract type: steques *)

let steque =
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
  declare "Steque.push" (value ^> steque ^> steque) R.push Steque.push;
  declare "Steque.inject" (steque ^> value ^> steque) R.inject Steque.inject;
  declare "Steque.pop" (steque ^> option (value *** steque)) R.pop Steque.pop;
  declare "Steque.length" (steque ^> int) R.length Steque.length;

  declare "Steque.pop1" (steque ^!> value) R.pop1 Steque.pop1;
  declare "Steque.pop2" (steque ^!> steque) R.pop2 Steque.pop2;
  declare "Steque.nth" (steque ^>> fun l -> list_index l ^!> value) R.nth Steque.nth;
  declare "Steque.nth_opt" (steque ^>> fun l -> list_index l ^!> option value) R.nth_opt Steque.nth_opt;

  declare "Steque.is_empty" (steque ^> bool) R.is_empty Steque.is_empty;
  declare "Steque.empty" steque R.empty Steque.empty;
  declare "Steque.singleton" (value ^> steque) R.singleton Steque.singleton;

  declare "Steque.make" (list_size ^> value ^!> steque) R.make Steque.make;
  declare "Steque.init" (init (list value ^> steque)) R.init Steque.init;
  declare "Steque.rev" (steque ^> steque) R.rev Steque.rev;

  declare "Steque.(=)" (steque ^> steque ^> bool) R.(=) Steque.(=);
  (* TODO: equal, compare *)

  declare "Steque.append" (steque ^> steque ^> steque) R.append Steque.append;
  declare "Steque.rev_append" (steque ^> steque ^> steque) R.rev_append Steque.rev_append;
  (* TODO: concat *)

  declare "Steque.iter" (iter (steque ^> list value)) R.iter Steque.iter;
  (* TODO: iteri, map, mapi, rev_map, filter_map, concat_map, fold_left_map, fold_right_map *)
  declare "Steque.fold_left" (foldl (steque ^> list value)) R.fold_left Steque.fold_left;
  declare "Steque.fold_right" (foldr (steque ^> list value)) R.fold_right Steque.fold_right;
  (* TODO: iter2, map2, rev_map2, fold_left2, fold_right2 *)
  (* TODO: fold_all, exists, for_all2, exists2 *)

  declare "Steque.mem" (value ^> steque ^> bool) R.mem Steque.mem;
  declare "Steque.memq" (value ^> steque ^> bool) R.memq Steque.memq;
  (* TODO: find, find_opt, find_map, filter, find_all, filteri, partition *)
  (* TODO: assoc, assoc_opt, assq, assq_opt, mem_assoc, mem_assq *)
  (* TODO: split, combine *)

  declare "Steque.to_array" (steque ^> naive_array value) R.to_array Steque.to_array;
  declare "Steque.of_array" (naive_array value ^> steque) R.of_array Steque.of_array;
  declare "Steque.to_list" (steque ^> list value) R.to_list Steque.to_list;
  declare "Steque.of_list" (list value ^> steque) R.of_list Steque.of_list;
  (* TODO: conversions from/to Seq.t *)
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

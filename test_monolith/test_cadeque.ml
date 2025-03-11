open Deques
open Monolith
open Test_support.Specs

(* Our reference implementation: lists *)
module R = Test_support.Reference

(* We draw small integers as values in the sequence *)

let value = lt 16

(* We have one abstract type: cadeques *)

let cadeque =
  declare_abstract_type ()

(* Print the following prelude when showing a failing test scenario. *)
let () = dprintf "\
          open Cadeques
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
  declare "Cadeque.push" (value ^> cadeque ^> cadeque) R.push Cadeque.push;
  declare "Cadeque.inject" (cadeque ^> value ^> cadeque) R.inject Cadeque.inject;
  declare "Cadeque.pop" (cadeque ^> option (value *** cadeque)) R.pop Cadeque.pop;
  declare "Cadeque.eject" (cadeque ^> option (cadeque *** value)) R.eject Cadeque.eject;
  declare "Cadeque.length" (cadeque ^> int) R.length Cadeque.length;

  declare "Cadeque.pop1" (cadeque ^!> value) R.pop1 Cadeque.pop1;
  declare "Cadeque.pop2" (cadeque ^!> cadeque) R.pop2 Cadeque.pop2;
  declare "Cadeque.eject1" (cadeque ^!> cadeque) R.eject1 Cadeque.eject1;
  declare "Cadeque.eject2" (cadeque ^!> value) R.eject2 Cadeque.eject2;
  declare "Cadeque.nth" (cadeque ^>> fun l -> list_index l ^!> value) R.nth Cadeque.nth;
  declare "Cadeque.nth_opt" (cadeque ^>> fun l -> list_index l ^!> option value) R.nth_opt Cadeque.nth_opt;

  declare "Cadeque.is_empty" (cadeque ^> bool) R.is_empty Cadeque.is_empty;
  declare "Cadeque.empty" cadeque R.empty Cadeque.empty;
  declare "Cadeque.singleton" (value ^> cadeque) R.singleton Cadeque.singleton;

  declare "Cadeque.make" (list_size ^> value ^!> cadeque) R.make Cadeque.make;
  declare "Cadeque.init" (init (list value ^> cadeque)) R.init Cadeque.init;
  declare "Cadeque.rev" (cadeque ^> cadeque) R.rev Cadeque.rev;

  declare "Cadeque.(=)" (cadeque ^> cadeque ^> bool) R.(=) Cadeque.(=);
  (* TODO: equal, compare *)

  declare "Cadeque.append" (cadeque ^> cadeque ^> cadeque) R.append Cadeque.append;
  declare "Cadeque.rev_append" (cadeque ^> cadeque ^> cadeque) R.rev_append Cadeque.rev_append;
  (* TODO: concat *)

  declare "Cadeque.iter" (iter (cadeque ^> list value)) R.iter Cadeque.iter;
  (* TODO: iteri, map, mapi, rev_map, filter_map, concat_map, fold_left_map, fold_right_map *)
  declare "Cadeque.fold_left" (foldl (cadeque ^> list value)) R.fold_left Cadeque.fold_left;
  declare "Cadeque.fold_right" (foldr (cadeque ^> list value)) R.fold_right Cadeque.fold_right;
  (* TODO: iter2, map2, rev_map2, fold_left2, fold_right2 *)
  (* TODO: fold_all, exists, for_all2, exists2 *)

  declare "Cadeque.mem" (value ^> cadeque ^> bool) R.mem Cadeque.mem;
  declare "Cadeque.memq" (value ^> cadeque ^> bool) R.memq Cadeque.memq;
  (* TODO: find, find_opt, find_map, filter, find_all, filteri, partition *)
  (* TODO: assoc, assoc_opt, assq, assq_opt, mem_assoc, mem_assq *)
  (* TODO: split, combine *)

  declare "Cadeque.to_array" (cadeque ^> naive_array value) R.to_array Cadeque.to_array;
  declare "Cadeque.of_array" (naive_array value ^> cadeque) R.of_array Cadeque.of_array;
  declare "Cadeque.to_list" (cadeque ^> list value) R.to_list Cadeque.to_list;
  declare "Cadeque.of_list" (list value ^> cadeque) R.of_list Cadeque.of_list;
  (* TODO: conversions from/to Seq.t *)
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

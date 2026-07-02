open Monolith
open Test_support.Specs

(* Our reference implementation: lists. *)
module R = Test_support.Reference

(* Our candidate implementation: our OCaml catenables deques.  *)
module C = Deques.Cadeque

(* We draw small integers as values in the sequence *)

let value = lt 16

(* We have one abstract type: cadeques *)

let cadeque =
  declare_abstract_type ()

(* Print the following prelude when showing a failing test scenario. *)
let () = dprintf "\
          open Deques.Cadeque
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
  declare "push" (value ^> cadeque ^> cadeque) R.push C.push;
  declare "inject" (cadeque ^> value ^> cadeque) R.inject C.inject;
  declare "pop" (cadeque ^> option (value *** cadeque)) R.pop C.pop;
  declare "eject" (cadeque ^> option (cadeque *** value)) R.eject C.eject;
  declare "length" (cadeque ^> int) R.length C.length;

  declare "pop1" (cadeque ^!> value) R.pop1 C.pop1;
  declare "pop2" (cadeque ^!> cadeque) R.pop2 C.pop2;
  declare "eject1" (cadeque ^!> cadeque) R.eject1 C.eject1;
  declare "eject2" (cadeque ^!> value) R.eject2 C.eject2;
  declare "nth" (cadeque ^>> fun l -> list_index l ^!> value) R.nth C.nth;
  declare "nth_opt" (cadeque ^>> fun l -> list_index l ^!> option value) R.nth_opt C.nth_opt;

  declare "is_empty" (cadeque ^> bool) R.is_empty C.is_empty;
  declare "empty" cadeque R.empty C.empty;
  declare "singleton" (value ^> cadeque) R.singleton C.singleton;

  declare "make" (list_size ^> value ^!> cadeque) R.make C.make;
  declare "init" (init (list value ^> cadeque)) R.init C.init;
  declare "rev" (cadeque ^> cadeque) R.rev C.rev;

  declare "(=)" (cadeque ^> cadeque ^> bool) R.(=) C.(=);
  (* TODO: equal, compare *)

  declare "append" (cadeque ^> cadeque ^> cadeque) R.append C.append;
  declare "rev_append" (cadeque ^> cadeque ^> cadeque) R.rev_append C.rev_append;
  (* TODO: concat *)

  declare "iter" (iter (cadeque ^> list value)) R.iter C.iter;
  (* TODO: iteri, map, mapi, rev_map, filter_map, concat_map, fold_left_map, fold_right_map *)
  declare "fold_left" (foldl (cadeque ^> list value)) R.fold_left C.fold_left;
  declare "fold_right" (foldr (cadeque ^> list value)) R.fold_right C.fold_right;
  (* TODO: iter2, map2, rev_map2, fold_left2, fold_right2 *)
  (* TODO: fold_all, exists, for_all2, exists2 *)

  declare "mem" (value ^> cadeque ^> bool) R.mem C.mem;
  declare "memq" (value ^> cadeque ^> bool) R.memq C.memq;
  (* TODO: find, find_opt, find_map, filter, find_all, filteri, partition *)
  (* TODO: assoc, assoc_opt, assq, assq_opt, mem_assoc, mem_assq *)
  (* TODO: split, combine *)

  declare "to_array" (cadeque ^> naive_array value) R.to_array C.to_array;
  declare "of_array" (naive_array value ^> cadeque) R.of_array C.of_array;
  declare "to_list" (cadeque ^> list value) R.to_list C.to_list;
  declare "of_list" (list value ^> cadeque) R.of_list C.of_list;
  (* TODO: conversions from/to Seq.t *)
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

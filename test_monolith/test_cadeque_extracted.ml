open Monolith

(* Our reference implementation: lists. *)
module R = Test_support.Reference

(* Our candidate implementation: our catenables deques,
   extracted from Rocq to OCaml. *)
module C = Deques_extraction.Cadeque

(* We draw small integers as values in the sequence *)

let value = lt 16

(* We have one abstract type: cadeques *)

let cadeque =
  declare_abstract_type ()

(* Print the following prelude when showing a failing test scenario. *)
let () = dprintf "\
          open Deques_extraction.Cadeque
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
  declare "empty" cadeque R.empty C.empty;
  declare "push" (value ^> cadeque ^> cadeque) R.push C.push;
  declare "inject" (cadeque ^> value ^> cadeque) R.inject C.inject;
  declare "pop" (cadeque ^> option (value *** cadeque)) R.pop C.pop;
  declare "eject" (cadeque ^> option (cadeque *** value)) R.eject C.eject;
  declare "concat" (cadeque ^> cadeque ^> cadeque) R.append C.concat;
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

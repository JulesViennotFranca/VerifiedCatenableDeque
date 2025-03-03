open Deques
open Monolith

(* Reference implementation: lists *)
module R = struct
  let empty = []
  let is_empty = function
    | [] -> true
    | _ -> false
  let push x l = x :: l
  let inject l x = l @ [x]
  let pop l = match l with
    | [] -> None
    | x :: xs -> Some (x, xs)
  let rec eject = function
    | [] -> None
    | [x] -> Some ([], x)
    | x :: y :: xs ->
       match eject (y :: xs) with
       | None -> assert false
       | Some (ys, z) -> Some (x :: ys, z)
  let append = (@)
end

(* We draw small integers as values in the sequence *)

let value = lt 16

(* We have one abstract type: cadeques *)

let cadeque =
  declare_abstract_type ()

(* Print the following prelude when showing a failing test scenario. *)
let () = dprintf "\
          open Deques
"

(* Declare the operations. *)

let () =
  declare "Cadeque.empty" cadeque Cadeque.empty R.empty;
  declare "Cadeque.is_empty" (cadeque ^> bool) Cadeque.is_empty R.is_empty;
  declare "Cadeque.push" (value ^> cadeque ^> cadeque) Cadeque.push R.push;
  declare "Cadeque.inject" (cadeque ^> value ^> cadeque) Cadeque.inject R.inject;
  declare "Cadeque.pop" (cadeque ^> option (value *** cadeque)) Cadeque.pop R.pop;
  declare "Cadeque.eject" (cadeque ^> option (cadeque *** value)) Cadeque.eject R.eject;
  declare "Cadeque.append" (cadeque ^> cadeque ^> cadeque) Cadeque.append R.append;
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

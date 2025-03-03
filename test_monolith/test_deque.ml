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
end

(* We draw small integers as values in the sequence *)

let value = lt 16

(* We have one abstract type: deques *)

let deque =
  declare_abstract_type ()

(* Print the following prelude when showing a failing test scenario. *)
let () = dprintf "\
          open Deques
"

(* Declare the operations. *)

let () =
  declare "Deque.empty" deque Deque.empty R.empty;
  declare "Deque.is_empty" (deque ^> bool) Deque.is_empty R.is_empty;
  declare "Deque.push" (value ^> deque ^> deque) Deque.push R.push;
  declare "Deque.inject" (deque ^> value ^> deque) Deque.inject R.inject;
  declare "Deque.pop" (deque ^> option (value *** deque)) Deque.pop R.pop;
  declare "Deque.eject" (deque ^> option (deque *** value)) Deque.eject R.eject;
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

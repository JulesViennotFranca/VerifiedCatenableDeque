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
  let append = (@)
end

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
  declare "Steque.empty" steque Steque.empty R.empty;
  declare "Steque.is_empty" (steque ^> bool) Steque.is_empty R.is_empty;
  declare "Steque.push" (value ^> steque ^> steque) Steque.push R.push;
  declare "Steque.inject" (steque ^> value ^> steque) Steque.inject R.inject;
  declare "Steque.pop" (steque ^> option (value *** steque)) Steque.pop R.pop;
  declare "Steque.append" (steque ^> steque ^> steque) Steque.append R.append;
  ()

(* Start the engine! *)

let () =
  let fuel = 128 in
  main fuel

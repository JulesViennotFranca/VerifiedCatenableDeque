(* Reference implementation: lists *)

include List
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

let pop1 = List.hd
let pop2 = List.tl

let eject1 l =
  match eject l with
  | None -> failwith "List.eject1"
  | Some (xs, _) -> xs

let eject2 l =
  match eject l with
  | None -> failwith "List.eject2"
  | Some (_, x) -> x

let singleton x = [x]

let make len x = List.init len (fun _ -> x)

let (=) l l' = List.equal (=) l l'

let to_list l = l
let of_list l = l
let to_array = Array.of_list
let of_array = Array.to_list

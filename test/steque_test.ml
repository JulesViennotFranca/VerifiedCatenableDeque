module type STEQUE = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val inject : 'a t -> 'a -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val append : 'a t -> 'a t -> 'a t
end

module Bi (A : STEQUE) (B : STEQUE) = struct

  let rec compare_prefix n (a, b) =
    if n <= 0
    then a, b
    else
    match A.pop a, B.pop b with
    | None, None -> raise Not_found
    | Some (x, a), Some (y, b) ->
        assert (x = y) ;
        compare_prefix (n - 1) (a, b)
    | _ ->
        assert false

  let make a b =
    try compare_prefix 500 (a, b)
    with Not_found -> (a, b)

  let empty = make A.empty B.empty

  let push x (a, b) =
    make (A.push x a) (B.push x b)

  let inject (a, b) x =
    make (A.inject a x) (B.inject b x)

  let pop (a, b) =
    match A.pop a, B.pop b with
    | None, None -> None
    | Some (x, a), Some (x', b) ->
        assert (x = x') ;
        Some (x, make a b)
    | _ -> assert false

  let append (a0, b0) (a1, b1) =
    make (A.append a0 a1) (B.append b0 b1)

end

module D2 = Bi (Cadeque.Steque) (Cadeque)

let counter = ref 0
let elt () = incr counter ; !counter

let some_snd t = function
  | None -> t
  | Some (_, t) -> t

let () = Random.init 9

let rec test n t =
  match Random.int 5 with
  | 0 -> n, D2.push (elt ()) t
  | 1 -> n, D2.inject t (elt ())
  | 2 -> n, some_snd t (D2.pop t)
  | 3 -> n, D2.append t t
  | 4 ->
      let fuel = Random.int n in
      let t' = test_repeatedly fuel D2.empty in
      n - fuel, D2.append t t'
  | _ -> assert false

and test_repeatedly n t =
  if n <= 0
  then t
  else let n, t = test n t in
       test_repeatedly (n - 1) t

let () =
  let s, d = test_repeatedly 1000 D2.empty in
  let xs = Cadeque.Steque.to_list s in
  let ys = Cadeque.to_list d in
  assert (xs = ys) ;
  assert (List.length xs = Cadeque.Steque.length s)

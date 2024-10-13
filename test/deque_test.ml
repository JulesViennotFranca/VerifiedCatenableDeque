module type DEQUE = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val inject : 'a t -> 'a -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val eject : 'a t -> ('a t * 'a) option
  val rev : 'a t -> 'a t
  val to_list : 'a t -> 'a list
  val length : 'a t -> int
  val nth : 'a t -> int -> 'a
end

module Lst = struct
  type 'a t = 'a list

  let empty = []

  let push x t = x :: t

  let pop = function
    | []   -> None
    | x::t -> Some (x, t)

  let eject t =
    let rec go acc = function
      | []   -> None
      | [x]  -> Some (List.rev acc, x)
      | x::t -> go (x::acc) t
    in
    go [] t

  let concat a b = List.rev_append (List.rev a) b
  let inject t x = concat t [x]
  let rev = List.rev
  let to_list t = t
  let length = List.length
  let nth = List.nth
end

module Bi (A : DEQUE) (B : DEQUE) = struct

  let rec pop_to_list t = match B.pop t with
    | None -> []
    | Some (x, t) -> x :: pop_to_list t

  let rec eject_to_list acc t = match B.eject t with
    | None -> acc
    | Some (t, x) -> eject_to_list (x::acc) t
  let eject_to_list t = eject_to_list [] t

  let make a b =
    let xs = A.to_list a in
    let ys = B.to_list b in
    assert (xs = ys) ;
    assert (xs = pop_to_list b) ;
    assert (xs = eject_to_list b) ;
    assert (List.length xs = B.length b) ;
    (a, b)

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

  let eject (a, b) =
    match A.eject a, B.eject b with
    | None, None -> None
    | Some (a, x), Some (b, x') ->
        assert (x = x') ;
        Some (make a b, x)
    | _ -> assert false

  let rev (a, b) = make (A.rev a) (B.rev b)

  let check_nth (a, b) =
    let n = B.length b in
    assert (n = A.length a) ;
    if n = 0
    then ()
    else begin
      let i = Random.int n in
      let x = A.nth a i in
      let y = B.nth b i in
      assert (x = y)
    end

end

module Test (X : DEQUE) = struct

  module D2 = Bi (Lst) (X)

  let elt () = Random.int 10000

  let some_fst t = function
    | None -> t
    | Some (t, _) -> t

  let some_snd t = function
    | None -> t
    | Some (_, t) -> t

  let test t =
    match Random.int 6 with
    | 0 -> D2.push (elt ()) t
    | 1 -> D2.inject t (elt ())
    | 2 -> some_snd t (D2.pop t)
    | 3 -> some_fst t (D2.eject t)
    | 4 -> D2.rev t
    | 5 -> D2.check_nth t ; t
    | _ -> assert false

  let rec test_repeatedly n t =
    if n <= 0
    then ()
    else test_repeatedly (n - 1) (test t)

  let () =
    test_repeatedly 100000 D2.empty

end

let header name =
  Printf.printf "-- %s %s\n%!"
    name
    (String.make (70 - String.length name - 4) '-')

let () = header "Deque"
module A = Test (Cadeque.Deque)
let () = Printf.printf "\n%!"

let () = header "Cadeque"
module B = Test (Cadeque)
let () = Printf.printf "\n%!"
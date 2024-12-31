module type LIST = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

let bench name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%12s : %.3f s\n%!" name (t1 -. t0) ;
  result

module Lst = struct
  type 'a t = 'a list
  let empty = []
  let push x xs = x::xs
  let pop = function
    | [] -> None
    | x::xs -> Some (x, xs)
  let fold_left = List.fold_left
end

module Bench (L : LIST) = struct

  let make n =
    let rec go acc i =
      if i = 0
      then acc
      else go (L.push i acc) (i - 1)
    in
    go L.empty n

  let sum_foldl xs = L.fold_left ( + ) 0 xs

  let rec sum_pop acc xs =
    match L.pop xs with
    | None -> acc
    | Some (x, xs) -> sum_pop (acc + x) xs

  let sum_pop xs = sum_pop 0 xs

  let () =
    let xs = bench "make 10m" (fun () -> make 10_000_000) in
    let x = bench "sum_foldl" (fun () -> sum_foldl xs) in
    let y = bench "sum_pop" (fun () -> sum_pop xs) in
    assert (x = y) ;
    ()

end

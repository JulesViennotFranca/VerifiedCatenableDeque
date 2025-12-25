(** Module type for list-like data structures.
    Provides basic operations: empty, push, pop, and fold_left. *)
module type LIST = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

(** Benchmark a function and print its execution time.
    Returns the result of the function. *)
let bench name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%12s : %.3f s\n%!" name (t1 -. t0) ;
  result

(** Module implementing the LIST interface for standard OCaml lists. *)
module Lst = struct
  type 'a t = 'a list
  let empty = []
  let push x xs = x::xs
  let pop = function
    | [] -> None
    | x::xs -> Some (x, xs)
  let fold_left = List.fold_left
end

(** Functor creating benchmark operations for any list-like data structure. *)
module Bench (L : LIST) = struct

  (** Create a list-like structure containing integers from 1 to n. *)
  let make n =
    let rec go acc i =
      if i = 0
      then acc
      else go (L.push i acc) (i - 1)
    in
    go L.empty n

  (** Sum all elements in the structure using fold_left. *)
  let sum_foldl xs = L.fold_left ( + ) 0 xs

  (** Sum all elements in the structure by repeatedly popping. *)
  let rec sum_pop acc xs =
    match L.pop xs with
    | None -> acc
    | Some (x, xs) -> sum_pop (acc + x) xs

  let sum_pop xs = sum_pop 0 xs

  (** Run benchmarks: create a structure with 10 million elements,
      then sum it using both fold_left and pop, verifying the results match. *)
  let () =
    let xs = bench "make 10m" (fun () -> make 10_000_000) in
    let x = bench "sum_foldl" (fun () -> sum_foldl xs) in
    let y = bench "sum_pop" (fun () -> sum_pop xs) in
    assert (x = y) ;
    ()

end

(* Benchmark standard OCaml lists. *)
let () = Printf.printf "List:\n%!"
module A = Bench (Lst)
let () = Printf.printf "\n%!"

(* Benchmark Deque data structure. *)
let () = Printf.printf "Deque:\n%!"
module B = Bench (Deques.Deque)
let () = Printf.printf "\n%!"

(* Benchmark Steque data structure. *)
let () = Printf.printf "Steque:\n%!"
module C = Bench (Deques.Steque)
let () = Printf.printf "\n%!"

(* Benchmark Cadeque data structure. *)
let () = Printf.printf "Cadeque:\n%!"
module D = Bench (Deques.Cadeque)
let () = Printf.printf "\n%!"

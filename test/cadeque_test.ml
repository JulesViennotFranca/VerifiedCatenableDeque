type 'a m =
  | Done : 'a m
  | Yield : 'a * (unit -> 'a m) -> 'a m
  | Interleave : 'a m * 'a m -> 'a m
  | Nest : (unit -> 'a m) -> 'a m
  | Bind : 'a m * ('a -> 'b m) -> 'b m
  | Delay : (unit -> 'a m) -> 'a m

let ( @ ) a b = Interleave (a, b)
let return x = Yield (x, fun () -> Done)
let ( >>= ) m f = Bind (m, f)
let map f m = Bind (m, fun x -> return (f x))

let rec force1
: type a. a m -> a option
= function
  | Done -> None
  | Yield (x, _) -> Some x
  | Delay m -> force1 (m ())
  | Interleave (a, b) ->
      begin match force1 a with
      | None -> force1 b
      | Some x -> Some x
      end
  | Bind (m, f) ->
      begin match force1 m with
      | None -> None
      | Some x -> force1 (f x)
      end
  | Nest m -> force1 (m ())

let rec iteri
: type a. int -> (int -> a -> int) -> int -> a m -> int
= fun count f d xs ->
  match xs with
  | Done -> count
  | Yield (x, xs) ->
      let count = f count x in
      iteri (count + 1) f d (xs ())
  | Delay m ->
      iteri count f d (m ())
  | Interleave (a, b) ->
      let a, b = if Random.bool () then a, b else b, a in
      let count = iteri count f d a in
      iteri count f d b
  | Bind (m, g) ->
      iteri count (fun count x -> iteri count f d (g x)) d m
  | Nest m ->
      if d <= 0
      then match force1 (m ()) with
           | None -> count
           | Some x -> f count x
      else iteri count f (d - 1) (m ())

let iteri f d xs = iteri 0 f d xs

open Lib.Color.GYOR
module D = Lib.Cadeque.Package.Core
open D

let rec list_gen size m =
  if size <= 0
  then return []
  else let rest = list_gen (size - 1) m in
       m >>= fun x ->
       rest >>= fun xs ->
       return (x::xs)

let buffer_make e size : ('a, 's) Buffer.t m =
  Obj.magic @@ map Lib.Deque.Package.of_list @@ list_gen size e

let buffer_ge8 e : ('a, z ge8) Buffer.t m = buffer_make e (8 + Random.int 6)
let buffer_ge7 e : ('a, z ge7) Buffer.t m = buffer_make e (7 + Random.int 6)
let buffer_ge6 e : ('a, z ge6) Buffer.t m = buffer_make e (6 + Random.int 6)
let buffer_ge5 e : ('a, z ge5) Buffer.t m = buffer_make e (5 + Random.int 6)
let buffer_ge3 e : ('a, z ge3) Buffer.t m = buffer_make e (3 + Random.int 6)
let buffer_eq2 e : ('a, z ge2) Buffer.t m = buffer_make e 2
let buffer_ge1 e : ('a, z ge1) Buffer.t m = buffer_make e (1 + Random.int 6)

type ('a, 'nkind, 'left_color) lchain_color =
  | Lcolor : ('a, 'ck ge1, 'nk, 'cl, 'cr) chain -> ('a, 'nk, 'cl) lchain_color

type ('a, 'nkind, 'color) onechain_color =
  | Onecolor : ('a, 'ck ge1, 'nk, 'c, 'c) chain -> ('a, 'nk, 'c) onechain_color

type ('a, 'nkind) nochain_color =
  | Nocolor : ('a, 'ck ge1, 'nk, 'cl, 'cr) chain -> ('a, 'nk) nochain_color

let rec stored_triple
: type a. a m -> a stored_triple m
= fun e ->
  if Random.bool ()
  then
  Nest
  (fun () ->
  ( buffer_ge3 e >>= fun p ->
    semi_cadeque (stored_triple e) >>= fun (S c) ->
    buffer_ge3 e >>= fun s ->
    return (Big (p, c, s)))
  @
  ( buffer_ge3 e >>= fun p -> return (Small p)))
  else
  Nest
  (fun () ->
  ( buffer_ge3 e >>= fun p -> return (Small p))
  @
  ( buffer_ge3 e >>= fun p ->
    semi_cadeque (stored_triple e) >>= fun (S c) ->
    buffer_ge3 e >>= fun s ->
    return (Big (p, c, s))))

and triple_only_green
: type a. a m -> (a, only, green) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_ge1 e >>= fun b ->
    let hd = Only_end b in
    return (Triple (GT, hd, Empty)))
  @
  ( buffer_ge8 e >>= fun p ->
    buffer_ge8 e >>= fun s ->
    non_empty_chain (stored_triple e) >>= fun (Nocolor c) ->
    let hd = Only (GN, p, s) in
    return (Triple (GT, hd, c)))
  @
  ( buffer_ge7 e >>= fun p ->
    buffer_ge7 e >>= fun s ->
    non_empty_green_left_chain (stored_triple e) >>= fun (Lcolor c) ->
    let hd = Only (YN, p, s) in
    return (Triple (YT, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    single_green_chain (stored_triple e) >>= fun c ->
    let hd = Only (ON, p, s) in
    return (Triple (OST, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    pair_green_green_chain (stored_triple e) >>= fun c ->
    let hd = Only (ON, p, s) in
    return (Triple (OPT, hd, c))))

and triple_left_green
: type a. a m -> (a, left, green) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_ge5 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    let y, z = Buffer.two s in
    let hd = Left (EN, p, y, z) in
    return (Triple (GT, hd, Empty)))
  @
  ( buffer_ge8 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    non_empty_chain (stored_triple e) >>= fun (Nocolor c) ->
    let y, z = Buffer.two s in
    let hd = Left (GN, p, y, z) in
    return (Triple (GT, hd, c)))
  @
  ( buffer_ge7 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    non_empty_green_left_chain (stored_triple e) >>= fun (Lcolor c) ->
    let y, z = Buffer.two s in
    let hd = Left (YN, p, y, z) in
    return (Triple (YT, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    single_green_chain (stored_triple e) >>= fun c ->
    let y, z = Buffer.two s in
    let hd = Left (ON, p, y, z) in
    return (Triple (OST, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    pair_green_green_chain (stored_triple e) >>= fun c ->
    let y, z = Buffer.two s in
    let hd = Left (ON, p, y, z) in
    return (Triple (OPT, hd, c))))

and triple_right_green
: type a. a m -> (a, right, green) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_eq2 e >>= fun p ->
    buffer_ge5 e >>= fun s ->
    let a, b = Buffer.two p in
    let hd = Right (EN, a, b, s) in
    return (Triple (GT, hd, Empty)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge8 e >>= fun s ->
    non_empty_chain (stored_triple e) >>= fun (Nocolor c) ->
    let a, b = Buffer.two p in
    let hd = Right (GN, a, b, s) in
    return (Triple (GT, hd, c)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge7 e >>= fun s ->
    non_empty_green_left_chain (stored_triple e) >>= fun (Lcolor c) ->
    let a, b = Buffer.two p in
    let hd = Right (YN, a, b, s) in
    return (Triple (YT, hd, c)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    single_green_chain (stored_triple e) >>= fun c ->
    let a, b = Buffer.two p in
    let hd = Right (ON, a, b, s) in
    return (Triple (OST, hd, c)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    pair_green_green_chain (stored_triple e) >>= fun c ->
    let a, b = Buffer.two p in
    let hd = Right (ON, a, b, s) in
    return (Triple (OPT, hd, c))))

and triple_only_red
: type a. a m -> (a, only, red) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_ge5 e >>= fun p ->
    buffer_ge5 e >>= fun s ->
    non_empty_green_chain (stored_triple e) >>= fun (Onecolor c) ->
    let hd = Only (RN, p, s) in
    return (Triple (RT, hd, c)))
  @
  ( buffer_ge7 e >>= fun p ->
    buffer_ge7 e >>= fun s ->
    non_empty_red_left_chain (stored_triple e) >>= fun (Lcolor c) ->
    let hd = Only (YN, p, s) in
    return (Triple (YT, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    single_red_chain (stored_triple e) >>= fun c ->
    let hd = Only (ON, p, s) in
    return (Triple (OST, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    pair_green_red_chain (stored_triple e) >>= fun c ->
    let hd = Only (ON, p, s) in
    return (Triple (OPT, hd, c))))

and triple_left_red
: type a. a m -> (a, left, red) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_ge5 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    non_empty_green_chain (stored_triple e) >>= fun (Onecolor c) ->
    let y, z = Buffer.two s in
    let hd = Left (RN, p, y, z) in
    return (Triple (RT, hd, c)))
  @
  ( buffer_ge7 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    non_empty_red_left_chain (stored_triple e) >>= fun (Lcolor c) ->
    let y, z = Buffer.two s in
    let hd = Left (YN, p, y, z) in
    return (Triple (YT, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    single_red_chain (stored_triple e) >>= fun c ->
    let y, z = Buffer.two s in
    let hd = Left (ON, p, y, z) in
    return (Triple (OST, hd, c)))
  @
  ( buffer_ge6 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    pair_green_red_chain (stored_triple e) >>= fun c ->
    let y, z = Buffer.two s in
    let hd = Left (ON, p, y, z) in
    return (Triple (OPT, hd, c))))

and triple_right_red
: type a. a m -> (a, right, red) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_eq2 e >>= fun p ->
    buffer_ge5 e >>= fun s ->
    non_empty_green_chain (stored_triple e) >>= fun (Onecolor c) ->
    let a, b = Buffer.two p in
    let hd = Right (RN, a, b, s) in
    return (Triple (RT, hd, c)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge7 e >>= fun s ->
    non_empty_red_left_chain (stored_triple e) >>= fun (Lcolor c) ->
    let a, b = Buffer.two p in
    let hd = Right (YN, a, b, s) in
    return (Triple (YT, hd, c)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    single_red_chain (stored_triple e) >>= fun c ->
    let a, b = Buffer.two p in
    let hd = Right (ON, a, b, s) in
    return (Triple (OST, hd, c)))
  @
  ( buffer_eq2 e >>= fun p ->
    buffer_ge6 e >>= fun s ->
    pair_green_red_chain (stored_triple e) >>= fun c ->
    let a, b = Buffer.two p in
    let hd = Right (ON, a, b, s) in
    return (Triple (OPT, hd, c))))

and single_green_chain
: type a. a m -> (a, single, only, green, green) chain m
= fun e ->
  Nest
  (fun () ->
  ( triple_only_green e >>= fun t ->
    return (chain_of_triple t)))

and single_red_chain
: type a. a m -> (a, single, only, red, red) chain m
= fun e ->
  Nest
  (fun () ->
  ( triple_only_red e >>= fun t ->
    return (chain_of_triple t)))

and pair_green_green_chain
: type a. a m -> (a, pair, only, green, green) chain m
= fun e ->
  Nest
  (fun () ->
  ( triple_left_green e >>= fun tl ->
    triple_right_green e >>= fun tr ->
    return (Pair (chain_of_triple tl, chain_of_triple tr))))

and pair_green_red_chain
: type a. a m -> (a, pair, only, green, red) chain m
= fun e ->
  Nest
  (fun () ->
  ( triple_left_green e >>= fun tl ->
    triple_right_red e >>= fun tr ->
    return (Pair (chain_of_triple tl, chain_of_triple tr))))

and non_empty_green_chain
: type a. a m -> (a, only, green) onechain_color m
= fun e ->
  Nest
  (fun () ->
  ( single_green_chain e >>= fun c ->
    return (Onecolor c))
  @
  ( pair_green_green_chain e >>= fun c ->
    return (Onecolor c)))

and non_empty_green_left_chain
: type a. a m -> (a, only, green) lchain_color m
= fun e ->
  Nest
  (fun () ->
  ( non_empty_green_chain e >>= fun (Onecolor c) ->
    return (Lcolor c))
  @
  ( pair_green_red_chain e >>= fun c ->
    return (Lcolor c)))

and non_empty_red_left_chain
: type a. a m -> (a, only, red) lchain_color m
= fun e ->
  Nest
  (fun () ->
  ( single_red_chain e >>= fun c ->
    return (Lcolor c))
  @
  ( triple_left_red e >>= fun tl ->
    triple_right_green e >>= fun tr ->
    return (Lcolor (Pair (chain_of_triple tl, chain_of_triple tr))))
  @
  ( triple_left_red e >>= fun tl ->
    triple_right_red e >>= fun tr ->
    return (Lcolor (Pair (chain_of_triple tl, chain_of_triple tr)))))

and non_empty_chain
: type a. a m -> (a, only) nochain_color m
= fun e ->
  Nest
  (fun () ->
  ( non_empty_green_left_chain e >>= fun (Lcolor c) ->
    return (Nocolor c))
  @
  ( non_empty_red_left_chain e >>= fun (Lcolor c) ->
    return (Nocolor c)))

and semi_cadeque
: type a. a m -> a semi_cadeque m
= fun e ->
  Nest
  (fun () ->
  ( return (S Empty))
  @
  ( non_empty_chain e >>= fun (Nocolor c) ->
    return (S c)))

let counter = ref 0
let elt () =
  let v = !counter in
  counter := v + 1 ;
  v

let gen_elt = Delay (fun () -> return (elt ()))

let all_semi = semi_cadeque gen_elt

let max_depth = 6

let rec pop_to_list acc deq =
  match D.pop deq with
  | None -> List.rev acc
  | Some (x, d) -> pop_to_list (x::acc) d
let pop_to_list deq = pop_to_list [] deq

let to_list = pop_to_list

let rec un_to_list left right deq =
  if Random.bool ()
  then match D.pop deq with
    | None -> List.concat [List.rev left ; right]
    | Some (x, d) -> un_to_list (x :: left) right d
  else match D.eject deq with
    | None -> List.concat [List.rev left ; right]
    | Some (d, x) -> un_to_list left (x :: right) d

let un_to_list d = un_to_list [] [] d

let fold_left_to_list t =
  List.rev
    (Lib.Cadeque.Package.fold_left
      (fun xs x -> x::xs)
      []
      ({ core = t; length = 0 }))

let test _ deq =
  let real_deq = D.regularize deq in
  let lst = to_list real_deq in
  assert (lst = Lib.Cadeque.Package.to_list
                  { core = real_deq; length = 0 }) ;
  assert (lst = fold_left_to_list real_deq) ;
  let x = elt () in
  assert (to_list (D.push x real_deq) = x :: lst) ;
  assert (to_list (D.inject real_deq x) = List.concat [lst; [x]] ) ;
  assert (lst = un_to_list real_deq) ;
  iteri
    (fun j deq' ->
      let real_deq' = D.regularize deq' in
      let lst' = to_list real_deq' in
      assert (to_list (D.concat real_deq real_deq')
              = List.concat [lst; lst']) ;
      let d1 = (D.semi_concat deq deq') in
      let d2 = (D.semi_concat deq' deq) in
      let real_deq = D.regularize d1 in
      let dlst = to_list (D.regularize d1) in
      assert (dlst = un_to_list real_deq) ;
      let real_deq = D.regularize d2 in
      let dlst = to_list (D.regularize d2) in
      assert (dlst = un_to_list real_deq) ;
      j
    )
    (max_depth - 4)
    all_semi

let () = ignore (iteri test max_depth all_semi)

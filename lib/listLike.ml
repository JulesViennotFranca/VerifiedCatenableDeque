module type STEQUE = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val inject : 'a t -> 'a -> 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val rev : 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t

  val length : 'a t -> int

  val name : string
end

module type DEQUE = sig
  include STEQUE

  val eject : 'a t -> ('a t * 'a) option
end

module OfSteque (S : STEQUE) = struct

  let hd t = match S.pop t with
    | None -> failwith (S.name ^ ".hd")
    | Some (x, _) -> x

  let tl t = match S.pop t with
    | None -> failwith (S.name ^ ".tl")
    | Some (_, t) -> t

  let iter f t = S.fold_left (fun () x -> f x) () t

  let iteri f t =
    let _ = S.fold_left (fun i x -> f i x ; i + 1) 0 t in
    ()

  let nth (type a) t idx =
    if idx < 0 then invalid_arg (S.name ^ ".nth") ;
    let module M = struct exception Found of a end in
    try iteri (fun i x -> if i = idx then raise (M.Found x)) t ;
        failwith (S.name ^ ".nth")
    with M.Found x -> x

  let nth_opt t idx =
    try Some (nth t idx) with Failure _ -> None

  let map f t =
    S.fold_left (fun ys x -> S.inject ys (f x)) S.empty t

  let mapi f t =
    let _, t =
      S.fold_left
        (fun (i, ys) x -> i + 1, S.inject ys (f i x))
        (0, S.empty)
        t
    in t

  let rev_map f t =
    S.fold_left (fun ys x -> S.push (f x) ys) S.empty t

  let filter_map f t =
    S.fold_left
      (fun ys x ->
        match f x with
        | None -> ys
        | Some y -> S.inject ys y)
      S.empty
      t

  let fold_left_map f z t =
    S.fold_left
      (fun (z, ys) x ->
        let z, y = f z x in
        z, S.inject ys y)
      (z, S.empty)
      t

  let fold_right_map f t z =
    S.fold_right
      (fun x (z, ys) ->
        let z, y = f x z in
        z, S.push y ys)
      t
      (z, S.empty)

  exception Abort

  let exists p t =
    try iter (fun x -> if p x then raise Abort) t ; false
    with Abort -> true

  let for_all p t = not (exists (fun x -> not (p x)) t)

  let mem x t = exists (( = ) x) t
  let memq x t = exists (( == ) x) t

  let find (type a) p t =
    let module M = struct exception Found of a end in
    try iter (fun x -> if p x then raise (M.Found x)) t ;
        raise Not_found
    with M.Found x -> x

  let find_opt (type a) p t =
    let module M = struct exception Found of a end in
    try iter (fun x -> if p x then raise (M.Found x)) t ;
        None
    with M.Found x -> Some x

  let find_map (type a) f t =
    let module M = struct exception Found of a end in
    let g x = match f x with None -> () | Some y -> raise (M.Found y) in
    try iter g t ;
        None
    with M.Found x -> Some x

  let filter f t =
    S.fold_left
      (fun ys x -> if f x then S.inject ys x else ys)
      S.empty
      t

  let find_all = filter

  let filteri f t =
    let _, t =
      S.fold_left
        (fun (i, ys) x ->
          i + 1, if f i x then S.inject ys x else ys)
        (0, S.empty)
        t
    in t

  let partition f t =
    S.fold_left
      (fun (left, right) x ->
        if f x
        then S.inject left x, right
        else left, S.inject right x)
      (S.empty, S.empty)
      t

  let assoc k t =
    let _, y = find (fun (x, _) -> x = k) t in
    y

  let assoc_opt k t =
    find_map (fun (x, y) -> if x = k then Some y else None) t

  let assq k t =
    let _, y = find (fun (x, _) -> x == k) t in
    y

  let assq_opt k t =
    find_map (fun (x, y) -> if x == k then Some y else None) t

  let mem_assoc x t = exists (fun (x', _) -> x = x') t
  let mem_assq x t = exists (fun (x', _) -> x == x') t

  let split t =
    S.fold_left
      (fun (left, right) (x, y) -> S.inject left x, S.inject right y)
      (S.empty, S.empty)
      t

  let to_list t = S.fold_right (fun x xs -> x :: xs) t []

  let of_list xs = List.fold_left (fun t x -> S.inject t x) S.empty xs

  let to_seq t = Seq.unfold S.pop t

  let of_seq s = Seq.fold_left (fun xs x -> S.inject xs x) S.empty s

  let init n f =
    let rec go acc i =
      if i >= n
      then acc
      else let x = f i in
           go (S.inject acc x) (i + 1)
    in
    go S.empty 0

  let to_array t =
    match S.pop t with
    | None -> [| |]
    | Some (x, t) ->
      let n = S.length t in
      let arr = Array.make (n + 1) x in
      iteri (fun i x -> arr.(i + 1) <- x) t ;
      arr

  let of_array t =
    init (Array.length t) (Array.get t)


  let rec merge cmp acc xs ys =
    match S.pop xs with
    | None -> S.append acc ys
    | Some (x, xs) -> merge_left cmp acc x xs ys

  and merge_left cmp acc x xs ys =
    match S.pop ys with
    | None -> S.append (S.inject acc x) xs
    | Some (y, ys) -> merge_head cmp acc x xs y ys

  and merge_right cmp acc xs y ys =
    match S.pop xs with
    | None -> S.append (S.inject acc y) ys
    | Some (x, xs) -> merge_head cmp acc x xs y ys

  and merge_head cmp acc x xs y ys =
    if cmp x y <= 0
    then merge_right cmp (S.inject acc x) xs y ys
    else merge_left  cmp (S.inject acc y) x xs ys

  let merge cmp xs ys = merge cmp S.empty xs ys

  let sort cmp t =
    let t = to_array t in
    Array.sort cmp t ;
    of_array t

  let stable_sort cmp t =
    let t = to_array t in
    Array.stable_sort cmp t ;
    of_array t

  let fast_sort cmp t =
    let t = to_array t in
    Array.fast_sort cmp t ;
    of_array t

  let sort_uniq cmp t =
    of_list @@ List.sort_uniq cmp @@ to_list t


  let fold_left2 ~exn f z xs ys =
    let z, ys =
      S.fold_left
        (fun (z, ys) x ->
          match S.pop ys with
          | None -> raise exn
          | Some (y, ys) ->
              f z x y, ys)
        (z, ys)
        xs
    in
    if S.is_empty ys
    then z
    else raise exn

  let iter2 f xs ys =
    fold_left2 ~exn:(Invalid_argument (S.name ^ ".iter2"))
      (fun () x y -> f x y)
      ()
      xs
      ys

  let map2 f xs ys =
    fold_left2 ~exn:(Invalid_argument (S.name ^ ".map2"))
      (fun t x y -> S.inject t (f x y))
      S.empty
      xs
      ys

  let rev_map2 f xs ys =
    fold_left2 ~exn:(Invalid_argument (S.name ^ ".rev_map2"))
      (fun t x y -> S.push (f x y) t)
      S.empty
      xs
      ys

  let exists2 p xs ys =
    try fold_left2 ~exn:(Invalid_argument (S.name ^ ".exists2"))
          (fun b x y -> if p x y then raise Abort else b)
          false
          xs
          ys
    with Abort -> true

  let for_all2 p xs ys =
    try fold_left2 ~exn:(Invalid_argument (S.name ^ ".for_all2"))
          (fun b x y -> if p x y then b else raise Abort)
          true
          xs
          ys
    with Abort -> false

  let combine xs ys =
    fold_left2 ~exn:(Invalid_argument (S.name ^ ".combine"))
      (fun t x y -> S.inject t (x, y))
      S.empty
      xs
      ys

  let fold_left2 f z xs ys =
    fold_left2 ~exn:(Invalid_argument (S.name ^ ".fold_left2")) f z xs ys



  let compare cmp xs ys =
    let module M = struct exception Return of int end in
    try let ys =
          S.fold_left
            (fun ys x ->
              match S.pop ys with
              | None -> raise (M.Return (-1))
              | Some (y, ys) ->
                  match cmp x y with
                  | 0 -> ys
                  | n -> raise (M.Return n))
            ys
            xs
        in
        if S.is_empty ys then 0 else 1
    with M.Return n -> n

  let equal eq xs ys =
    try let ys =
          S.fold_left
            (fun ys x ->
              match S.pop ys with
              | None -> raise Abort
              | Some (y, ys) ->
                  if eq x y
                  then ys
                  else raise Abort)
            ys
            xs
        in
        S.is_empty ys
    with Abort -> false

  let ( = ) xs ys = equal ( = ) xs ys


  let ( @ ) = S.append

  let rev_append xs ys = S.rev xs @ ys

  let concat xss = S.fold_left (fun z x -> x @ z) S.empty xss

  let flatten = concat

  let concat_map f t =
    S.fold_left (fun ys x -> ys @ f x) S.empty t

end

module OfDeque (D : DEQUE) = struct
  include OfSteque(D)

  let dh t = match D.eject t with
    | None -> failwith (D.name ^ ".dh")
    | Some (_, x) -> x

  let lt t = match D.eject t with
    | None -> failwith (D.name ^ ".lt")
    | Some (t, _) -> t

  let fold_right2 f xs ys z =
    let xs, z =
      D.fold_right
        (fun y (xs, z) ->
          match D.eject xs with
          | None -> raise (Invalid_argument (D.name ^ ".fold_right2"))
          | Some (xs, x) ->
              xs, f x y z)
        ys
        (xs, z)
    in
    if D.is_empty xs
    then z
    else raise (Invalid_argument (D.name ^ ".fold_right2"))
end
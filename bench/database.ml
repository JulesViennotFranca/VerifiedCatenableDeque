(* ============================ useful functions ============================ *)

(** [fold_left f a n] applies [n] times [f] on [a]. *)
let fold_left f a n = List.fold_left (fun a _ -> f a) a (List.init n Fun.id)

(** [pow2 n] computes 2 to the power of [n]. *)
let rec pow2 = function
  | n when n <= 0 -> 1
  | n -> 2 * pow2 (n-1)

(** [choose_bellow min n] selects randomly an integer between [min] and [n].
    It follows a geometric distribution of parameter [1/2]. *)
let rec choose_bellow min = function
  | n when n <= min -> min
  | n -> if Random.bool () then n else choose_bellow min (n-1)

(** [choose_above max n] selects randomly an integer between [n] and [max].
    It follows a geometric distribution of parameter [1/2]. *)
let rec choose_above max = function
  | n when n >= max -> max
  | n -> if Random.bool () then n else choose_above max (n+1)

(** [merge_flatten merge l] returns a list where pairs of consecutive elements
    [[a1; a2]] of [l] have been replaced by the value [merge a1 a2].

    @raise Failure if the list has an odd number of elements. *)
let merge_flatten merge l =
  let rec aux accu = function
    | [] -> List.rev accu
    | [_] -> raise (Failure "merge_flatten")
    | a :: b :: l -> aux (merge a b :: accu) l
  in
  aux [] l

(** [progress_bar title maxN curN] updates a progress bar showing the
    percentage [curN / maxN]. If [title] is specified, the progress bar is
    prefixed with the title. *)
let progress_bar title maxN =
  let memN = ref (-1) in
  let finished = ref false in
  fun curN ->
    if curN <> !memN && not !finished then begin
      let percentage = (curN * 100 / maxN) in
      let blocks = fold_left (fun s -> "█" ^ s) "" percentage in
      let dots = String.make (100 - percentage) '.' in
      Printf.printf "\r%s: |%s%s|%d%% " title blocks dots percentage;
      flush stdout;
      memN := curN;
      if curN >= maxN then begin
        let white_spaces = String.make (100 + 2 + 4 - 5) ' ' in
        Printf.printf "\r%s: DONE.%s\n" title white_spaces;
        finished := true
      end
    end

(* =============================== operations =============================== *)

type 'a operation =
  | Push   of ('a -> 'a)
  | Pop    of ('a -> 'a)
  | Inject of ('a -> 'a)
  | Eject  of ('a -> 'a)
  | Concat of ('a -> 'a -> 'a)

(* ======================= elements with their length ======================= *)

module WithLength = struct
  type 'a t = 'a * int

  let get = fst

  let length = snd

  let wrap_make f n = (f n, n)

  let wrap_op = function
    | Push f   -> Push   (fun (a, len) -> (f a, len + 1))
    | Pop f    -> Pop    (fun (a, len) -> (f a, max 0 (len - 1)))
    | Inject f -> Inject (fun (a, len) -> (f a, len + 1))
    | Eject f  -> Eject  (fun (a, len) -> (f a, max 0 (len - 1)))
    | Concat f -> Concat (fun (a1, len1) (a2, len2) -> (f a1 a2, len1 + len2))
end

(* ================================= slices ================================= *)

module Slice = struct
  type 'a t = { array : 'a array ; mutable length : int }

  let create ~size ~dummy = { array = Array.make size dummy ; length = 0 }

  let add t a =
    t.array.(t.length) <- a;
    t.length <- t.length + 1

  let is_full t = Array.length t.array <= t.length

  let iter f t =
    for i = 0 to t.length - 1 do
      f t.array.(i)
    done

  let iter2 f t1 t2 =
    assert (t1.length = t2.length);
    for i = 0 to t1.length - 1 do
        f t1.array.(i) t2.array.(i)
    done

  let sample t = t.array.(Random.int t.length)

  let sample_n t n = fold_left (fun l -> sample t :: l) [] n
end

(* ================================= ranges ================================= *)

module Range = struct
  type t = int * int

  let make a b = (a, b)

  let inf = fst
  let middle (a, b) = (a + b) / 2
  let sup r = snd r - 1
end

(* =============================== databases ================================ *)

type 'a t = (Range.t * 'a WithLength.t Slice.t) array

let get t n = snd (t.(n))

let create ~buffers ~size make =
  let dummy = make 0 in
  let rec aux accu n = match n with
    | 0 -> Array.of_list accu
    | _ -> aux ((Range.make (n/2) n, Slice.create ~size ~dummy) :: accu) (n/2)
  in
  let t = aux [] (pow2 (buffers - 1)) in
  Slice.add (get t 2) (make 2);
  Slice.add (get t 2) (make 3);
  let add_middle t n =
    let r, s = t.(n) in
    let m = Range.middle r in
    let a = make m in
    Slice.add s a
  in
  List.iter (fun n -> add_middle t (n+3)) (List.init (buffers - 3) Fun.id);
  let add_dummy t n _ =
    let a = make n in
    Slice.add (get t n) a
  in
  List.iter (add_dummy t 0) (List.init size Fun.id);
  List.iter (add_dummy t 1) (List.init size Fun.id);
  t

let add_incr t n f =
  let r, s = t.(n) in
  let m = choose_bellow 0 n in
  let a = Slice.sample (get t m) in
  let cur_length = WithLength.length a in
  if Range.sup r < cur_length + 1 then ()
  else
    let inf = max (Range.inf r) cur_length in
    let v = Range.sup r - (inf + 1) in
    let new_length = inf + 1 + if v <= 0 then v else Random.int v in
    let a = fold_left f a (new_length - cur_length) in
    Slice.add s a

let add_decr t n f =
  let r, s = t.(n) in
  let m = choose_above (Array.length t - 1) n in
  let a = Slice.sample (get t m) in
  let cur_length = WithLength.length a in
  if Range.inf r > cur_length - 1 then ()
  else
    let sup = min (Range.sup r) cur_length in
    let v = sup - 1 - Range.inf r in
    let new_length = sup - 1 - if v <= 0 then v else Random.int v in
    let a = fold_left f a (cur_length - new_length) in
    Slice.add s a

let add_double t n f =
  if n <= 1 then () else
    let m1 = choose_bellow 1 (n-1) in
    let m2 = choose_bellow 1 (n-1) in
    let as1 = Slice.sample_n (get t m1) (pow2 (n - 1 - m1)) in
    let as1 = fold_left (merge_flatten f) as1 (n - 1 - m1) in
    let a1 = List.hd as1 in
    let as2 = Slice.sample_n (get t m2) (pow2 (n - 1 - m2)) in
    let as2 = fold_left (merge_flatten f) as2 (n - 1 - m2) in
    let a2 = List.hd as2 in
    Slice.add (get t n) (f a1 a2)

let cur_length t =
  let d = - (get t 0).Slice.length (* section 0 is full at the start *)
          - (get t 1).length       (* section 1 is full at the start *)
          - 2                      (* section 2 starts with two elements *)
          - (Array.length t - 3)   (* other sections start with one element *)
  in
  Array.fold_left (fun sum (_, s) -> sum + s.Slice.length) d t

let max_length t =
  let d = - Array.length (get t 0).Slice.array
          - Array.length (get t 1).array
          - 2
          - (Array.length t - 3)
  in
  Array.fold_left (fun sum (_, s) -> sum + Array.length s.Slice.array) d t

let update_progress_bar pb t = pb (cur_length t)

let add_with pb t f =
  update_progress_bar pb t;
  let l = List.init (Array.length t) Fun.id in
  let l = List.filter (fun n -> not (Slice.is_full (get t n))) l in
  let a = Array.of_list l in
  if Array.length a = 0 then
    false
  else
    let n = a.(Random.int (Array.length a)) in
    begin match f with
      | Push push     -> add_incr t n push
      | Pop pop       -> add_decr t n pop
      | Inject inject -> add_incr t n inject
      | Eject eject   -> add_decr t n eject
      | Concat concat -> add_double t n concat
    end;
    true

let build ~buffers ~size ~make ~operations =
  let make = WithLength.wrap_make make in
  let len_op = Array.length operations in
  let operations = Array.map WithLength.wrap_op operations in
  let db = create ~buffers ~size make in
  let f = ref (operations.(Random.int len_op)) in
  let pb = progress_bar "building" (max_length db) in
  while add_with pb db !f do
    f := operations.(Random.int len_op)
  done;
  db

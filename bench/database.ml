(* ============================ useful functions ============================ *)

(** [pow2 n] computes 2 to the power of [n]. *)
let pow2 n =
  if n < Sys.word_size - 2 then
    1 lsl n
  else
    failwith (Printf.sprintf "Cannot compute 2^%d" n)

(**[log2 n] is the base 2 logarithm of [n]. *)
let rec log2 accu n =
  if n <= 1 then accu else log2 (accu + 1) (n / 2)
let log2 n = log2 0 n

(* 50 square block characters (3 bytes each). *)
let blocks = List.init 50 (fun _ -> "█") |> String.concat ""
(* A string of [n] square block characters, where [n] is at most 50. *)
let blocks n = String.sub blocks 0 (3 * n)
(* A string of [n] dot characters. *)
let dots n = String.make n '.'

(** [progress_bar title maxN curN] updates a progress bar showing the
    percentage [curN / maxN]. If [title] is specified, the progress bar is
    prefixed with the title. *)
let progress_bar title maxN =
  let memN = ref (-1) in
  let finished = ref false in
  fun curN ->
    if curN <> !memN && not !finished then begin
      let percentage = (curN * 100 / maxN) in
      let blocks = blocks (percentage / 2) in
      let dots = dots (50 - percentage / 2) in
      Printf.printf "\r%s: |%s%s|%d%% " title blocks dots percentage;
      flush stdout;
      memN := curN;
      if curN >= maxN then begin
        let white_spaces = String.make (50 + 2 + 4 - 5) ' ' in
        Printf.printf "\r%s: DONE.%s\n" title white_spaces;
        finished := true
      end
    end

(* ================================= slices ================================= *)

(* A vector is an array that we intend to progressively fill. In other words,
   it is a vector (in the usual sense) whose capacity is fixed ahead of time. *)

module Vector = struct

  type 'a t =
    { array : 'a array; mutable length : int }

  let create ~size ~dummy =
    { array = Array.make size dummy ; length = 0 }

  let get t i =
    assert (0 <= i && i < t.length);
    t.array.(i)

  let add t a =
    assert (t.length < Array.length t.array);
    t.array.(t.length) <- a;
    t.length <- t.length + 1

  let is_full t =
    Array.length t.array = t.length

  let iter f t =
    for i = 0 to t.length - 1 do
      f t.array.(i)
    done

  let iter2 f t1 t2 =
    for i = 0 to t1.length - 1 do
      for j = 0 to t2.length - 1 do
        f t1.array.(i) t2.array.(j)
      done;
    done

  let to_list t =
    List.init t.length (Array.get t.array)

  let sample t =
    t.array.(Random.int t.length)

end

(* ================================= ranges ================================= *)

module Range = struct
  type t = int * int

  let make a b = (a, b)

  let inf = fst
  let middle (a, b) = (a + b) / 2
  let sup r = snd r - 1

  let is_in r i = fst r <= i && i < snd r

  let to_string (i, j) = "[" ^ string_of_int i ^ ", " ^ string_of_int j ^ "["
end

(* ================================= traces ================================= *)

type operation =
  | Push of int
  | Pop of int
  | Inject of int
  | Eject of int
  | Concat of int * int

let string_of_operation = function
  | Push i -> "Push " ^ string_of_int i
  | Pop i -> "Pop " ^ string_of_int i
  | Inject i -> "Inject " ^ string_of_int i
  | Eject i -> "Eject " ^ string_of_int i
  | Concat (i1, i2) -> "Concat " ^ string_of_int i1 ^ " " ^ string_of_int i2

module Traces = struct
  type t = (int * operation) list array

  (** Create a traces database with n elements. *)
  let create n = Array.make n []

  (** [save t i op j] saves that [j] is obtained by applying operation [op] on [i] in the trace database [t]. *)
  let save t i op j = if i >= 0 then t.(i) <- (j, op) :: t.(i)

  let to_string t = String.concat "\n" (List.map (fun (i, l) ->
    string_of_int i ^ " [" ^
    String.concat ", " (List.map (fun (j, op) ->
      "(" ^ string_of_int j ^ ", " ^ string_of_operation op ^ ")"
    ) l) ^ "]"
  ) (Array.to_list (Array.mapi (fun i l -> (i, l)) t)))
end

(* ============================= Raw databases ============================== *)

(** A database stores elements, along with their respective range, and the traces leading to the creation of the elements. *)
type 'a t = {
  elements : 'a Vector.t ;
  ranges : (Range.t * int Vector.t) array ;
  traces : Traces.t ;
}

(** A raw database stores lengths of elements, along with their respective range, and the traces leading to the creation of the elements. So no elements is created. *)
type raw_t = int t

let string_of_database db string_of_a =
  "Elements:\n" ^
  String.concat "\n" (List.mapi (fun i a ->
    string_of_int i ^ ": " ^ string_of_a a)
  (Vector.to_list db.elements)) ^
  "\n\nRanges:\n" ^
  String.concat "\n" (
    List.map (fun (r, s) ->
      Range.to_string r ^ " " ^
        String.concat ", " (List.map string_of_int (Vector.to_list s))
    ) (Array.to_list db.ranges)
  ) ^
  "\n\nTraces:\n" ^
  Traces.to_string db.traces

(** [raw_add_element rdb len p op] adds [len] to the raw database [rdb]. [len] is the length of an element obtained by applying [op] on an element whose size is stored at the index [p] in [rdb]. *)
let raw_add_element rdb len p op =
  assert (not (Vector.is_full rdb.elements));
  let idx = rdb.elements.length in
  let ridx = ref 0 in
  while not (Range.is_in (fst rdb.ranges.(!ridx)) len) do
    ridx := !ridx + 1
  done;
  Vector.add rdb.elements len;
  Vector.add (snd rdb.ranges.(!ridx)) idx;
  Traces.save rdb.traces p op idx

(** Create a raw database with only the length of the empty elements stored. *)
let raw_create ~bins ~binhabitants =
  let rec aux accu n = match n with
    | 0 -> Array.of_list accu
    | _ -> aux ((Range.make (n/2) n, Vector.create ~size:binhabitants ~dummy:(-1)) :: accu) (n/2)
  in
  let ranges = aux [] (pow2 (bins - 1)) in
  let rdb = {
    elements = Vector.create ~size:(bins * binhabitants) ~dummy:(-1) ;
    ranges = ranges ;
    traces = Traces.create (bins * binhabitants) ;
  } in
  raw_add_element rdb 0 (-1) (Push (-1));
  rdb

(** Is the given range of the raw database full ? *)
let is_range_full rdb ridx = Vector.is_full (snd rdb.ranges.(ridx))

(** Has the given range of the raw database some space available ? *)
let is_range_avail rdb ridx = not (is_range_full rdb ridx)

(** Has the given range of the raw database some space available ? *)
let is_next_range_avail rdb ridx =
  ridx < Array.length rdb.ranges - 1 && is_range_avail rdb (ridx + 1)

(** Does the length of an element stored at index [i] in [rdb], contained in range [ridx], allow for a decreasing operation ? *)
let is_possible_decr rdb i ridx =
  (ridx <> 0) && (
    let len = Vector.get rdb.elements i in
    let inf = Range.inf (fst rdb.ranges.(ridx)) in
    if inf == len then is_range_avail rdb (ridx - 1)
    else is_range_avail rdb ridx
  )

(** Does the length of an element stored at index [i] in [rdb], contained in range [ridx], allow for an increasing operation ? *)
let is_possible_incr rdb i ridx =
  let len = Vector.get rdb.elements i in
  let sup = Range.sup (fst rdb.ranges.(ridx)) in
  if len == sup then is_next_range_avail rdb ridx
  else is_range_avail rdb ridx

(** Return indices of elements whose lengths permit some unary operation to be performed to obtain a new element in the raw database. *)
let possible_ucandidates rdb =
  let res = ref [] in
  let a2res x = res := x :: !res in
  for ridx = 0 to Array.length rdb.ranges - 1 do
    Vector.iter (fun i ->
      if is_possible_decr rdb i ridx then
        begin a2res (Pop i); a2res (Eject i) end;
      if is_possible_incr rdb i ridx then
        begin a2res (Push i); a2res (Inject i) end
    ) (snd rdb.ranges.(ridx))
    done;
  Array.of_list !res

(** Does the length of an element contained in range [ridx] in [rdb], allow for a doubling operation ? *)
let is_possible_add rdb i1 i2 ridx1 ridx2 =
  if ridx1 == ridx2 then is_next_range_avail rdb ridx1
  else
    let ridx = if ridx1 < ridx2 then ridx2 else ridx1 in
    let len = Vector.get rdb.elements i1 + Vector.get rdb.elements i2 in
    if Range.is_in (fst rdb.ranges.(ridx)) len then is_range_avail rdb ridx
    else is_next_range_avail rdb ridx

(** Return indices of elements whose lengths permit some binary operation to be performed to obtain a new element in the raw database. The authorized operations are returned along the indices. *)
let possible_bcandidates rdb =
  let res = ref [] in
  let a2res x = res := x :: !res in
  for ridx1 = 1 to Array.length rdb.ranges - 1 do
    for ridx2 = ridx1 to Array.length rdb.ranges - 1 do
      Vector.iter2 (fun i1 i2 ->
        if i1 <= i2 && is_possible_add rdb i1 i2 ridx1 ridx2 then
          a2res (Concat (i1, i2))
      ) (snd rdb.ranges.(ridx1)) (snd rdb.ranges.(ridx2))
      done;
    done;
  Array.of_list !res

(** Choose a candidate among several. *)
let choose_candidate candidates =
  let len = Array.length candidates in
  candidates.(Random.int len)

(** Construct randomly a raw database with [bins] ranges, each of size [size]. *)
let raw_construct ~bins ~binhabitants  =
  let rdb = raw_create ~bins ~binhabitants in
  let ucandidates = ref (possible_ucandidates rdb) in
  let bcandidates = ref (possible_bcandidates rdb) in
  while Array.length !ucandidates + Array.length !bcandidates > 0 do
    let candidates =
      if Array.length !ucandidates == 0 then !bcandidates
      else if Array.length !bcandidates == 0 then !ucandidates
      else if Random.int 5 < 4 then !ucandidates else !bcandidates
    in
    let op = choose_candidate candidates in
    begin match op with
      | Push i | Inject i ->
        let len = Vector.get rdb.elements i in
        raw_add_element rdb (len + 1) i op
      | Pop i | Eject i ->
        let len = Vector.get rdb.elements i in
        raw_add_element rdb (len - 1) i op
      | Concat (i1, i2) ->
        let len1 = Vector.get rdb.elements i1 in
        let len2 = Vector.get rdb.elements i2 in
        raw_add_element rdb (len1 + len2) (max i1 i2) op
    end;
    ucandidates := possible_ucandidates rdb;
    bcandidates := possible_bcandidates rdb
  done;
  rdb

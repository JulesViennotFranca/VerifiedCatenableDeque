open Printf

module Vector =
  FixedCapacityVector

(* ============================ useful functions ============================ *)

(** [pow2 n] computes 2 to the power of [n]. *)
let pow2 n =
  if n < Sys.word_size - 2 then
    1 lsl n
  else
    failwith (sprintf "Cannot compute 2^%d" n)

(**[log2 n] is the base 2 logarithm of [n]. *)
let rec log2 accu n =
  if n <= 1 then accu else log2 (accu + 1) (n / 2)
let log2 n =
  if n = 0 then -1 else log2 0 n

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
      printf "\r%s: |%s%s|%d%% " title blocks dots percentage;
      flush stdout;
      memN := curN;
      if curN >= maxN then begin
        let white_spaces = String.make (50 + 2 + 4 - 5) ' ' in
        printf "\r%s: DONE.%s\n" title white_spaces;
        finished := true
      end
    end

(* ================================= ranges ================================= *)

(* A range represents an interval [a, b). *)

module Range = struct

  type t = int * int

  let make a b = (a, b)

  let inf (a, _) = a
  let sup (_, b) = b - 1

  let is_in (a, b) i = a <= i && i < b

  let to_string (i, j) = "[" ^ string_of_int i ^ ", " ^ string_of_int j ^ "["

end

(* ================================= traces ================================= *)

(* A variable is a de Bruijn level, that is, a creation time. *)
type var =
  int

(* An operation carries its operands: zero, one, or two variables. *)
type operation =
  | Empty
  | Push of var
  | Pop of var
  | Inject of var
  | Eject of var
  | Concat of var * var

let show_var =
  string_of_int

let show_op = function
  | Empty -> "empty"
  | Push i -> "push " ^ show_var i
  | Pop i -> "pop " ^ show_var i
  | Inject i -> "inject " ^ show_var i
  | Eject i -> "eject " ^ show_var i
  | Concat (i1, i2) -> "concat " ^ show_var i1 ^ " " ^ show_var i2

(* An assignment is a pair of a target variable and an operation. *)
type assignment =
  var * operation

let show_assignment j op =
  sprintf "%s := %s\n" (show_var j) (show_op op)

(**A history is a sequence of operations. The target of the operation at
   index [i] is the variable [i]. *)
type history =
  operation array

let show_history h =
  h |> Array.to_list |> List.mapi show_assignment |> String.concat ""

(* ============================= Raw databases ============================== *)

(**A database stores a collection of elements of type ['a]. An element is
   some kind of sequence data structure (a list, a deque, etc.), so it has
   a length. The database keeps track of a histogram of elements (that is,
   it stores elements in bins, based on their length). It also records the
   history that allows creating these elements. *)
type 'a t = {

  elements : 'a Vector.t;
  (**The elements. The index of an element in this vector is its identifier,
     that is, its creation date. *)

  bin : bin array;
  (**The histogram, an array of bins, whose ranges form a partition of the
     interval of permitted lengths. *)

  history : history;
  (**The history that allows creating these elements. *)

}

(**A bin is a pair of a range and a set of elements (identifiers) which
   inhabit this bin. That is, for each element [x] in this vector, the
   length of [x] is a member of this range. *)
and bin =
  Range.t * var Vector.t

(** A raw database stores just length information. That is, a sequence data
    structure is summarized by just its length. *)
type raw_t = int t

let string_of_database db string_of_a =
  "Elements:\n" ^
  String.concat "\n" (List.mapi (fun i a ->
    show_var i ^ ": " ^ string_of_a a)
  (Vector.to_list db.elements)) ^
  "\n\nBins:\n" ^
  String.concat "\n" (
    List.map (fun (r, s) ->
      Range.to_string r ^ " " ^
        String.concat ", " (List.map show_var (Vector.to_list s))
    ) (Array.to_list db.bin)
  ) ^
  "\n\nHistory:\n" ^
  show_history db.history

(** [raw_add_element rdb op len] adds the operation [op] to the raw database
    [rdb] and records that the length of its result is [len]. *)
let raw_add_element rdb op len =
  assert (not (Vector.is_full rdb.elements));
  (* Find out in which bin the length [len] falls. Linear search is used,
     as performance is not critical here. *)
  let b = ref 0 in
  while not (Range.is_in (fst rdb.bin.(!b)) len) do
    b := !b + 1
  done;
  (* We could also use this formula. *)
  assert (!b = log2 len + 1);
  (* This new element receives the identifier [i]. *)
  let i = rdb.elements.length in
  Vector.push rdb.elements len;
  let _, bin_inhabitants = rdb.bin.(!b) in
  Vector.push bin_inhabitants i;
  rdb.history.(i) <- op

(**[raw_create ~bins ~binhabitants] creates a raw database where the number of
   bins is [bins] and the number of inhabitants per bin is [binhabitants]. *)
let raw_create ~bins ~binhabitants =
  let rec aux accu n = match n with
    | 0 -> Array.of_list accu
    | _ -> aux ((Range.make (n/2) n, Vector.create ~size:binhabitants ~dummy:(-1)) :: accu) (n/2)
  in
  let population = bins * binhabitants in
  let elements = Vector.create ~size:population ~dummy:(-1)
  and bin = aux [] (pow2 (bins - 1))
  and history = Array.make population Empty in
  { elements; bin; history }

(** Is the given range of the raw database full ? *)
let bin_is_full rdb b =
  let _, inhabitants = rdb.bin.(b) in
  Vector.is_full inhabitants

(** Has the given range of the raw database some space available ? *)
let bin_is_not_full rdb b =
  not (bin_is_full rdb b)

(** Has the given range of the raw database some space available ? *)
let is_next_range_avail rdb ridx =
  ridx < Array.length rdb.bin - 1 && bin_is_not_full rdb (ridx + 1)

(** Does the length of an element stored at index [i] in [rdb], contained in range [ridx], allow for a decreasing operation ? *)
let is_possible_decr rdb i ridx =
  (ridx <> 0) && (
    let len = Vector.get rdb.elements i in
    let inf = Range.inf (fst rdb.bin.(ridx)) in
    if inf == len then bin_is_not_full rdb (ridx - 1)
    else bin_is_not_full rdb ridx
  )

(** Does the length of an element stored at index [i] in [rdb], contained in range [ridx], allow for an increasing operation ? *)
let is_possible_incr rdb i ridx =
  let len = Vector.get rdb.elements i in
  let sup = Range.sup (fst rdb.bin.(ridx)) in
  if len == sup then is_next_range_avail rdb ridx
  else bin_is_not_full rdb ridx

(** Return indices of elements whose lengths permit some unary operation to be performed to obtain a new element in the raw database. *)
let possible_ucandidates rdb =
  let res = ref [] in
  let a2res x = res := x :: !res in
  for ridx = 0 to Array.length rdb.bin - 1 do
    Vector.iter (fun i ->
      if is_possible_decr rdb i ridx then
        begin a2res (Pop i); a2res (Eject i) end;
      if is_possible_incr rdb i ridx then
        begin a2res (Push i); a2res (Inject i) end
    ) (snd rdb.bin.(ridx))
    done;
  Array.of_list !res

(** Does the length of an element contained in range [ridx] in [rdb], allow for a doubling operation ? *)
let is_possible_add rdb i1 i2 ridx1 ridx2 =
  if ridx1 == ridx2 then is_next_range_avail rdb ridx1
  else
    let ridx = if ridx1 < ridx2 then ridx2 else ridx1 in
    let len = Vector.get rdb.elements i1 + Vector.get rdb.elements i2 in
    if Range.is_in (fst rdb.bin.(ridx)) len then bin_is_not_full rdb ridx
    else is_next_range_avail rdb ridx

(** Return indices of elements whose lengths permit some binary operation to be performed to obtain a new element in the raw database. The authorized operations are returned along the indices. *)
let possible_bcandidates rdb =
  let res = ref [] in
  let a2res x = res := x :: !res in
  for ridx1 = 1 to Array.length rdb.bin - 1 do
    for ridx2 = ridx1 to Array.length rdb.bin - 1 do
      Vector.iter2 (fun i1 i2 ->
        if i1 <= i2 && is_possible_add rdb i1 i2 ridx1 ridx2 then
          a2res (Concat (i1, i2))
      ) (snd rdb.bin.(ridx1)) (snd rdb.bin.(ridx2))
      done;
    done;
  Array.of_list !res

(** Choose a candidate among several. *)
let choose_candidate candidates =
  let len = Array.length candidates in
  candidates.(Random.int len)

(** Construct randomly a raw database with [bins] bins, each of size [size]. *)
let raw_construct ~bins ~binhabitants  =
  let rdb = raw_create ~bins ~binhabitants in
  raw_add_element rdb Empty 0;
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
      | Empty ->
          raw_add_element rdb op 0
      | Push i | Inject i ->
          let len = Vector.get rdb.elements i in
          raw_add_element rdb op (len + 1)
      | Pop i | Eject i ->
          let len = Vector.get rdb.elements i in
          raw_add_element rdb op (len - 1)
      | Concat (i1, i2) ->
          let len1 = Vector.get rdb.elements i1 in
          let len2 = Vector.get rdb.elements i2 in
          raw_add_element rdb op (len1 + len2)
    end;
    ucandidates := possible_ucandidates rdb;
    bcandidates := possible_bcandidates rdb
  done;
  rdb

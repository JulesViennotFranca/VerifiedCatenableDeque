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

let postincrement r =
  r := !r + 1;
  !r

(* [with_progress_bar title maxN yield] creates a progress bar with title
   [title] and maximum value [maxN] and applies [yield] to a function [tick]
   which increments the value of the progress bar by one. *)
let with_progress_bar title maxN yield =
  let pb = progress_bar title maxN in
  let c = ref 0 in
  let tick () = pb (postincrement c) in
  yield tick

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

(* [raw_interpret len op] computes the length of the result of the operation
   [op], under the assumption that the function [len] provides access to the
   length of the operands. *)
let raw_interpret len op =
  match op with
  | Empty ->
      0
  | Push i
  | Inject i ->
      len i + 1
  | Pop i
  | Eject i ->
      len i - 1
  | Concat (i1, i2) ->
      len i1 + len i2

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

(**[find_bin rdb len] translates the length [len] to a bin index.
   That is, it finds out in which bin the length [len] falls.
   We assume that [len] does fall within an existing bin. *)
let find_bin rdb len =
  (* Linear search is used, as performance is not critical here. *)
  let b = ref 0 in
  while not (Range.is_in (fst rdb.bin.(!b)) len) do
    b := !b + 1
  done;
  (* We could also use this formula. *)
  assert (!b = log2 len + 1);
  !b

(** [raw_add_element rdb op len] adds the operation [op] to the raw database
    [rdb] and records that the length of its result is [len]. *)
let raw_add_element rdb op len =
  let b = find_bin rdb len in
  (* This new element receives the identifier [i]. *)
  let i = rdb.elements.length in
  Vector.push rdb.elements len;
  let _, bin_inhabitants = rdb.bin.(b) in
  Vector.push bin_inhabitants i;
  rdb.history.(i) <- op

(**[raw_create ~bins ~binhabitants] creates a raw database where the number of
   bins is [bins] and the number of inhabitants per bin is [binhabitants]. *)
let raw_create ~bins ~binhabitants =
  let rec aux accu n = match n with
    | 0 -> Array.of_list accu
    | _ -> aux ((Range.make (n/2) n, Vector.create binhabitants (-1)) :: accu) (n/2)
  in
  let population = bins * binhabitants in
  let elements = Vector.create population (-1)
  and bin = aux [] (pow2 (bins - 1))
  and history = Array.make population Empty in
  { elements; bin; history }

(** Is the given range of the raw database full ? *)
let bin_is_full rdb b =
  let _, inhabitants = rdb.bin.(b) in
  Vector.is_full inhabitants

(**[is_permitted rdb len] determines whether the length [len] falls within an
   existing bin and this bin is not full. *)
let is_permitted rdb len =
  0 <= len &&
  let bins = Array.length rdb.bin in
  let bound = Range.sup (fst rdb.bin.(bins - 1)) in
  len <= bound &&
  let b = find_bin rdb len in
  not (bin_is_full rdb b)

(**[is_permitted rdb top] determines whether the opereation [op] is permitted,
   by applying [is_permitted rdb len] to the result length of this operation. *)
let is_permitted rdb op =
  let get i = Vector.get rdb.elements i in
  is_permitted rdb (raw_interpret get op)

(** Return the unary operations that could be performed to construct a new
    element. An operation is permitted if the length of its result falls
    within a valid non-full bin. *)
let possible_ucandidates rdb : operation array =
  let candidates = ref [] in
  let retain op = candidates := op :: !candidates in
  let retain_if_permitted op = if is_permitted rdb op then retain op in
  for j = 0 to Vector.length rdb.elements - 1 do
    List.iter retain_if_permitted [Pop j; Eject j; Push j; Inject j]
  done;
  Array.of_list !candidates

(** Return the binary operations that could be performed to construct a new
    element. An operation is permitted if the length of its result falls
    within a valid non-full bin. *)
let possible_bcandidates rdb =
  let candidates = ref [] in
  let retain op = candidates := op :: !candidates in
  let retain_if_permitted op = if is_permitted rdb op then retain op in
  for i1 = 0 to Vector.length rdb.elements - 1 do
    for i2 = 0 to Vector.length rdb.elements - 1 do
      retain_if_permitted (Concat (i1, i2))
    done;
  done;
  Array.of_list !candidates

(** Choose a candidate among an array of candidates. *)
let choose candidates =
  let len = Array.length candidates in
  candidates.(Random.int len)

(**[sample k candidates] chooses [k] distinct elements out of the array
   [candidates]. The length of this array must be at least [k]. The time
   complexity of this operation is linear in [n]. *)
let sample k candidates =
  let n = Array.length candidates in
  assert (k <= n);
  let reservoir = Array.sub candidates 0 k in
  for i = k to n-1 do
    let j = Random.int i in
    if j < k then
      reservoir.(j) <- candidates.(i)
  done;
  reservoir

(** Construct randomly a raw database with [bins] bins, each of which has
    [binhabitants] inhabitants. *)
let raw_construct ~bins ~binhabitants =
  with_progress_bar "raw database" (bins * binhabitants) @@ fun tick ->
  let rdb = raw_create ~bins ~binhabitants in
  let apply op =
    let len = raw_interpret (Vector.get rdb.elements) op in
    raw_add_element rdb op len;
    tick()
  in
  let apply_if_permitted op =
    if is_permitted rdb op then
      apply op
  in
  apply Empty;
  let ucandidates = ref (possible_ucandidates rdb)
  and bcandidates = ref (possible_bcandidates rdb) in
  while Array.length !ucandidates + Array.length !bcandidates > 0 do
    (* A naive algorithm picks one candidate, applies this operation, then
       re-computes the set of all candidates. However, this is very costly
       (cubic time). To save time, we pick several candidates, which are
       likely to be independent. (They might conflict with each other only if
       the destination bin is near full.) *)
    let k = 12 in
    let u = Array.length !ucandidates
    and b = Array.length !bcandidates in
    let n = u + b in
    let () =
      if k <= u && k / 4 <= b then begin
        (* Pick 80% of unary operations. *)
        Array.iter apply_if_permitted (sample k !ucandidates);
        Array.iter apply_if_permitted (sample (k/4) !bcandidates)
      end
      else
        let candidates =
          if u = 0 then bcandidates
          else if b = 0 then ucandidates
          else
          (* If both unary and binary operations are permitted,
             we choose a unary operation with 80% probability. *)
          if Random.int 5 < 4 then ucandidates else bcandidates
        in
        let op = choose !candidates in
        assert (is_permitted rdb op);
        apply op
    in
    ucandidates := possible_ucandidates rdb;
    bcandidates := possible_bcandidates rdb
  done;
  rdb

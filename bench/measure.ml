open Printf
open Database

(* ============================ useful functions ============================ *)

(** Tail-recursive map. *)
let tr_map f l = List.rev (List.fold_left (fun accu x -> f x :: accu) [] l)

(** Tail-recursive map2. *)
let tr_map2 f l1 l2 =
  List.rev (List.fold_left2 (fun accu x1 x2 -> f x1 x2 :: accu) [] l1 l2)

(** Remove the last character of a string. *)
let remove_last_char str =
  let len = String.length str in
  String.sub str 0 (len - 1)

(* ================================== data ================================== *)

(**A measurement is a pair of a duration (in seconds) and a number of
   repetitions. *)
type t = float * int

let to_string_label lbl = sprintf "%sT,%sN" lbl lbl
let to_string_data (t, n) = sprintf "%f,%d" t n

let of_string t n = (float_of_string t, int_of_string n)

let get_time = fst

let base = (0., 0)

let format (f, n) = (1_000_000. *. f, n)

let add (f1, n1) (f2, n2) = (f1 +. f2, n1 + n2)

(* =============================== operations =============================== *)

let wrap_uop f steps (a, len) =
  try
    let steps = steps len in
    let t1 = Unix.gettimeofday () in
    for _ = 1 to steps do
      ignore (f a)
    done;
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, steps)
  with _ -> (0., 0)

let wrap_bop f steps (a1, len1) (a2, len2) =
  try
    let steps = steps len1 len2 in
    let t1 = Unix.gettimeofday () in
    for _ = 1 to steps do
      ignore (f a1 a2)
    done;
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, steps)
  with _ -> (0., 0)

(* =========================== CSV files support ============================ *)

module CSV = struct

  let csvdir = "./tmp"

  (** Writing measurements to disk in a [.csv] file. *)
  let write operation_name structure_name (measurements : t array) =
    if not (Sys.file_exists csvdir) then Sys.mkdir csvdir 0o755;
    let file = sprintf "%s/%s-%s.csv" csvdir structure_name operation_name in
    let oc = open_out file in
    fprintf oc "%s\n" (to_string_label structure_name);
    Array.iter (fun m ->
      fprintf oc "%s\n" (to_string_data m)
    ) measurements;
    close_out oc

end

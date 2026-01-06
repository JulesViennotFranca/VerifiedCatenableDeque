open Printf

(* ================================== data ================================== *)

(** A module representing data points.

    Each data point [(t, n)] is associated to a particular operation
    and captures the time [t] it took to compute [n] times this operation.*)
module Data = struct

  (** Type of data points. *)
  type t = float * int

  (** Return the string representation of a data point. *)
  let to_string (t, n) = sprintf "%f,%d" t n

  (** Return a data point from its string representation. *)
  let of_string t n = (float_of_string t, int_of_string n)

  (** Get the time part of a data point. *)
  let get_time = fst

  (** The base data point [(0., 0)]. *)
  let base = (0., 0)

  (** Change the time of a data point from seconds to microseconds. *)
  let to_mus (f, n) = (1_000_000. *. f, n)

  (** Add two data points together. *)
  let add (f1, n1) (f2, n2) = (f1 +. f2, n1 + n2)

end

(* ================================ measure ================================= *)

(** Wrap a unary operation on structures to measure its execution time.
    [wrap_uop f f_steps] returns a new function computing data points
    corresponding to executions of [f]. This new function takes a pair
    [(s, len)] as argument where [s] is a structure, and [len] is its length.
    Based on [len], [f_steps] computes the number of times [f] is applied on [s]
    to obtain a new data point. *)
let wrap_uop f f_steps (s, len) =
  try
    let steps = f_steps len in
    let t1 = Unix.gettimeofday () in
    for _ = 1 to steps do
      ignore (f s)
    done;
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, steps)
  with _ -> (0., 0)

(** Wrap a binary operation on structures to measure its execution time.
    [wrap_bop f f_steps] returns a new function computing data points
    corresponding to executions of [f]. This new function takes two pairs
    [(s1, len1)] and [(s2, len2)] as arguments where [s1] and [s2] are
    structures, and [len1] and [len2] are their corresponding lengths. Based on
    [len1] and [len2], [f_steps] computes the number of times [f] is applied on
    [s1] and [s2] to obtain a new data point. *)
let wrap_bop f steps (s1, len1) (s2, len2) =
  try
    let steps = steps len1 len2 in
    let t1 = Unix.gettimeofday () in
    for _ = 1 to steps do
      ignore (f s1 s2)
    done;
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, steps)
  with _ -> (0., 0)

(* =========================== CSV files support ============================ *)


(** Module providing some CSV files support. *)
module CSV = struct

  (** Return the CSV labels associated with a structure. *)
  let to_string_label lbl = sprintf "%sT,%sN" lbl lbl

  (** The directory containing the CSV files. *)
  let csvdir = "./tmp"

  (** Create the directory containing the CSV files if it does not exists. *)
  let create_csvdir () =
    if not (Sys.file_exists csvdir) then Sys.mkdir csvdir 0o777

  (** Writing measurements to disk in a [.csv] file. *)
  let write operation_name structure_name (measurements : Data.t array) =
    create_csvdir ();
    if not (Sys.file_exists csvdir) then Sys.mkdir csvdir 0o755;
    let file = sprintf "%s/%s-%s.csv" csvdir structure_name operation_name in
    let oc = open_out file in
    fprintf oc "%s\n" (to_string_label structure_name);
    Array.iter (fun m ->
      fprintf oc "%s\n" (Data.to_string m)
    ) measurements;
    close_out oc

end

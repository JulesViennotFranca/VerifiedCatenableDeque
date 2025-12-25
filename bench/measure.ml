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

  (** Remove the last character of a string. *)
  let remove_last_char str =
    let len = String.length str in
    String.sub str 0 (len - 1)

  (** Retrieve all computed data points. *)
  let get () =
    let res = Hashtbl.create 5 in
    (* List all the possible data files. *)
    let list_file =
      Sys.readdir "./bench/tmp" |>
      Array.to_list |>
      List.filter (fun x -> Filename.extension x = ".csv")
    in
    (* Read labels of a file. *)
    let read_labels line =
      let select i _ = i mod 2 = 0 in
      String.split_on_char ',' line |>
      List.filteri select |>
      List.map remove_last_char
    in
    (* Read a line of data. *)
    let read_data line =
      let datas = String.split_on_char ',' line in
      let rec aux accu = function
        | [] -> List.rev accu
        | [_] -> assert false
        | t :: n :: l -> aux (Data.of_string t n :: accu) l
      in
      aux [] datas
    in
    (* Read all data lines. *)
    let read_data_lines labels lines =
      let rec aux accu = function
        | [] -> List.map List.rev accu
        | line :: lines ->
          let data = read_data line in
          aux (List.map2 List.cons data accu) lines
      in
      let datas = aux (List.map (Fun.const []) labels) lines in
      List.map2 (fun l d -> (l, d)) labels datas
    in
    (* Make a hashtbl of data values. *)
    let make_hashtbl datas =
      let res = Hashtbl.create (List.length datas) in
      List.iter (fun (l, d) -> Hashtbl.add res l d) datas;
      res
    in
    (* Read a file. *)
    let read_file filename =
      let lines = ref [] in
      let chan = open_in ("./bench/tmp/" ^ filename) in
      begin try
        while true; do
          lines := input_line chan :: !lines
        done;
      with End_of_file -> close_in chan end;
      lines := List.rev !lines;
      let labels_line, rest_of_lines = List.hd !lines, List.tl !lines in
      let labels = read_labels labels_line in
      let datas = read_data_lines labels rest_of_lines in
      let datas = List.map (fun (l, d) -> (l, Array.of_list d)) datas in
      let name = Filename.remove_extension filename in
      let tbl = make_hashtbl datas in
      Hashtbl.add res name tbl
    in
    (* Read all files. *)
    List.iter read_file list_file;
    res

  (** Write data points to a file. *)
  let write operation data_structure datas =
    (* Merge all datas computed. *)
    let prev = get () in
    let tbl = match Hashtbl.find_opt prev operation with
      | None ->
        let tbl = Hashtbl.create 6 in
        Hashtbl.add prev operation tbl;
        tbl
      | Some tbl -> tbl
    in
    let prev_datas = match Hashtbl.find_opt tbl data_structure with
      | None ->
        let ar = Array.make (Array.length datas) Data.base in
        Hashtbl.add tbl data_structure ar;
        ar
      | Some ar -> ar
    in
    for i = 0 to Array.length datas - 1 do
      prev_datas.(i) <- Data.add prev_datas.(i) datas.(i)
    done;
    (* Write labels. *)
    let rec write_labels oc = function
      | [] -> ()
      | [lbl] -> fprintf oc "%s\n" (to_string_label lbl)
      | lbl :: labels ->
        fprintf oc "%s," (to_string_label lbl); write_labels oc labels
    in
    (* Write data lines. *)
    let rec write_datas oc = function
      | [] -> ()
      | [d] -> fprintf oc "%s\n" (Data.to_string d)
      | d :: datas ->
        fprintf oc "%s," (Data.to_string d); write_datas oc datas
    in
    (* Add a data structure to a list of data structures. *)
    let add_data_structure (names, datas) (name, data) =
      (name :: names, List.map2 List.cons (Array.to_list data) datas)
    in
    (* Write an operation's data. *)
    let write_operation name datas =
      let file = "bench/tmp/" ^ name ^ ".csv" in
      let oc = open_out file in
      let datas = List.of_seq (Hashtbl.to_seq datas) in
      let (name, data), datas = List.hd datas, List.tl datas in
      let data = List.map (fun d -> [d]) (Array.to_list data) in
      let (names, datas) =
        List.fold_left add_data_structure ([name], data) datas
      in
      write_labels oc names;
      List.iter (write_datas oc) datas;
      close_out oc
    in
    Hashtbl.iter write_operation prev
end

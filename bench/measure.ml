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

type t = float * int

let to_string_label lbl = sprintf "%sT,%sN" lbl lbl
let to_string_data (t, n) = sprintf "%f,%d" t n

let of_string t n = (float_of_string t, int_of_string n)

let base = (0., 0)

let format (f, n) = (1_000_000. *. f, n)

let add (f1, n1) (f2, n2) = (f1 +. f2, n1 + n2)

(* ========================== benchmark variables =========================== *)

let buffers = 21

let size = 10

(* ================================= steps ================================== *)

let steps = pow2 (buffers - 1)

let uconstant_steps _ = steps
let bconstant_steps _ _ = steps

let pos_length a = max 1 (WithLength.length a)

let ulinear_steps a = steps / (pos_length a)
let blinearmin_steps a b = steps / min (pos_length a) (pos_length b)
let blinearfst_steps a _ = steps / (pos_length a)

(* =============================== operations =============================== *)

let wrap_uop f steps a =
  try
    let steps = steps a in
    let t1 = Unix.gettimeofday () in
    for _ = 1 to steps do
      ignore (f a)
    done;
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, steps)
  with _ -> (0., 0)

let wrap_bop f steps a b =
  try
    let steps = steps a b in
    let t1 = Unix.gettimeofday () in
    for _ = 1 to steps do
      ignore (f a b)
    done;
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, steps)
  with _ -> (0., 0)

(* =========================== CSV files support ============================ *)

module CSV = struct

  (** Retrieve all datas already computed. *)
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
      tr_map remove_last_char
    in
    (* Read a line of data. *)
    let read_data line =
      let datas = String.split_on_char ',' line in
      let rec aux accu = function
        | [] -> List.rev accu
        | [_] -> assert false
        | t :: n :: l -> aux (of_string t n :: accu) l
      in
      aux [] datas
    in
    (* Read all data lines. *)
    let read_data_lines labels lines =
      let rec aux accu = function
        | [] -> tr_map List.rev accu
        | line :: lines ->
          let data = read_data line in
          aux (tr_map2 List.cons data accu) lines
      in
      let datas = aux (tr_map (Fun.const []) labels) lines in
      tr_map2 (fun l d -> (l, d)) labels datas
    in
    (* Make an hashtbl of data values. *)
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
      let datas = tr_map (fun (l, d) -> (l, Array.of_list d)) datas in
      let name = Filename.remove_extension filename in
      let tbl = make_hashtbl datas in
      Hashtbl.add res name tbl
    in
    (* Read all files. *)
    List.iter read_file list_file;
    res

  (** Write new datas. *)
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
        let ar = Array.make (Array.length datas) base in
        Hashtbl.add tbl data_structure ar;
        ar
      | Some ar -> ar
    in
    for i = 0 to Array.length datas - 1 do
      prev_datas.(i) <- add prev_datas.(i) datas.(i)
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
      | [d] -> fprintf oc "%s\n" (to_string_data d)
      | d :: datas ->
        fprintf oc "%s," (to_string_data d); write_datas oc datas
    in
    (* Add a data structure to a list of data structures. *)
    let add_data_structure (names, datas) (name, data) =
      (name :: names, tr_map2 List.cons (Array.to_list data) datas)
    in
    (* Write an operation datas. *)
    let write_operation name datas =
      let file = "bench/tmp/" ^ name ^ ".csv" in
      let oc = open_out file in
      let datas = List.of_seq (Hashtbl.to_seq datas) in
      let (name, data), datas = List.hd datas, List.tl datas in
      let data = tr_map (fun d -> [d]) (Array.to_list data) in
      let (names, datas) =
        List.fold_left add_data_structure ([name], data) datas
      in
      write_labels oc names;
      List.iter (write_datas oc) datas;
      close_out oc
    in
    Hashtbl.iter write_operation prev
end

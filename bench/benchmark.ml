open Printf

open Base

(* ================================== data ================================== *)

type data = float * int

let update_data (t, n) t0 t1 steps = (t +. (t1 -. t0) *. 1_000_000., n + steps)

let format_label lbl = sprintf "%sT,%sN" lbl lbl
let format_data (t, n) = sprintf "%f,%d" t n

let of_string t n = (float_of_string t, int_of_string n)


(* =============================== measuring ================================ *)

type (_, _) op_kind =
  | Unary : ('a, 'a) op_kind
  | Binary : ('a, 'a * 'a) op_kind

type ('a, 'i, 'o) wrapper_info = {
  operation : string ;              (* operation name *)
  f : 'i -> 'o ;                    (* operation body *)
  kind : ('a, 'i) op_kind ;         (* operation kind *)
  datastruct : string ;             (* datastructure name *)
  verify : 'i -> int -> unit ;      (* verification of the datastructure *)
  index : 'i -> data array -> int ; (* index in the data array *)
  steps : 'i -> int                 (* number of repetition of the operation *)
}



(** Remove the last character of a string. *)
let remove_last_char str =
  let len = String.length str in
  String.sub str 0 (len - 1)

(** This module collects execution metrics for various operations on data
    structures. *)
module Measure = struct

  (** The maximum size allowed for each data structure. *)
  let size = DataConstr.max_size + 1

  (** Tail-recursive map. *)
  let tr_map f l = List.rev (List.fold_left (fun accu x -> f x :: accu) [] l)

  (** Tail-recursive map2. *)
  let tr_map2 f l1 l2 =
    List.rev (List.fold_left2 (fun accu x1 x2 -> f x1 x2 :: accu) [] l1 l2)

  (** [info] stores information for operations.

      It is a hash table where keys are operation names and values are hash
      tables mapping data structure names to execution times, depending on the
      data structure’s size. *)
  let info =
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

  (** [find winfo] find the storred information about the operation
      [winfo.operation] and the datastructure [winfo.datastruct]. Allocate the
      space for this information if it's not yet created. *)
  let find
  : type a i o. (a, i, o) wrapper_info -> data array
  = fun winfo ->
    let tbl = match Hashtbl.find_opt info winfo.operation with
      | None ->
        let tbl = Hashtbl.create 6 in
        Hashtbl.add info winfo.operation tbl;
        tbl
      | Some tbl -> tbl
    in
    match Hashtbl.find_opt tbl winfo.datastruct with
    | None ->
      let ar = match winfo.kind with
        | Unary -> Array.make size (0., 0)
        | Binary -> Array.make (size * size) (0., 0)
      in
      Hashtbl.add tbl winfo.datastruct ar;
      ar
    | Some st -> st

  (** [wrap winfo] create a wrapper for the operation [winfo.operation] and the
      datastruct [winfo.datastruct].

      The returned function [wrapper x] returns [winfo.f x] while collecting information about [winfo.steps x] calls of [winfo.f] on [x]. *)
  let wrap winfo =
    let ar = find winfo in
    let wrapper x =
      try
        winfo.verify x DataConstr.max_size;
        let res = winfo.f x in
        let steps = winfo.steps x in
        let t0 = Unix.gettimeofday () in
        for _ = 1 to steps do
          ignore (winfo.f x)
        done;
        let t1 = Unix.gettimeofday () in
        let idx = winfo.index x ar in
        ar.(idx) <- update_data ar.(idx) t0 t1 steps;
        res
      with DataConstr.TooBig -> winfo.f x
    in
    wrapper

  (** Save all information. *)
  let save () =
    let rec write_labels oc = function
      | [] -> ()
      | [lbl] -> fprintf oc "%s\n" (format_label lbl)
      | lbl :: labels ->
        fprintf oc "%s," (format_label lbl); write_labels oc labels
    in
    let rec write_datas oc = function
      | [] -> ()
      | [d] -> fprintf oc "%s\n" (format_data d)
      | d :: datas ->
        fprintf oc "%s," (format_data d); write_datas oc datas
    in
    let add_datastruct (names, datas) (name, data) =
      (name :: names, tr_map2 List.cons (Array.to_list data) datas)
    in
    let save_operation name datas =
      let file = "bench/tmp/" ^ name ^ ".csv" in
      let oc = open_out file in
      let datas = List.of_seq (Hashtbl.to_seq datas) in
      let (name, data), datas = List.hd datas, List.tl datas in
      let data = tr_map (fun d -> [d]) (Array.to_list data) in
      let (names, datas) = List.fold_left add_datastruct ([name], data) datas in
      write_labels oc names;
      List.iter (write_datas oc) datas;
      close_out oc
    in
    Hashtbl.iter save_operation info

end

(* ============================== wrapper info ============================== *)

let unary_verify length x ms =
  begin if length x > ms then raise DataConstr.TooBig end
let binary_verify length (x, y) ms =
  begin if length x > ms || length y > ms then raise DataConstr.TooBig end

let unary_index length x _ = length x
let binary_index length (x, y) ar =
  let s = int_of_float (sqrt (float_of_int (Array.length ar))) in
  length x * s + length y

let ( / ) x y = if y = 0 then x else x / y
let const_unary_steps _ = DataConstr.max_size
let const_binary_steps (_, _) = DataConstr.max_size
let lin_unary_steps length x = DataConstr.max_size / length x
let lin_fst_binary_steps length (x, _) = DataConstr.max_size / length x
let lin_min_binary_steps length (x, y) =
  DataConstr.max_size / min (length x) (length y)

exception WrongMode

let make_const
: type a i o.
     string -> (i -> o) -> (a, i) op_kind
  -> string -> (a -> int)
  -> (a, i, o) wrapper_info
= fun operation f kind datastruct length ->
  let
    (verify : i -> int -> unit),
    (index : i -> data array -> int),
    (steps : i -> int)
    = match kind with
  | Unary -> (unary_verify length, unary_index length, const_unary_steps)
  | Binary -> (binary_verify length, binary_index length, const_binary_steps)
  in
  { operation; f; kind; datastruct; verify; index; steps }

let make_lin
: type a i o.
     ?bmode:string
  -> string -> (i -> o) -> (a, i) op_kind
  -> string -> (a -> int)
  -> (a, i, o) wrapper_info
= fun ?(bmode="min") operation f kind datastruct length ->
  let
    (verify : i -> int -> unit),
    (index : i -> data array -> int),
    (steps : i -> int)
    = match kind with
  | Unary -> (unary_verify length, unary_index length, lin_unary_steps length)
  | Binary -> (binary_verify length, binary_index length,
      if bmode = "fst" then lin_fst_binary_steps length
      else if bmode = "min" then lin_min_binary_steps length
      else raise WrongMode)
  in
  { operation; f; kind; datastruct; verify; index; steps }

(* ============================= datastructures ============================= *)

open Deques

let fignore f x = ignore (f x)

module ListDS = struct
  type t = int list * int

  let empty = ([], 0)

  let length = snd

  let push (l, n) = (0 :: l, n+1)
  let wpush = Measure.wrap (make_const "push" (fignore push) Unary "List" snd)

  let pop (l, n) = match l with
    | [] -> ([], 0)
    | _ :: l -> (l, n-1)
  let wpop = Measure.wrap (make_const "pop" (fignore pop) Unary "List" snd)

  let constr_operations = [(push, 2); (pop, 1)]

  let inject (l, _) = ignore (List.rev_append (List.rev l) [0])
  let winject = Measure.wrap (make_lin "inject" inject Unary "List" snd)

  let eject (l, _) =
    ignore (match l with [] -> [] | l -> List.rev (List.tl (List.rev l)))
  let weject = Measure.wrap (make_lin "eject" eject Unary "List" snd)

  let concat ((l1, _), (l2, _)) = ignore (List.append l1 l2)
  let wconcat =
    Measure.wrap (make_lin "concat" concat Binary "List" snd ~bmode:"fst")

  let revconcat ((l1, _), (l2, _)) = ignore (List.rev_append (List.rev l1) l2)
  let wrevconcat =
    Measure.wrap (make_lin "concat" revconcat Binary "ListRev" snd ~bmode:"fst")
end

module DequeDS = struct
  type t = int Deque.t

  let empty = Deque.empty

  let length = Deque.length

  let wpush =
    let push = Deque.push 0 in
    Measure.wrap (make_const "push" (fignore push) Unary "Deque" length)

  let pop d = if length d = 0 then empty else snd (Option.get (Deque.pop d))
  let wpop =
    Measure.wrap (make_const "pop" (fignore pop) Unary "Deque" length)

  let winject =
    let inject = Fun.flip Deque.inject 0 in
    Measure.wrap (make_const "inject" (fignore inject) Unary "Deque" length)

  let eject d = if length d = 0 then empty else fst (Option.get (Deque.eject d))
  let weject =
    Measure.wrap (make_const "eject" (fignore eject) Unary "Deque" length)

  let constr_operations =
    [(Deque.push 0, 2); (pop, 1); (Fun.flip Deque.inject 0, 2); (eject, 1)]

  let concat (d1, d2) = ignore (Deque.append d1 d2)
  let wconcat = Measure.wrap (make_lin "concat" concat Binary "Deque" length)
end

module StequeDS = struct
  type t = int Steque.t

  let empty = Steque.empty

  let length = Steque.length

  let wpush =
    let push = Steque.push 0 in
    Measure.wrap (make_const "push" (fignore push) Unary "Steque" length)

  let pop s = if length s = 0 then empty else snd (Option.get (Steque.pop s))
  let wpop =
    Measure.wrap (make_const "pop" (fignore pop) Unary "Steque" length)

  let winject =
    let inject = Fun.flip Steque.inject 0 in
    Measure.wrap (make_const "inject" (fignore inject) Unary "Steque" length)

  let constr_operations =
    [(Steque.push 0, 2); (pop, 2); (Fun.flip Steque.inject 0, 2)]

  let eject s = ignore (
      if Steque.length s = 0 then empty
      else Steque.rev (Steque.tl (Steque.rev s))
    )
  let weject = Measure.wrap (make_lin "eject" eject Unary "Steque" length)

  let concat (s1, s2) = ignore (Steque.append s1 s2)
  let wconcat =
    Measure.wrap (make_lin "concat" concat Binary "Steque" length)
end

module CadequeDS = struct
  type t = int Cadeque.t

  let empty = Cadeque.empty

  let length = Cadeque.length

  let wpush =
    let push = Cadeque.push 0 in
    Measure.wrap (make_const "push" (fignore push) Unary "Steque" length)

  let pop c = if length c = 0 then empty else snd (Option.get (Cadeque.pop c))
  let wpop =
    Measure.wrap (make_const "pop" (fignore pop) Unary "Cadeque" length)

  let winject =
    let inject = Fun.flip Cadeque.inject 0 in
    Measure.wrap (make_const "inject" (fignore inject) Unary "Cadeque" length)

  let eject c =
    if length c = 0 then empty else fst (Option.get (Cadeque.eject c))
  let weject =
    Measure.wrap (make_const "eject" (fignore eject) Unary "Cadeque" length)

  let constr_operations =
    [(Cadeque.push 0, 2); (pop, 1); (Fun.flip Cadeque.inject 0, 2); (eject, 1)]

  let concat (c1, c2) = ignore (Cadeque.append c1 c2)
  let wconcat =
    Measure.wrap (make_const "concat" concat Binary "Cadeque" length)
end


(* =============================== benchmarks =============================== *)

let () =
  print_endline "=============== Benchmarks ===============\n";

  print_endline "-------- List ---------";
  print_endline "Constructing database ...";
  let ldb = run (module ListDS) ~parallel:1 in
  print_endline "Done.";
  print_endline "Benchmark on push ...";
  DataBase.uiter ListDS.wpush ldb;
  print_endline "Done.";
  print_endline "Benchmark on pop ...";
  DataBase.uiter ListDS.wpop ldb;
  print_endline "Done.";
  print_endline "Benchmark on inject ...";
  DataBase.uiter ListDS.winject ldb;
  print_endline "Done.";
  print_endline "Benchmark on eject ...";
  DataBase.uiter ListDS.weject ldb;
  print_endline "Done.";
  print_endline "Benchmark on concat ...";
  DataBase.biter ListDS.wconcat 0.1 ldb;
  print_endline "Done.";
  print_endline "Benchmark on rev concat ...";
  DataBase.biter ListDS.wrevconcat 0.1 ldb;
  print_endline "Done.";
  print_endline "-----------------------\n";

  print_endline "-------- Deque --------";
  print_endline "Constructing database ...";
  let ddb = run (module DequeDS) ~parallel:1 in
  print_endline "Done.";
  print_endline "Benchmark on push ...";
  DataBase.uiter DequeDS.wpush ddb;
  print_endline "Done.";
  print_endline "Benchmark on pop ...";
  DataBase.uiter DequeDS.wpop ddb;
  print_endline "Done.";
  print_endline "Benchmark on inject ...";
  DataBase.uiter DequeDS.winject ddb;
  print_endline "Done.";
  print_endline "Benchmark on eject ...";
  DataBase.uiter DequeDS.weject ddb;
  print_endline "Done.";
  print_endline "Benchmark on concat ...";
  DataBase.biter DequeDS.wconcat 0.1 ddb;
  print_endline "Done.";
  print_endline "-----------------------\n";

  print_endline "------- Steque --------";
  print_endline "Constructing database ...";
  let sdb = run (module StequeDS) ~parallel:1 in
  print_endline "Done.";
  print_endline "Benchmark on push ...";
  DataBase.uiter StequeDS.wpush sdb;
  print_endline "Done.";
  print_endline "Benchmark on pop ...";
  DataBase.uiter StequeDS.wpop sdb;
  print_endline "Done.";
  print_endline "Benchmark on inject ...";
  DataBase.uiter StequeDS.winject sdb;
  print_endline "Done.";
  print_endline "Benchmark on eject ...";
  DataBase.uiter StequeDS.weject sdb;
  print_endline "Done.";
  print_endline "Benchmark on concat ...";
  DataBase.biter StequeDS.wconcat 0.1 sdb;
  print_endline "Done.";
  print_endline "-----------------------\n";

  print_endline "------- Cadeque -------";
  print_endline "Constructing database ...";
  let cdb = run (module CadequeDS) ~parallel:1 in
  print_endline "Done.";
  print_endline "Benchmark on push ...";
  DataBase.uiter CadequeDS.wpush cdb;
  print_endline "Done.";
  print_endline "Benchmark on pop ...";
  DataBase.uiter CadequeDS.wpop cdb;
  print_endline "Done.";
  print_endline "Benchmark on inject ...";
  DataBase.uiter CadequeDS.winject cdb;
  print_endline "Done.";
  print_endline "Benchmark on eject ...";
  DataBase.uiter CadequeDS.weject cdb;
  print_endline "Done.";
  print_endline "Benchmark on concat ...";
  DataBase.biter CadequeDS.wconcat 0.1 cdb;
  print_endline "Done.";
  print_endline "-----------------------\n";

  print_endline "Saving ...";
  Measure.save ();
  print_endline "Done."

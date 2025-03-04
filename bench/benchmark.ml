open Printf
open Sys
open Filename

open Base

let remove_last_char str =
  let len = String.length str in
  String.sub str 0 (len - 1)

module Measure = struct
  type s = string * int

  type ut = { time : float ; info : s }
  type bt = { time : float ; info1 : s ; info2 : s }

  let max_size = 2000

  exception TooBig

  let get_name : s -> string = fst
  let get_size : s -> int = snd

  let verify (m : 'a -> s) (a : 'a) : unit =
    if get_size (m a) >= max_size then raise TooBig

  let uget measure a =
    let info = measure a in
    let time = Unix.gettimeofday () in
    { time ; info }

  let utimes =
    let res = Hashtbl.create 4 in
    (* List all the possible data files. *)
    let list_file =
      Sys.readdir "./tmp" |>
      Array.to_list |>
      List.filter (fun x -> Filename.extension x = ".csv")
    in
    (* Read labels of a file. *)
    let read_labels line =
      let rec aux accu = function
        | [] | [_] -> accu
        | _ :: st :: l -> aux (remove_last_char st :: accu) l
      in
      aux [] (String.split_on_char ',' line)
    in
    (* Read a line of data. *)
    let read_data line =
      let datas = String.split_on_char ',' line in
      let rec aux accu = function
        | [] -> accu
        | [_] -> assert false
        | t :: n :: l -> aux ((float_of_string t, int_of_string n) :: accu) l
      in
      aux [] (List.rev datas)
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
    (* Make an hashtbl of data values. *)
    let make_hashtbl datas =
      let res = Hashtbl.create (List.length datas) in
      if List.length (snd (List.hd datas)) > max_size then begin
        List.iter (fun (l, d) -> Hashtbl.add res l (Array.of_list d)) datas;
        res
      end else
        let make_array d =
          let a = Array.make max_size (0., 0) in
          List.iteri (fun i tn -> a.(i) <- tn) d;
          a
        in
        List.iter (fun (l, d) -> Hashtbl.add res l (make_array d)) datas;
        res
    in
    (* Read a file. *)
    let read_file filename =
      let lines = ref [] in
      let chan = open_in ("./tmp/" ^ filename) in
      begin try
        while true; do
          lines := input_line chan :: !lines
        done;
      with End_of_file ->
        close_in chan
      end;
      let labels_line, rest_of_lines = match List.rev !lines with
        | [] -> assert false
        | tl :: rol -> tl, rol
      in
      let labels = read_labels labels_line in
      let datas = read_data_lines labels rest_of_lines in
      (Filename.remove_extension filename, make_hashtbl datas)
    in
    (* Read all files. *)
    let hashtbl_file = List.map read_file list_file in
    List.iter (fun (l, d) -> Hashtbl.add res l d) hashtbl_file;
    res

  let utimes_update name steps (m1 : ut) (m2 : ut) =
    let ds_name = get_name m2.info in
    let tbl = match Hashtbl.find_opt utimes name with
      | None -> Hashtbl.create 6
      | Some tbl -> tbl
    in
    let ar = match Hashtbl.find_opt tbl ds_name with
      | None -> Array.make max_size (0., 0)
      | Some ar -> ar
    in
    let (t, n) = ar.(get_size m1.info) in
    ar.(get_size m1.info) <- (t +. m2.time -. m1.time, n + steps);
    Hashtbl.replace tbl ds_name ar;
    Hashtbl.replace utimes name tbl

  let uupdate name steps m1 m2 =
    utimes_update name steps m1 m2

  let wrap_uop name f m steps a =
    verify m a;
    let new_a = f a in
    let steps = steps (m a) in
    let m1 = uget m a in
    for _ = 1 to steps do
      ignore (f a)
    done;
    let m2 = uget m new_a in
    uupdate name steps m1 m2;
    new_a

  let bget measure a b =
    let info1 = measure a in
    let info2 = measure b in
    let time = Unix.gettimeofday () in
    { time ; info1 ; info2 }

  let btimes = Hashtbl.create 1

  let btimes_update name steps (m1 : bt) (m2 : ut) =
    let ds_name = get_name m2.info in
    let tbl = match Hashtbl.find_opt btimes name with
      | None -> Hashtbl.create 6
      | Some tbl -> tbl
    in
    let ar = match Hashtbl.find_opt tbl ds_name with
      | None -> Array.init max_size (fun _ -> Array.make max_size (0., 0))
      | Some ar -> ar
    in
    let (t, n) = ar.(get_size m1.info1).(get_size m1.info2) in
    ar.(get_size m1.info1).(get_size m1.info2)
      <- (t +. m2.time -. m1.time, n + steps);
    Hashtbl.replace tbl ds_name ar;
    Hashtbl.replace btimes name tbl

  let bupdate name steps m1 m2 =
    btimes_update name steps m1 m2

  let wrap_bop name f m steps a b =
    verify m a;
    verify m b;
    let new_a = f a b in
    let steps = steps (m a) (m b) in
    let m1 = bget m a b in
    for _ = 1 to steps do
      ignore (f a b)
    done;
    let m2 = uget m new_a in
    bupdate name steps m1 m2;
    new_a

  let utimes_save () =
    let rec write_labels oc = function
      | [] -> ()
      | [s] -> fprintf oc "%sT,%sN\n" s s
      | s :: labels ->
        fprintf oc "%sT,%sN" s s; write_labels oc labels
    in
    let rec write_line oc = function
      | [] -> ()
      | [(t, n)] -> fprintf oc "%f,%d\n" t n
      | (t, n) :: line -> fprintf oc "%f,%d," t n; write_line oc line
    in
    let add_datastruct (names, datas) (name, data) =
      for k = 0 to Array.length datas - 1 do
        datas.(k) <- data.(k) :: datas.(k)
      done;
      (name :: names, datas)
    in
    let save_op (name, data) =
      let file = "bench/tmp/" ^ name ^ ".csv" in
      let oc = open_out file in
      let data = Hashtbl.to_seq data in
      let ar = Array.make max_size [] in
      let (names, datas) = Seq.fold_left add_datastruct ([], ar) data in
      fprintf oc "Size,";
      write_labels oc names;
      Array.iteri (fun i l -> fprintf oc "%d," i; write_line oc l) datas;
      close_out oc
    in
    let ops = Hashtbl.to_seq utimes in
    Seq.iter save_op ops

  let usave () =
    utimes_save ()

  let btimes_save () =
    let rec write_labels oc = function
      | [] -> ()
      | [s] -> fprintf oc "%sT,%sN\n" s s
      | s :: labels ->
        fprintf oc "%sT,%sN," s s; write_labels oc labels
    in
    let rec write_line oc = function
      | [] -> ()
      | [(t, n)] -> fprintf oc "%f,%d\n" t n
      | (t, n) :: line -> fprintf oc "%f,%d," t n; write_line oc line
    in
    let add_datastruct (names, datas) (name, data) =
      for k = 0 to Array.length data - 1 do
        for l = 0 to Array.length data.(k) - 1 do
          datas.(k+l) <- data.(k).(l) :: datas.(k+l)
        done;
      done;
      (name :: names, datas)
    in
    let save_op (name, data) =
      let file = "bench/tmp/" ^ name ^ ".csv" in
      let oc = open_out file in
      let data = Hashtbl.to_seq data in
      let ar = Array.make (max_size * max_size) [] in
      let (names, datas) = Seq.fold_left add_datastruct ([], ar) data in
      fprintf oc "Size1,Size2,";
      write_labels oc names;
      Array.iteri
        (fun i l -> fprintf oc "%d,%d," (i / max_size) (i mod max_size);
                    write_line oc l)
        datas;
      close_out oc
    in
    let ops = Hashtbl.to_seq btimes in
    Seq.iter save_op ops

  let bsave () =
    btimes_save ()
end

let ref_steps = 1000
let const1_steps _ = ref_steps
(* let const2_steps _ _ = ref_steps *)

module ListDS = struct
  type t = int list * int

  let empty = ([], 0)

  type measure = Measure.s
  let measure l = ("List", snd l)

  let push (l, n) = (0 :: l, n+1)

  let pop (l, n) = match l with
    | [] -> ([], 0)
    | _ :: l -> (l, n-1)

  let inject (l, n) =
    let rec aux = function
      | [] -> [0]
      | x :: l -> x :: aux l
    in (aux l, n+1)
  let wraped_inject l =
    let steps m = ref_steps / (Measure.get_size m) in
    try ignore (Measure.wrap_uop "inject" inject measure steps l)
    with _ -> ()

  let rec eject (l, n) = match l with
    | [] -> ([], 0)
    | [0] -> ([], 0)
    | x :: l -> let l, _ = eject (l, 0) in (x :: l, n-1)
  let wraped_eject l =
    let steps m = ref_steps / (Measure.get_size m) in
    try ignore (Measure.wrap_uop "eject" eject measure steps l)
    with _ -> ()

  let unary_operations = [
      ("push", push, const1_steps, 2);
      ("pop", pop, const1_steps, 1)
    ]

  let concat (l1, n1) (l2, n2) = (List.append l1 l2, n1 + n2)
  let wraped_concat l1 l2 =
    let steps m1 _ = ref_steps / (Measure.get_size m1) in
    try ignore (Measure.wrap_bop "concat" concat measure steps l1 l2)
    with _ -> ()

  let rev_concat (l1, n1) (l2, n2) = (List.rev_append (List.rev l1) l2, n1 + n2)
  let wraped_revconcat l1 l2 =
    let measure l = ("ListRev", snd (measure l)) in
    let steps m1 _ = ref_steps / (Measure.get_size m1) in
    try ignore (Measure.wrap_bop "concat" rev_concat measure steps l1 l2)
    with _ -> ()

  let binary_operations = []
end

module DequeDS = struct
  type t = int Cadeque.Deque.t

  let empty = Cadeque.Deque.empty

  type measure = Measure.s
  let measure d = ("Deque", Cadeque.Deque.length d)

  let pop d = match Cadeque.Deque.pop d with
    | None -> Cadeque.Deque.empty
    | Some (_, d) -> d

  let eject d = match Cadeque.Deque.eject d with
    | None -> Cadeque.Deque.empty
    | Some (d, _) -> d

  let unary_operations = [
      ("push", (Cadeque.Deque.push 0), const1_steps, 2);
      ("pop", pop, const1_steps, 1);
      ("inject", (Fun.flip Cadeque.Deque.inject 0), const1_steps, 2);
      ("eject", eject, const1_steps, 1)
    ]

  let wraped_concat l1 l2 =
    let steps m1 m2 =
      ref_steps / min (Measure.get_size m1) (Measure.get_size m2)
    in
    try ignore (
      Measure.wrap_bop "concat" Cadeque.Deque.append measure steps l1 l2
    )
  with _ -> ()

  let binary_operations = []
end

(* module StequeDS = struct
  type t = int Cadeque.Steque.t

  let empty = Cadeque.Steque.empty

  type measure = Measure.s
  let measure s = ("Steque", Cadeque.Steque.length s)

  let pop d = match Cadeque.Steque.pop d with
    | None -> Cadeque.Steque.empty
    | Some (_, d) -> d

  let unary_operations = [
      ("push", (Cadeque.Steque.push 0), const1_steps, 2);
      ("pop", pop, const1_steps, 2);
      ("inject", (Fun.flip Cadeque.Steque.inject 0), const1_steps, 2)
    ]

  let binary_operations = [("concat", Cadeque.Steque.append, const2_steps, 1)]
end *)

(* module CadequeDS = struct
  type t = int Cadeque.t

  let empty = Cadeque.empty

  type measure = Measure.s
  let measure c = ("Cadeque", Cadeque.length c)

  let pop d = match Cadeque.pop d with
    | None -> Cadeque.empty
    | Some (_, d) -> d

  let eject d = match Cadeque.eject d with
    | None -> Cadeque.empty
    | Some (d, _) -> d

  let unary_operations = [
      ("push", (Cadeque.push 0), const1_steps, 2);
      ("pop", pop, const1_steps, 1);
      ("inject", (Fun.flip Cadeque.inject 0), const1_steps, 2);
      ("eject", eject, const1_steps, 1)
    ]

  let binary_operations = [("concat", Cadeque.append, const2_steps, 1)]
end *)

(* module CadequeDS2 = struct
  type t = int Cadeque.Cadeque2.t

  let empty = Cadeque.Cadeque2.empty

  type measure = Measure.s
  let measure c = ("Cadeque2", Cadeque.Cadeque2.length c)

  let pop d = match Cadeque.Cadeque2.pop d with
    | None -> Cadeque.Cadeque2.empty
    | Some (_, d) -> d

  let eject d = match Cadeque.Cadeque2.eject d with
    | None -> Cadeque.Cadeque2.empty
    | Some (d, _) -> d

  let unary_operations = [
      ("push", (Cadeque.Cadeque2.push 0), const1_steps, 2);
      ("pop", pop, const1_steps, 1);
      ("inject", (Fun.flip Cadeque.Cadeque2.inject 0), const1_steps, 2);
      ("eject", eject, const1_steps, 1)
    ]

  let binary_operations = [("concat", Cadeque.Cadeque2.append, const2_steps, 1)]
end *)

let () =
  print_endline "=============== Benchmarks ===============";
  let ldb = run (module Measure) (module ListDS) 20 in
  Database.uiter ListDS.wraped_inject ldb;
  Database.uiter ListDS.wraped_eject ldb;
  Database.biter ListDS.wraped_concat ldb;
  Database.biter ListDS.wraped_revconcat ldb;
  print_endline "List completed";
  let ddb = run (module Measure) (module DequeDS) 80 in
  Database.biter DequeDS.wraped_concat ddb;
  print_endline "Deque completed";
  (* ignore (run (module Measure) (module StequeDS) 2000);
  print_endline "Steque completed";
  ignore (run (module Measure) (module CadequeDS) 4000);
  print_endline "Cadeque completed";
  ignore (run (module Measure) (module CadequeDS2) 4000);
  print_endline "Cadeque2 completed"; *)
  Measure.usave ();
  Measure.bsave ()

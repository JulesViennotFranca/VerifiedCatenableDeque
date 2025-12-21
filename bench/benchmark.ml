open Database
open Measure

(* ============================ data structures ============================= *)

open Deques
open Sek

module type Structure  = sig
  type t

  val name : string

  val empty : t
  val push : t -> t
  val push_steps : int -> int
  val pop : t -> t
  val pop_steps : int -> int
  val inject : t -> t
  val inject_steps : int -> int
  val eject : t -> t
  val eject_steps : int -> int
  val concat : t -> t -> t
  val concat_steps : int -> int -> int

  val to_string : t -> string
end

let string_of_list l = "[" ^ String.concat ", " (List.map string_of_int l) ^ "]"

module BList : Structure = struct
  type t = int list

  let name = "List"

  let empty = []
  let push = List.cons 0
  let push_steps = uconstant_steps
  let pop = function [] -> [] | _ :: l -> l
  let pop_steps = uconstant_steps
  let inject l = List.rev (0 :: List.rev l)
  let inject_steps = ulinear_steps
  let eject l = match List.rev l with [] -> [] | _ :: l -> List.rev l
  let eject_steps = ulinear_steps
  let concat l1 l2 = List.rev_append (List.rev l1) l2
  let concat_steps = blinearfst_steps

  let to_string = string_of_list
end

module BSek : Structure = struct
  type t = int P.t

  let name = "Sek"

  let empty = P.create 0
  let push s = P.push front s 0
  let push_steps = uconstant_steps
  let pop s = snd (P.pop_opt front s)
  let pop_steps = uconstant_steps
  let inject s = P.push back s 0
  let inject_steps = uconstant_steps
  let eject s = snd (P.pop_opt back s)
  let eject_steps = uconstant_steps
  let concat = P.concat
  let concat_steps = bconstant_steps

  let to_string s = string_of_list (P.to_list s)
end

module BDeque : Structure = struct
  type t = int Deque.t

  let name = "Deque"

  let empty = Deque.empty
  let push = Deque.push 0
  let push_steps = uconstant_steps
  let pop d = match Deque.pop d with
    | None -> Deque.empty
    | Some (_, d) -> d
  let pop_steps = uconstant_steps
  let inject d = Deque.inject d 0
  let inject_steps = uconstant_steps
  let eject d = match Deque.eject d with
    | None -> Deque.empty
    | Some (d, _) -> d
  let eject_steps = uconstant_steps
  let concat = Deque.append
  let concat_steps = blinearmin_steps

  let to_string s = string_of_list (Deque.to_list s)
end

module BSteque : Structure = struct
  type t = int Steque.t

  let name = "Steque"

  let empty = Steque.empty
  let push = Steque.push 0
  let push_steps = uconstant_steps
  let pop s = match Steque.pop s with
    | None -> Steque.empty
    | Some (_, s) -> s
  let pop_steps = uconstant_steps
  let inject s = Steque.inject s 0
  let inject_steps = uconstant_steps
  let eject s = Steque.rev (pop (Steque.rev s))
  let eject_steps = ulinear_steps
  let concat = Steque.append
  let concat_steps = blinearmin_steps

  let to_string s = string_of_list (Steque.to_list s)
end

module BCadeque : Structure = struct
  type t = int Cadeque.t

  let name = "Cadeque"

  let empty = Cadeque.empty
  let push = Cadeque.push 0
  let push_steps = uconstant_steps
  let pop d = match Cadeque.pop d with
    | None -> Cadeque.empty
    | Some (_, d) -> d
  let pop_steps = uconstant_steps
  let inject d = Cadeque.inject d 0
  let inject_steps = uconstant_steps
  let eject d = match Cadeque.eject d with
    | None -> Cadeque.empty
    | Some (d, _) -> d
  let eject_steps = uconstant_steps
  let concat = Cadeque.append
  let concat_steps = bconstant_steps

  let to_string s = string_of_list (Cadeque.to_list s)
end

(* ================================ database ================================ *)

module PQ = struct
  (* This requires OCaml 5.4. *)
  include Pqueue.MakeMin(struct
    type t = int * operation
    let compare (j1, _op1) (j2, _op2) = Int.compare j1 j2
  end)
  let add_list q xs =
    add_iter q List.iter xs
end

let construct :
type a. Database.raw_t -> (module Structure with type t = a) -> a Database.t
= fun rdb (module S) ->
  let pb = progress_bar "Database construction" (rdb.elements.length - 1) in
  let idx = ref 1 in
  let aelements = Array.make rdb.elements.length None in
  let get i = Option.get aelements.(i)
  and set i x = aelements.(i) <- Some x
  and is_unset i = Option.is_none aelements.(i) in
  set 0 S.empty;
  let q = PQ.create() in
  PQ.add_list q rdb.traces.(0);
  while not (PQ.is_empty q) do
    let (j, op) = Option.get (PQ.pop_min q) in
    if is_unset j then
      let elem = match op with
        | Push i -> S.push (get i)
        | Pop i -> S.pop (get i)
        | Inject i -> S.inject (get i)
        | Eject i -> S.eject (get i)
        | Concat (i1, i2) -> S.concat (get i1) (get i2)
      in
      set j elem;
      PQ.add_list q rdb.traces.(j);
      idx := !idx + 1;
      pb !idx
  done;
  assert (Array.for_all Option.is_some aelements);
  let elements = Slice.create ~size:(rdb.elements.length) ~dummy:S.empty in
  Array.iter (fun oe -> Slice.add elements (Option.get oe)) aelements;
  {elements; ranges = rdb.ranges; traces = rdb.traces}

(* =============================== benchmarks =============================== *)

let with_length rdb db i = (Slice.get db.elements i, Slice.get rdb.elements i)

let bench_unary rdb db operation_name structure_name f steps =
  let datas = Array.make (Array.length db.ranges) Measure.base in
  let f = Measure.wrap_uop f steps in
  let pb = progress_bar operation_name db.elements.length in
  let idx = ref 0 in
  for i = 0 to Array.length db.ranges - 1 do
    let f j =
      datas.(i) <-
        Measure.add datas.(i) (Measure.format (f (with_length rdb db j)));
      idx := !idx + 1;
      pb !idx
    in
    let s = snd db.ranges.(i) in
    Slice.iter f s
  done;
  CSV.write operation_name structure_name datas

let bench_binary rdb db operation_name structure_name f steps =
  let len = Array.length db.ranges in
  let datas = Array.make (len * len) Measure.base in
  let f = Measure.wrap_bop f steps in
  let pb =
    progress_bar operation_name (db.elements.length * db.elements.length) in
  let idx = ref 0 in
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      let k = i * len + j in
      let f ix iy =
        if Random.int (len * len) < len then begin
          let x = with_length rdb db ix in
          let y = with_length rdb db iy in
          datas.(k) <- Measure.add datas.(k) (Measure.format (f x y)) end;
        idx := !idx + 1;
        pb !idx
      in
      let s1 = snd db.ranges.(i) in
      let s2 = snd db.ranges.(j) in
      Slice.iter2 f s1 s2
    done;
  done;
  CSV.write operation_name structure_name datas

let bench_traces :
type a. raw_t -> a Database.t -> (module Structure with type t = a) -> unit
= fun rdb db (module S) ->
  let datas = Array.make db.elements.length None in
  datas.(0) <- Some Measure.base;
  let pb = progress_bar "traces" db.elements.length in
  let idx = ref 1 in
  let q = PQ.create() in
  PQ.add_list q db.traces.(0);
  while not (PQ.is_empty q) do
    let (j, op) = Option.get (PQ.pop_min q) in
    if Option.is_none datas.(j) then begin
      let d = match op with
        | Push i ->
          let id = Option.get datas.(i) in
          let m = wrap_uop S.push (Fun.const 1) (with_length rdb db i) in
          Measure.add id m
        | Pop i ->
          let id = Option.get datas.(i) in
          let m = wrap_uop S.pop (Fun.const 1) (with_length rdb db i) in
          Measure.add id m
        | Inject i ->
          let id = Option.get datas.(i) in
          let m = wrap_uop S.inject (Fun.const 1) (with_length rdb db i)
          in
          Measure.add id m
        | Eject i ->
          let id = Option.get datas.(i) in
          let m = wrap_uop S.eject (Fun.const 1) (with_length rdb db i) in
          Measure.add id m
        | Concat (i1, i2) ->
          let i1d = Option.get datas.(i1) in
          let i2d = Option.get datas.(i2) in
          let x1 = with_length rdb db i1 in
          let x2 = with_length rdb db i2 in
          let m = wrap_bop S.concat (Fun.const @@ Fun.const 1) x1 x2 in
          Measure.add i1d (Measure.add i2d m)
      in
      datas.(j) <- Some d;
      PQ.add_list q rdb.traces.(j);
      idx := !idx + 1;
      pb !idx
    end
  done;
  assert (Array.for_all Option.is_some datas);
  let datas = Array.map (fun (_, s) ->
    Slice.to_list s |>
    List.map (fun i -> Option.get datas.(i)) |>
    List.map (fun d -> (Measure.get_time d, 1)) |>
    List.fold_left Measure.add Measure.base
  ) db.ranges
  in
  CSV.write "traces" S.name datas

let bench rdb (module S : Structure) =
  (* TODO: set random seed at the beginning with the time of the day *)
  print_endline ("==================== " ^ S.name ^ " ====================");
  let db = construct rdb (module S) in
  bench_unary rdb db "push" S.name S.push S.push_steps;
  bench_unary rdb db "pop" S.name S.pop S.pop_steps;
  bench_unary rdb db "inject" S.name S.inject S.inject_steps;
  bench_unary rdb db "eject" S.name S.eject S.eject_steps;
  bench_binary rdb db "concat" S.name S.concat S.concat_steps;
  bench_traces rdb db (module S);
  ()

let () =
  let rdb = raw_construct ~buffers ~size in
  bench rdb (module BList);
  bench rdb (module BSek);
  bench rdb (module BDeque);
  bench rdb (module BSteque);
  bench rdb (module BCadeque);
  ()

let valid_operations ops =
  let sum_prob = Array.fold_left (fun total (_, _, p) -> total + p) 0 ops in
  assert (sum_prob = 100)

module type BenchIn = sig
  type t

  val empty : t
  val length : t -> int
  val operations : (string * (t -> t) * int) array

  val name : string
  val color : string
end

module type Info = sig
  val max_size : int
  val steps : int
end

module type BenchOut = sig
  type t

  val time_per_op_per_size : (string, (int * float) array) Hashtbl.t

  val operations : (t -> t) array
  val run : unit -> unit
  val data : string -> (string * string * float array)
end

module Make (B : BenchIn) (I : Info) : BenchOut = struct
  type t = B.t

  let time_per_op_per_size =
    let init_times_op () = Array.make I.max_size (0, 0.) in
    let res = Hashtbl.create (Array.length B.operations) in
    let adding (name, _, _) = Hashtbl.add res name (init_times_op ()) in
    Array.iter adding B.operations;
    res

  let make_operation (name, op, prob) k ar =
    let new_op t =
      let t0 = Unix.gettimeofday () in
      let res = op t in
      let t1 = Unix.gettimeofday () in
      let times = Hashtbl.find time_per_op_per_size name in
      let n, tt = times.(B.length t) in
      times.(B.length t) <- (n + 1, tt +. t1 -. t0);
      Hashtbl.replace time_per_op_per_size name times;
      res
    in
    for i = 0 to prob - 1 do
      ar.(k + i) <- new_op
    done

  let operations =
    valid_operations B.operations;
    let res = Array.make 100 Fun.id in
    let accu = ref 0 in
    for idx = 0 to Array.length B.operations - 1 do
      let (name, op, prob) = B.operations.(idx) in
      make_operation (name, op, prob) !accu res;
      accu := !accu + prob;
    done;
    res

  let run () =
    for _ = 1 to I.steps do
      let t = ref B.empty in
      while B.length !t < I.max_size do
        let idx = Random.int 100 in
        let op = operations.(idx) in
        t := op !t
      done;
    done

  let data name =
    let data = Hashtbl.find time_per_op_per_size name in
    (B.name, B.color, Array.map (fun (n, t) -> t /. float_of_int n) data)
end

module L : BenchIn = struct
  type t = int list

  let empty = []
  let length = List.length

  let tl = function
    | [] -> []
    | _ :: tl -> tl

  let operations = [|("push", List.cons 0, 66); ("pop", tl, 34)|]

  let name = "List"
  let color = "#DB3AE0"
end

module Deque : BenchIn = struct
  type t = int Cadeque.Deque.t

  let empty = Cadeque.Deque.empty
  let length = Cadeque.Deque.length

  let pop d = match Cadeque.Deque.pop d with
    | None -> empty
    | Some (_, d) -> d

  let eject d = match Cadeque.Deque.eject d with
    | None -> empty
    | Some (d, _) -> d

  let operations = [|
    ("push", Cadeque.Deque.push 0, 33);
    ("inject", Fun.flip Cadeque.Deque.inject 0, 33);
    ("pop", pop, 17);
    ("eject", eject, 17)
  |]

  let name = "Deque"
  let color = "#F72500"
end

module Steque : BenchIn = struct
  type t = int Cadeque.Steque.t

  let empty = Cadeque.Steque.empty
  let length = Cadeque.Steque.length

  let pop d = match Cadeque.Steque.pop d with
    | None -> empty
    | Some (_, d) -> d

  let operations = [|
    ("push", Cadeque.Steque.push 0, 27);
    ("inject", Fun.flip Cadeque.Steque.inject 0, 27);
    ("pop", pop, 34);
    ("concat", (fun st -> Cadeque.Steque.append st st), 12)
  |]

  let name = "Steque"
  let color = "#BCC20A"
end

module Cadeque : BenchIn = struct
  type t = int Cadeque.t

  let empty = Cadeque.empty
  let length = Cadeque.length

  let pop d = match Cadeque.pop d with
    | None -> empty
    | Some (_, d) -> d

  let eject d = match Cadeque.eject d with
    | None -> empty
    | Some (d, _) -> d

  let operations = [|
    ("push", Cadeque.push 0, 27);
    ("inject", Fun.flip Cadeque.inject 0, 27);
    ("pop", pop, 17);
    ("eject", eject, 17);
    ("concat", (fun d -> Cadeque.append d d), 12)
  |]

  let name = "Cadeque"
  let color = "#5C7AFF"
end

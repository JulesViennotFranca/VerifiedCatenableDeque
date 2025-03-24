open Database
open Measure

(* =============================== benchmarks =============================== *)

let bench_unary db operation data_structure f steps =
  let datas = Array.make (Array.length db) Measure.base in
  let f (a, _) = (f a, 0) in
  let f = Measure.wrap_uop f steps in
  let pb = progress_bar operation (Array.length db) in
  for i = 0 to Array.length db - 1 do
    let f a = datas.(i) <- Measure.add datas.(i) (Measure.format (f a)) in
    let s = get db i in
    Slice.iter f s;
    pb (i + 1)
  done;
  CSV.write operation data_structure datas

let bench_binary db operation data_structure f steps =
  let len = Array.length db in
  let datas = Array.make (len * len) Measure.base in
  let f (a, _) (b, _) = (f a b, 0) in
  let f = Measure.wrap_bop f steps in
  let pb = progress_bar operation (len * len) in
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      let k = i * len + j in
      let f a b = datas.(k) <- Measure.add datas.(k) (Measure.format (f a b)) in
      let s1 = get db i in
      let s2 = get db j in
      Slice.iter2 f s1 s2;
      pb (k + 1)
    done;
  done;
  CSV.write operation data_structure datas

(* ============================ data structures ============================= *)

open Deques
open Sek

module List = struct
  let make n = fold_left (List.cons 0) [] n
  let push = List.cons 0
  let pop = function
    | [] -> []
    | _ :: l -> l
  let inject l = List.rev (0 :: List.rev l)
  let eject l = match List.rev l with
    | [] -> []
    | _ :: l -> List.rev l
  let concat = List.append
  let rev_concat l1 l2 = List.rev_append (List.rev l1) l2

  let bench () =
    print_endline "========================== list ===========================";
    let operations = [|Push push; Pop pop|] in
    let ldb = Database.build ~buffers ~size ~make:make ~operations in
    bench_unary ldb "push" "List" push uconstant_steps;
    bench_unary ldb "pop" "List" pop uconstant_steps;
    bench_unary ldb "inject" "List" inject ulinear_steps;
    bench_unary ldb "eject" "List" eject ulinear_steps;
    bench_binary ldb "concat" "List" concat blinearfst_steps;
    bench_binary ldb "concat" "ListRev" rev_concat blinearfst_steps;
    print_endline "===========================================================";
    print_endline "";
end

module PSek = struct
  let make n = P.make 0 n 0
  let push s = P.push front s 0
  let pop s = snd (P.pop_opt front s)
  let inject s = P.push back s 0
  let eject s = snd (P.pop_opt back s)
  let concat = P.concat

  let bench () =
    print_endline "=========================== sek ===========================";
    let operations =
      [|Push push; Pop pop; Inject inject; Eject eject; Concat concat|]
    in
    let sdb = Database.build ~buffers ~size ~make:make ~operations in
    bench_unary sdb "push" "Sek" push uconstant_steps;
    bench_unary sdb "pop" "Sek" pop uconstant_steps;
    bench_unary sdb "inject" "Sek" inject uconstant_steps;
    bench_unary sdb "eject" "Sek" eject uconstant_steps;
    bench_binary sdb "concat" "Sek" concat bconstant_steps;
    print_endline "===========================================================";
    print_endline "";
end

module Deque = struct
  let make n = Deque.make n 0
  let push d = Deque.push 0 d
  let pop d = match Deque.pop d with
    | None -> Deque.empty
    | Some (_, d) -> d
  let inject d = Deque.inject d 0
  let eject d = match Deque.eject d with
    | None -> Deque.empty
    | Some (d, _) -> d

  let bench () =
    print_endline "========================== deque ==========================";
    let operations = [|Push push; Pop pop; Inject inject; Eject eject|] in
    let ddb = Database.build ~buffers ~size ~make:make ~operations in
    bench_unary ddb "push" "Deque" push uconstant_steps;
    bench_unary ddb "pop" "Deque" pop uconstant_steps;
    bench_unary ddb "inject" "Deque" inject uconstant_steps;
    bench_unary ddb "eject" "Deque" eject uconstant_steps;
    print_endline "===========================================================";
    print_endline "";
end

module Steque = struct
  let make n = Steque.make n 0
  let push d = Steque.push 0 d
  let pop d = match Steque.pop d with
    | None -> Steque.empty
    | Some (_, d) -> d
  let inject d = Steque.inject d 0
  let concat = Steque.append

  let bench () =
    print_endline "========================= steque ==========================";
    let operations = [|Push push; Pop pop; Inject inject; Concat concat|] in
    let sdb = Database.build ~buffers ~size ~make:make ~operations in
    bench_unary sdb "push" "Steque" push uconstant_steps;
    bench_unary sdb "pop" "Steque" pop uconstant_steps;
    bench_unary sdb "inject" "Steque" inject uconstant_steps;
    bench_binary sdb "concat" "Steque" concat bconstant_steps;
    print_endline "===========================================================";
    print_endline "";
end

module Cadeque = struct
  let make n = Cadeque.make n 0
  let push d = Cadeque.push 0 d
  let pop d = match Cadeque.pop d with
    | None -> Cadeque.empty
    | Some (_, d) -> d
  let inject d = Cadeque.inject d 0
  let eject d = match Cadeque.eject d with
    | None -> Cadeque.empty
    | Some (d, _) -> d
  let concat = Cadeque.append

  let bench () =
    print_endline "========================= cadeque =========================";
    let operations =
      [|Push push; Pop pop; Inject inject; Eject eject; Concat concat|]
    in
    let cdb = Database.build ~buffers ~size ~make:make ~operations in
    bench_unary cdb "push" "Cadeque" push uconstant_steps;
    bench_unary cdb "pop" "Cadeque" pop uconstant_steps;
    bench_unary cdb "inject" "Cadeque" inject uconstant_steps;
    bench_unary cdb "eject" "Cadeque" eject uconstant_steps;
    bench_binary cdb "concat" "Cadeque" concat bconstant_steps;
    print_endline "===========================================================";
    print_endline ""
end

(* =============================== execution ================================ *)

let () =
  List.bench ();
  PSek.bench ();
  Deque.bench ();
  Steque.bench ();
  Cadeque.bench ()

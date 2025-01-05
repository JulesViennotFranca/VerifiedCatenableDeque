let title = "Random operations"

module type OP = sig
  include Modules.OP

  val push : 'a -> 'a t -> 'a t
  val inject : 'a t -> 'a -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val eject : 'a t -> ('a t * 'a) option
  val append : 'a t -> 'a t -> 'a t
end

let modules : (module OP) list = [
  (module Modules.Cadeque);
  (module Modules.ArtWend)
]

let make_point steps execs (module M : OP) =
  let op t step = match step with
    | 0 | 1 -> M.push 0 t
    | 2     -> (match M.pop t with
      | None -> t
      | Some (_, t') -> t')
    | 3     -> M.append t t
    | 4     -> (match M.eject t with
      | None -> t
      | Some (t', _) -> t')
    | 5 | 6 -> M.inject t 0
    | _ -> assert false
  in

  let run exec = ignore (List.fold_left op (M.make 0 0) exec) in

  let t0 = Unix.gettimeofday () in
  List.iter run execs;
  let t1 = Unix.gettimeofday () in
  1000. *. (t1 -. t0) /. float_of_int steps

let make_points steps size =
  let make_exec = fun _ -> List.init size (fun _ -> Random.int 7) in
  let execs = List.init steps make_exec in
  List.map (make_point steps execs) modules

let make_plot steps size =
  let sizes = List.init size Fun.id in
  let plot_infos =
    List.map (fun (module M : OP) -> (M.name, M.color)) modules in
  plot_infos, List.map (make_points steps) sizes

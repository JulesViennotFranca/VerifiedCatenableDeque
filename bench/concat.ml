let title = "Concatenating with itself"

module type OP = sig
  include Modules.OP
  val append : 'a t -> 'a t -> 'a t
end

let modules : (module OP) list = [
  (module Modules.List);
  (module Modules.ListRev);
  (module Modules.Steque);
  (module Modules.Cadeque);
  (module Modules.PairBuffers)
]

let make_point steps size (module M : OP) =
  let t = M.make size 0 in

  let t0 = Unix.gettimeofday () in
  for _ = 0 to steps do
    let _ = M.append t t in ()
  done;
  let t1 = Unix.gettimeofday () in
  1000. *. (t1 -. t0) /. float_of_int steps

let make_points steps size =
  List.map (make_point steps size) modules

let make_plot steps size =
  let sizes = List.init size Fun.id in
  let plot_infos =
    List.map (fun (module M : OP) -> (M.name, M.color)) modules in
  plot_infos, List.map (make_points steps) sizes

let make_points steps size =
  let res = ref [] in
  for s = 0 to size do
    let lst = List.init s (fun i -> i) in
    let cd = Cadeque.of_list lst in

    let times = [] in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = lst @ lst in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = (t1 -. t0) *. 1000. :: times in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = List.rev_append (List.rev lst) lst in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = (t1 -. t0) *. 1000. :: times in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Cadeque.(cd @ cd) in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = (t1 -. t0) *. 1000. :: times in

    res := List.rev times :: !res
  done;
  ["List"; "List rev"; "Cadeque"], List.rev !res

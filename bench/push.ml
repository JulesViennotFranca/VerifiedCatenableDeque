let make_points steps size =
  let res = ref [] in
  for s = 0 to size do
    let lst = List.init s (Fun.const 0) in
    let d = Cadeque.Deque.make s 0 in
    let st = Cadeque.Steque.make s 0 in
    let cd = Cadeque.make s 0 in

    let times = [] in

    let t0 = Unix.gettimeofday () in
    for k = 0 to steps do
      let _ = k :: lst in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    let t0 = Unix.gettimeofday () in
    for k = 0 to steps do
      let _ = Cadeque.Deque.push k d in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    let t0 = Unix.gettimeofday () in
    for k = 0 to steps do
      let _ = Cadeque.Steque.push k st in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    let t0 = Unix.gettimeofday () in
    for k = 0 to steps do
      let _ = Cadeque.push k cd in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    res := List.rev times :: !res
  done;
  ["List"; "Deque"; "Steque"; "Cadeque"], List.rev !res

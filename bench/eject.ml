let make_points steps size =
  let res = ref [] in
  for s = 0 to size do
    let lst = List.init s (Fun.const 0) in
    let d = Cadeque.Deque.make s 0 in
    let cd = Cadeque.make s 0 in

    let times = [] in

    let rec eject acc = function
      | [] -> None
      | [x] -> Some (acc, x)
      | x::l -> eject (x::acc) l
    in


    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = eject [] lst in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Cadeque.Deque.eject d in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Cadeque.eject cd in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = 1000. *. (t1 -. t0) /. float_of_int steps :: times in

    res := List.rev times :: !res
  done;
  ["List"; "Deque"; "Cadeque"], List.rev !res

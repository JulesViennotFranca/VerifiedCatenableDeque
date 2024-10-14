let make_points steps size =
  let res = ref [] in
  for s = 0 to size do
    let lst = List.init s (Fun.const 0) in
    let d = Cadeque.Deque.make s 0 in
    let cd = Cadeque.make s 0 in

    let times = [] in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = match lst with
        | [] -> None
        | x::l -> Some (x, l)
      in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = (t1 -. t0) *. 1000. :: times in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Cadeque.Deque.pop d in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = (t1 -. t0) *. 1000. :: times in

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Cadeque.pop cd in ()
    done ;
    let t1 = Unix.gettimeofday () in
    let times = (t1 -. t0) *. 1000. :: times in

    res := List.rev times :: !res
  done;
  ["List"; "Deque"; "Cadeque"], List.rev !res

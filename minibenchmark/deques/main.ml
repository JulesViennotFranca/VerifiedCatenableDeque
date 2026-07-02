module C = Deques.Cadeque

let rec push_many i n xs =
  if i = n then
    xs
  else
    let xs = C.push i xs in
    push_many (i+1) n xs

let rec pop_many i xs =
  match C.pop xs with
  | None ->
      assert (i = 0)
  | Some (x, xs) ->
      assert (i > 0);
      let i = i - 1 in
      assert (x = i);
      pop_many i xs

let n =
  500

let repetitions =
  100

let () =
  for _k = 1 to repetitions do
    let xs = push_many 0 n C.empty in
    pop_many n xs
  done

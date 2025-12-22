(* This is a vector whose capacity is fixed at its creation. *)

type 'a t =
  { array : 'a array; mutable length : int }

let[@inline] create ~size ~dummy =
  { array = Array.make size dummy; length = 0 }

let[@inline] length v =
  v.length

let[@inline] is_empty v =
  v.length = 0

let[@inline] is_full v =
  Array.length v.array = v.length

let[@inline] clear v =
  v.length <- 0

let[@inline] get v i =
  assert (0 <= i && i < v.length);
  v.array.(i)

let[@inline] set v i x =
  assert (0 <= i && i < v.length);
  v.array.(i) <- x

let[@inline] push v x =
  assert (v.length < Array.length v.array);
  v.array.(v.length) <- x;
  v.length <- v.length + 1

let[@inline] pop v =
  assert (0 < v.length);
  v.length <- v.length - 1;
  v.array.(v.length)

let[@inline] iter f v =
  for i = 0 to v.length - 1 do
    f v.array.(i)
  done

let[@inline] iter2 f v1 v2 =
  for i = 0 to v1.length - 1 do
    for j = 0 to v2.length - 1 do
      f v1.array.(i) v2.array.(j)
    done;
  done

let[@inline] to_list v =
  List.init v.length (get v)

let[@inline] sample v =
  get v (Random.int v.length)

open Monolith

(* Extra specification combinators *)

(* Indices in a list *)

let list_index l =
  int_within (fun () ->
    let len = List.length l in
    if Gen.bool () then
      -1
    else if Gen.bool () then
      len
    else
      Gen.lt len ()
  )

(* List size when creating a list *)

let list_size =
  semi_open_interval (-1) 15

(* The init higher-order function *)

type ('a, 'c) init =
  int -> (int -> 'a) -> 'c

(* TODO: can we also test the behavior of [init] on negative sizes? *)

let of_list_of_init (init : ('a, 'c) init) (l : 'a list) : 'c =
  init (List.length l) (fun i -> List.nth l i)

(* Transforms the specification of a length and an [of_list] function, that
   accepts a list of elements and returns a collection, into the specification
   for an [init] function, which accepts a length and a function that produces
   the element for each index and returns a collection.
*)
let init (of_list_spec : ('ra list -> 'rc, 'ca list -> 'cc) spec) :
  (('ra, 'rc) init, ('ca, 'cc) init) spec
  =
  map_into
    of_list_of_init
    (of_list_of_init, constant "Test_support.Specs.of_list_of_init")
    of_list_spec

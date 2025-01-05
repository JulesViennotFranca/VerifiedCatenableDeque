module type OP = sig
  type 'a t
  val make : int -> 'a -> 'a t
  val name : string
  val color : string
end

module List = struct
  type 'a t = 'a list
  let push = List.cons
  let inject l x = l @ [x]
  let pop = function
    | [] -> None
    | x :: l -> Some (x, l)
  let eject l =
    let rec aux accu = function
      | [] -> None
      | [x] -> Some (List.rev accu, x)
      | y :: l -> aux (y :: accu) l
    in
    aux [] l
  let append = List.append

  let make n x = List.init n (Fun.const x)

  let name = "List"
  let color = "#DB3AE0"
end

module ListRev = struct
  include List

  let inject l x = Stdlib.List.rev (x :: Stdlib.List.rev l)
  let append l1 l2 = Stdlib.List.rev_append (Stdlib.List.rev l1) l2

  let name = "ListRev"
  let color = "#EE594D"
end

module Deque = struct
  include Cadeque.Deque

  let name = "Deque"
  let color = "#F72500"
end

module Steque = struct
  include Cadeque.Steque

  let name = "Steque"
  let color = "#BCC20A"
end

module Cadeque = struct
  include Cadeque

  let name = "Cadeque"
  let color = "#5C7AFF"
end

module ArtWend = struct
  include Cadeque.ArtWend

  let name = "AW cadeque"
  let color = "#6338B3"
end

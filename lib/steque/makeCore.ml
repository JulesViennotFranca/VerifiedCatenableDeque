open Color.GYR

module Make (Deque : ListLike.DEQUE) = struct

  (* +----------------------------------------------------------------------+ *)
  (* |                                Types                                 | *)
  (* +----------------------------------------------------------------------+ *)

  (** A type for suffix buffers. *)
  type 'a suffix = 'a Deque.t

  (** A type for prefix buffers that need to have at least two elements. *)
  type ('a, 'color) prefix =
    | P2 : 'a * 'a                       -> ('a,    red) prefix
    | P3 : 'a * 'a * 'a                  -> ('a, yellow) prefix
    | P4 : 'a * 'a * 'a * 'a * 'a suffix -> ('a,  green) prefix

  (** A type for green or yellow prefixes. *)
  type 'a gy_prefix = GY : ('a, _ * _ * nored) prefix -> 'a gy_prefix

  (** A type for the regularity relation. *)
  type ('color_pkt, 'color_sub, 'color_chain) regularity =
    | G : ( green, _ * noyellow * _,  green) regularity
    | Y : (yellow, green, yellow) regularity
    | O : (yellow,   red, orange) regularity
    | R : (   red, green,    red) regularity

  (** A type for pairs. *)
  type 'a pair =
    Pair : ('a, _) prefix * ('a pair, _) chain -> 'a pair

  (** A type for packets. *)
  and ('a, 'b, 'color) packet =
    | Hole   : ('a, 'a, uncolored) packet
    | Packet : ('a, 'c) prefix
            * ('a pair, 'b, nogreen * _ * nored) packet
            * 'a suffix
            -> ('a, 'b, 'c) packet

  (** A type for chains. *)
  and ('a, 'color) chain =
    | Ending : 'a Deque.t -> ('a, green) chain
    | Chain : ('c1, 'c2, 'c3) regularity
            * ('a, 'b, 'c1) packet
            * ('b, 'c2) chain
          -> ('a, 'c3) chain

  (** A type for the triple representation of a steque. The steque is either a
      deque or a triple made of a prefix, a child steque and a suffix. *)
  type 'a triple =
    | Small : 'a suffix -> 'a triple
    | Triple : ('a, 'c) prefix * ('a pair, _) chain * 'a suffix -> 'a triple

  (** A type for semi-regular steques. *)
  type 'a semi_steque = S : ('a, _) chain -> 'a semi_steque

  (** A type for regular steques. *)
  type 'a steque = T : ('a, _ * _ * nored) chain -> 'a steque

  (* +----------------------------------------------------------------------+ *)
  (* |                                 Core                                 | *)
  (* +----------------------------------------------------------------------+ *)

  (** The empty chain. *)
  let empty_chain = Ending Deque.empty

  (** Takes a green prefix [p], a child chain [child] and a suffix [s] and
      returns the green chain (p, child, s). *)
  let make_green
  : type a c. (a, green) prefix -> (a pair, c) chain -> a suffix
          -> (a, green) chain
  = fun p child s ->
    match child with
    | Ending d -> Chain (G, Packet (p, Hole, s), Ending d)
    | Chain (G, pkt, c) -> Chain (G, Packet (p, Hole, s), Chain (G, pkt, c))
    | Chain (Y, pkt, c) -> Chain (G, Packet (p, pkt, s), c)
    | Chain (O, pkt, c) -> Chain (G, Packet (p, pkt, s), c)
    | Chain (R, pkt, c) -> Chain (G, Packet (p, Hole, s), Chain (R, pkt, c))

  (** Takes a yellow prefix [p], a green or yellow child chain [child] and a
      suffix [s] and returns the yellow chain (p, child, s). *)
  let make_yellow
  : type a g y. (a, yellow) prefix -> (a pair, g * y * nored) chain -> a suffix
            -> (a, yellow) chain
  = fun p child s ->
    match child with
    | Ending d -> Chain (Y, Packet (p, Hole, s), Ending d)
    | Chain (G, pkt, c) -> Chain (Y, Packet (p, Hole, s), Chain (G, pkt, c))
    | Chain (Y, pkt, c) -> Chain (Y, Packet (p, pkt, s), c)

  (** Takes a yellow prefix [p], a child chain [child] and a suffix [s] and
      returns the semi-regular steque (p, child, s).
      The hidden color of the output steque is either yellow or orange. *)
  let make_yellorange
  : type a c.
       (a, yellow) prefix -> (a pair, c) chain -> a suffix -> a semi_steque
  = fun p child s ->
    match child with
    | Ending d -> S (Chain (Y, Packet (p, Hole, s), Ending d))
    | Chain (G, pkt, c) -> S (Chain (Y, Packet (p, Hole, s), Chain (G, pkt, c)))
    | Chain (Y, pkt, c) -> S (Chain (Y, Packet (p, pkt, s), c))
    | Chain (O, pkt, c) -> S (Chain (O, Packet (p, pkt, s), c))
    | Chain (R, pkt, c) -> S (Chain (O, Packet (p, Hole, s), Chain (R, pkt, c)))

  (** Takes a red prefix [p], a child steque [child] and a suffix [s] and
      returns the red chain (p, child, s). *)
  let make_red
  : type a g y. (a, red) prefix -> (a pair, g * y * nored) chain -> a suffix
            -> (a, red) chain
  = fun p child s ->
    match child with
    | Ending d -> Chain (R, Packet (p, Hole, s), Ending d)
    | Chain (G, pkt, c) -> Chain (R, Packet (p, Hole, s), Chain (G, pkt, c))
    | Chain (Y, pkt, c) -> Chain (R, Packet (p, pkt, s), c)

  (** Takes a packet [pkt] and a green or red chain [c], and returns a
      semi-regular steque made of [pkt] followed by [c]. *)
  let build_on_green_or_red
  : type a b g1 y1 g2 r2.
      (a, b, g1 * y1 * nored) packet
    -> (b, g2 * noyellow * r2) chain
    -> a semi_steque
  = fun pkt c ->
    match pkt with
    | Hole -> S c
    | Packet (P4 _ as p, pkt, s) -> S (Chain (G, Packet (p, pkt, s), c))
    | Packet (P3 _ as p, pkt, s) ->
      match c with
      | Ending d -> S (Chain (Y, Packet (p, pkt, s), Ending d))
      | Chain (G, _, _) as c -> S (Chain (Y, Packet (p, pkt, s), c))
      | Chain (R, _, _) as c -> S (Chain (O, Packet (p, pkt, s), c))

  (** Takes a packet [pkt] and a green chain [c], and returns a regular steque
      made of [pkt] followed by [c]. *)
  let build_on_green
  : type a b y.
      (a, b, nogreen * y * nored) packet
    -> (b, green) chain
    -> a steque
  = fun pkt c ->
    match pkt with
    | Hole -> T c
    | Packet (P3 _, _, _) as pkt -> T (Chain (Y, pkt, c))

  (** Takes a yellow packet [pkt] and a red chain [c], and returns a
      semi-regular steque made of [pkt] followed by [c]. *)
  let build_on_red
  : type a b y.
      (a, b, nogreen * y * nored) packet
    -> (b, red) chain
    -> a semi_steque
  = fun pkt c ->
    match pkt with
    | Hole -> S c
    | Packet (P3 _, _, _) as pkt -> S (Chain (O, pkt, c))

  (** Pushes on a green prefix. *)
  let green_prefix_push
  : type a. a -> (a, green) prefix -> (a, green) prefix
  = fun x -> function
    | P4 (a, b, c, d, deq) -> P4 (x, a, b, c, Deque.push d deq)

  (** Pushes on a yellow prefix. *)
  let yellow_prefix_push
  : type a. a -> (a, yellow) prefix -> (a, green) prefix
  = fun x -> function
    | P3 (a, b, c) -> P4 (x, a, b, c, Deque.empty)

  (** Pushes on a red prefix. *)
  let red_prefix_push
  : type a. a -> (a, red) prefix -> (a, yellow) prefix
  = fun x -> function
    | P2 (a, b) -> P3 (x, a, b)

  (** Pushes tow elements on a prefix. *)
  let prefix_push_two
  : type a c. a -> a -> (a, c) prefix -> (a, green) prefix
  = fun x y -> function
    | P2 (a, b) -> P4 (x, y, a, b, Deque.empty)
    | P3 (a, b, c) -> P4 (x, y, a, b, Deque.push c Deque.empty)
    | P4 (a, b, c, d, e) -> P4 (x, y, a, b, Deque.push c (Deque.push d e))

  (** Pushes on a chain and returns a steque. *)
  let chain_push
  : type a c. a -> (a, c) chain -> a steque
  = fun x c ->
    match c with
    | Ending d -> T (Ending (Deque.push x d))
    | Chain (G, Packet (p, pkt, s), c) ->
      let p = green_prefix_push x p in
      T (Chain (G, Packet (p, pkt, s), c))
    | Chain (Y, Packet (p, pkt, s), c) ->
      let p = yellow_prefix_push x p in
      T (Chain (G, Packet (p, pkt, s), c))
    | Chain (O, Packet (p, pkt, s), c) ->
      let p = yellow_prefix_push x p in
      T (Chain (G, Packet (p, pkt, s), c))
    | Chain (R, Packet (p, pkt, s), c) ->
      let p = red_prefix_push x p in
      T (Chain (Y, Packet (p, pkt, s), c))

  (** Injects on a chain. *)
  let chain_inject
  : type a c. (a, c) chain -> a -> (a, c) chain
  = fun c x ->
    match c with
    | Ending d -> Ending (Deque.inject d x)
    | Chain (reg, Packet (p, pkt, s), c) ->
      let s = Deque.inject s x in
      Chain (reg, Packet (p, pkt, s), c)
    | Chain (_, Hole, _) -> .

  (** Pushes on a semi-regular steque. *)
  let semi_push
  : type a. a -> a semi_steque -> a semi_steque
  = fun x (S c) ->
    let T c = chain_push x c in
    S c

  (** Returns the triple corresponding to a chain. *)
  let triple_of_semi
  : type a. a semi_steque -> a triple
  = fun (S c) ->
    match c with
    | Ending d -> Small d
    | Chain (G, Packet (p, pkt, s), c) ->
      let S child = build_on_green_or_red pkt c in
      Triple (p, child, s)
    | Chain (Y, Packet (p, pkt, s), c) ->
      let T child = build_on_green pkt c in
      Triple (p, child, s)
    | Chain (O, Packet (p, pkt, s), c) ->
      let S child = build_on_red pkt c in
      Triple (p, child, s)
    | Chain (R, Packet (p, pkt, s), c) ->
      let T child = build_on_green pkt c in
      Triple (p, child, s)

  (** Joins a child chain [child], a suffix [s] and a semi-regular steque [sst].
      Computes case 1 of concatenating two steques:
      s1 = (_, child, s) and s2 = sst. *)
  let join
  : type a c.
      (a pair, c) chain
    -> a suffix
    -> a semi_steque
    -> (a pair, c) chain * a suffix
  = fun child s sst ->
    let child1, sst2 =
      match Deque.pop s with
      (* The suffix contains no elements, we do nothing. *)
      | None -> child, sst
      | Some (w, s) ->
        match Deque.pop s with
        (* The suffix contains one element, we push it on [sst2]. *)
        | None -> child, semi_push w sst
        | Some (x, s) ->
          (* The suffix contains more than two elements, we represent it as a
             pair ... *)
          let pair = match Deque.pop s with
          | None -> Pair (P2 (w, x), empty_chain)
          | Some (y, s) ->
            match Deque.pop s with
            | None -> Pair (P3 (w, x, y), empty_chain)
            | Some (z, s) -> Pair (P4 (w, x, y, z, s), empty_chain)
          in
          (* ... and inject (s, empty) on the child. *)
          chain_inject child pair, sst
    in
    (* At this stage, [child1] is the new child and [sst2] is the new
       semi-regular steque.

       Now we must perform a last operation on [child1] if [sst2] is a not
       ending, and then we can simply return [child1] and the suffix of
       [sst2]. *)
    match triple_of_semi sst2 with
    (* [sst2] is a suffix, we can return it directly. *)
    | Small s2 -> child1, s2
    (* Otherwise, we have to retrieve the prefix and the child of [sst2], and
       inject the pair (prefix(sst2), child(sst2)) on [child1]. *)
    | Triple (p2, child2, s2) ->
      chain_inject child1 (Pair (p2, child2)), s2

  (** Concatenates two semi-regular steques. *)
  let semi_concat
  : type a. a semi_steque -> a semi_steque -> a semi_steque
  = fun (S c1) sst2 ->
    match c1 with
    (* If our first semi-regular steque is only a deque, we look at the number
       of elements it contains. *)
    | Ending d ->
      begin match Deque.pop d with
      (* No elements, we return [sst2] *)
      | None -> sst2
      | Some (w, d) ->
        match Deque.pop d with
        (* One element, push it on [sst2] *)
        | None -> semi_push w sst2
        | Some (x, d) ->
          match Deque.pop d with
          (* Two elements, push them in the right order on [sst2] *)
          | None -> semi_push w (semi_push x sst2)
          | Some (y, d) ->
            match Deque.pop d with
            (* Three elements, push them in the right order on [sst2]. *)
            | None -> semi_push w (semi_push x (semi_push y sst2))
            | Some (z, d) ->
              (* If we have four or more elements, [c1] can be considered a
                 green prefix. *)
              let prefix = P4 (w, x, y, z, d) in
              (* We look wether [sst2] is a deque or not. *)
              match triple_of_semi sst2 with
              (* If it is the case, we can directly return the green steque
                 (c1, empty, sst2) = (prefix, empty, suffix) here. *)
              | Small suffix ->
                S (Chain (G, Packet (prefix, Hole, suffix), empty_chain))
              (* If [sst2] is a triple, its prefix is pushed on its child and
                 the final steque is simply (c1, child(sst2), suffix(sst2))
                 = (prefix, child, suffix) here. *)
              | Triple (p, child, suffix) ->
                let T child = chain_push (Pair (p, empty_chain)) child in
                S (make_green prefix child suffix)
        end
    (* Otherwise, we get the child of [c1] with a build function and we use the
       join function to concatenate this child and the second steque. *)
    | Chain (G, Packet (p, pkt, s), c) ->
      let S child = build_on_green_or_red pkt c in
      let child, s = join child s sst2 in
      S (make_green p child s)
    | Chain (Y, Packet (p, pkt, s), c) ->
      let T child = build_on_green pkt c in
      let child, s = join child s sst2 in
      S (make_yellow p child s)
    | Chain (O, Packet (p, pkt, s), c) ->
      let S child = build_on_red pkt c in
      let child, s = join child s sst2 in
      make_yellorange p child s
    | Chain (R, Packet (p, pkt, s), c) ->
      let T child = build_on_green pkt c in
      let child, s = join child s sst2 in
      S (make_red p child s)

  (** Pops off a green prefix. *)
  let green_prefix_pop
  : type a. (a, green) prefix -> a * a gy_prefix
  = function
    | P4 (a, b, c, d, deq) ->
        match Deque.pop deq with
        | None -> a, GY (P3 (b, c, d))
        | Some (e, deq) -> a, GY (P4 (b, c, d, e, deq))

  (** Pops off a yellow prefix. *)
  let yellow_prefix_pop
  : type a. (a, yellow) prefix -> a * (a, red) prefix
  = function P3 (a, b, c) -> a, P2 (b, c)

  (** Makes a red chain green. *)
  let green_of_red
  : type a. (a, red) chain -> (a, green) chain
  = function
    (* First, we look at the case where the child of the red chain is a
       deque. *)
    | Chain (R, Packet (P2 (a, b), Hole, s), Ending d) ->
      begin match Deque.pop d with
      (* If the deque is empty, we can simply push the two elements of the
         prefix on the suffix. *)
      | None -> Ending (Deque.push a (Deque.push b s))
      (* If not, we can pop a pair from the deque and push the two elements of
         the prefix on this pair to get a green prefix. *)
      | Some (Pair (p, stored), d) ->
          let p = prefix_push_two a b p in
          (* Then, to get the child of the final green steque, we concatenate
             the steque from the poped pair and the rest of the deque. *)
          let S child = semi_concat (S stored) (S (Ending d)) in
          make_green p child s
      end

    (* For the two remaining cases, we do the same thing:
       pop a pair [Pair (p, stored)] from the child,
       push the two elements of prefix on [p],
       concatenate [stored] and child to form [child],
       return the chain made of [p], [child] and the suffix. *)
    | Chain (R, Packet (P2 (a, b), Hole, s),
                Chain (G, Packet (p2, pkt, s2), c)) ->
      let Pair (p, stored), GY p2 = green_prefix_pop p2 in
      let p = prefix_push_two a b p in
      let child = build_on_green_or_red (Packet (p2, pkt, s2)) c in
      let S child = semi_concat (S stored) child in
      make_green p child s
    | Chain (R, Packet (P2 (a, b), Packet (P3 _ as p2, pkt, s2), s), c) ->
      let Pair (p, stored), p2 = yellow_prefix_pop p2 in
      let p = prefix_push_two a b p in
      let child = S (Chain (R, Packet (p2, pkt, s2), c)) in
      let S child = semi_concat (S stored) child in
      make_green p child s

  (** Makes a green or red chain green. *)
  let ensure_green
  : type a g r. (a, g * noyellow * r) chain -> (a, green) chain
  = function
    | Ending d -> Ending d
    | Chain (G, pkt, c) -> Chain (G, pkt, c)
    | Chain (R, pkt, c) -> green_of_red (Chain (R, pkt, c))

  (* +----------------------------------------------------------------------+ *)
  (* |                              Operations                              | *)
  (* +----------------------------------------------------------------------+ *)

  (** The empty steque. *)
  let empty = T empty_chain

  (** Pushes on a steque. *)
  let push
  : type a. a -> a steque -> a steque
  = fun x (T c) -> chain_push x c

  (** Injects on a steque. *)
  let inject
  : type a. a steque -> a -> a steque
  = fun (T c) x -> T (chain_inject c x)

  (** Pops off a steque. *)
  let pop
  : type a. a steque -> (a * a steque) option
  = fun (T c) ->
    match c with
    | Ending d ->
        begin match Deque.pop d with
        | None -> None
        | Some (x, d) -> Some (x, T (Ending d))
        end
    | Chain (G, Packet (p, pkt, s), c) ->
      let x, GY p = green_prefix_pop p in
      begin match p with
      | P4 _ as p -> Some (x, T (Chain (G, Packet (p, pkt, s), c)))
      | P3 _ as p -> Some (x, T (Chain (Y, Packet (p, pkt, s), ensure_green c)))
      end
    | Chain (Y, Packet (p, pkt, s), c) ->
        let x, p = yellow_prefix_pop p in
        Some (x, T (green_of_red (Chain (R, Packet (p, pkt, s), c))))

  (** Concatenates two steques. *)
  let concat
  : type a. a steque -> a steque -> a steque
  = fun (T c1) (T c2) ->
    match c1 with
    | Ending d ->
      begin match Deque.pop d with
      | None -> T c2
      | Some (w, d) ->
        match Deque.pop d with
        | None -> push w (T c2)
        | Some (x, d) ->
          match Deque.pop d with
          | None -> push w (push x (T c2))
          | Some (y, d) ->
            match Deque.pop d with
            | None -> push w (push x (push y (T c2)))
            | Some (z, d) ->
              let prefix = P4 (w, x, y, z, d) in
              match triple_of_semi (S c2) with
              | Small suffix ->
                T (Chain (G, Packet (prefix, Hole, suffix), empty_chain))
              | Triple (p, child, suffix) ->
                let T child = chain_push (Pair (p, empty_chain)) child in
                T (make_green prefix child suffix)
        end
    | Chain (G, Packet (p, pkt, s), c) ->
      let S child = build_on_green_or_red pkt c in
      let child, s = join child s (S c2) in
      T (make_green p child s)
    | Chain (Y, Packet (p, pkt, s), c) ->
      let T child = build_on_green pkt c in
      let child, s = join child s (S c2) in
      T (make_yellow p child s)

end
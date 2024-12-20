open Color.GYR

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(** A type for buffers. *)
type ('a, 'color) buffer =
  | B0 :                           ('a, red            ) buffer
  | B1 : 'a                     -> ('a, nogreen * _ * _) buffer
  | B2 : 'a * 'a                -> ('a, _              ) buffer
  | B3 : 'a * 'a * 'a           -> ('a, _              ) buffer
  | B4 : 'a * 'a * 'a * 'a      -> ('a, nogreen * _ * _) buffer
  | B5 : 'a * 'a * 'a * 'a * 'a -> ('a, red            ) buffer

(** A type for packets. *)
type ('a, 'b, 'color) packet =
  | Hole : ('a, 'a, uncolored) packet
  | Packet : ('a, 'c) buffer
           * ('a * 'a, 'b, nogreen * _ * nored) packet
           * ('a, 'c) buffer
          -> ('a, 'b, 'c) packet

(** A type for the regularity relation. *)
type ('color_pkt, 'color_chain) regularity =
  | G : (green , _ * noyellow * _) regularity
  | Y : (yellow, green) regularity
  | R : (red   , green) regularity

(** A type for chains. *)
type ('a, 'color) chain =
  | Ending : ('a, _) buffer -> ('a, green) chain
  | Chain :  ('c1, 'c2) regularity * ('a, 'b, 'c1) packet * ('b, 'c2) chain
          -> ('a, 'c1) chain

(** A type decomposing buffers according to their number of elements.
    Buffers with 0 or 1 element are decomposed into [Underflow];
    buffers with 2 or 3 elements are decomposed into [Ok];
    buffers with 4 or 5 elements are decomposed into [Overflow]. *)
type 'a decompose =
  | Underflow : 'a option -> 'a decompose
  | Ok        : ('a, green) buffer -> 'a decompose
  | Overflow  : ('a, green) buffer * ('a * 'a) -> 'a decompose

(** A type decomposing a buffer into its first element, a central buffer, and
    its last element. If such a decomposition is not possible, an option
    representing the buffer is returned with [Alone]. *)
type 'a sandwich =
  | Alone    : 'a option                -> 'a sandwich
  | Sandwich : 'a * ('a, _) buffer * 'a -> 'a sandwich

(** A type for deques. *)
type 'a deque = T : ('a, _ * _ * nored) chain -> 'a deque

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(** Pushes on a green buffer. *)
let green_push
: type a. a -> (a, green) buffer -> (a, yellow) buffer
= fun x buf ->
  match buf with
  | B2 (a, b)    -> B3 (x, a, b)
  | B3 (a, b, c) -> B4 (x, a, b, c)

(** Injects on a green buffer. *)
let green_inject
: type a. (a, green) buffer -> a -> (a, yellow) buffer
= fun buf x ->
  match buf with
  | B2 (a, b)    -> B3 (a, b, x)
  | B3 (a, b, c) -> B4 (a, b, c, x)

(** Pops off a green buffer. *)
let green_pop : type a. (a, green) buffer -> a * (a, yellow) buffer
= function
  | B2 (a, b)    -> a, B1 b
  | B3 (a, b, c) -> a, B2 (b, c)

(** Ejects off a green buffer. *)
let green_eject : type a. (a, green) buffer -> (a, yellow) buffer * a
= function
  | B2 (a, b)    -> B1 a, b
  | B3 (a, b, c) -> B2 (a, b), c

(** Pushes on a yellow buffer. *)
let yellow_push : type a. a -> (a, yellow) buffer -> (a, red) buffer
= fun x b ->
  match b with
  | B1 a            -> B2 (x, a)
  | B2 (a, b)       -> B3 (x, a, b)
  | B3 (a, b, c)    -> B4 (x, a, b, c)
  | B4 (a, b, c, d) -> B5 (x, a, b, c, d)

(** Injects on a yellow buffer. *)
let yellow_inject : type a. (a, yellow) buffer -> a -> (a, red) buffer
= fun b x ->
  match b with
  | B1 a            -> B2 (a, x)
  | B2 (a, b)       -> B3 (a, b, x)
  | B3 (a, b, c)    -> B4 (a, b, c, x)
  | B4 (a, b, c, d) -> B5 (a, b, c, d, x)

(** Pops off a yellow buffer. *)
let yellow_pop : type a. (a, yellow) buffer -> a * (a, red) buffer
= fun b ->
  match b with
  | B1 a            -> a, B0
  | B2 (a, b)       -> a, (B1  b)
  | B3 (a, b, c)    -> a, (B2 (b, c))
  | B4 (a, b, c, d) -> a, (B3 (b, c, d))

(** Ejects off a yellow buffer. *)
let yellow_eject : type a. (a, yellow) buffer -> (a, red) buffer * a
= fun b ->
  match b with
  | B1 a            -> B0,             a
  | B2 (a, b)       -> (B1  a),        b
  | B3 (a, b, c)    -> (B2 (a, b)),    c
  | B4 (a, b, c, d) -> (B3 (a, b, c)), d

(** Pushes on a buffer, and returns a green chain. *)
let buffer_push : type a c. a -> (a, c) buffer -> (a, green) chain
= fun x b ->
  match b with
  | B0 -> Ending (B1 x)
  | B1 a -> Ending (B2 (x, a))
  | B2 (a, b) -> Ending (B3 (x, a, b))
  | B3 (a, b, c) -> Ending (B4 (x, a, b, c))
  | B4 (a, b, c, d) -> Ending (B5 (x, a, b, c, d))
  | B5 (a, b, c, d, e) ->
      Chain (G, Packet (B3 (x, a, b), Hole, B3 (c, d, e)), Ending B0)

(** Injects on a buffer, and returns a green chain. *)
let buffer_inject : type a c. (a, c) buffer -> a -> (a, green) chain
= fun b x ->
  match b with
  | B0 -> Ending (B1 x)
  | B1 a -> Ending (B2 (a, x))
  | B2 (a, b) -> Ending (B3 (a, b, x))
  | B3 (a, b, c) -> Ending (B4 (a, b, c, x))
  | B4 (a, b, c, d) -> Ending (B5 (a, b, c, d, x))
  | B5 (a, b, c, d, e) ->
      Chain (G, Packet (B3 (a, b, c), Hole, B3 (d, e, x)), Ending B0)

(** Pops off a buffer, and returns an option. *)
let buffer_pop : type a c. (a, c) buffer -> (a * (a, red) buffer) option
= function
  | B0 -> None
  | B1 a -> Some (a, B0)
  | B2 (a, b) -> Some (a, (B1 b))
  | B3 (a, b, c) -> Some (a, (B2 (b, c)))
  | B4 (a, b, c, d) -> Some (a, (B3 (b, c, d)))
  | B5 (a, b, c, d, e) -> Some (a, (B4 (b, c, d, e)))

(** Ejects off a buffer, and returns an option. *)
let buffer_eject : type a c. (a, c) buffer -> ((a, red) buffer * a) option
= function
  | B0 -> None
  | B1 a -> Some (B0, a)
  | B2 (a, b) -> Some ((B1 a), b)
  | B3 (a, b, c) -> Some ((B2 (a, b)), c)
  | B4 (a, b, c, d) -> Some ((B3 (a, b, c)), d)
  | B5 (a, b, c, d, e) -> Some ((B4 (a, b, c, d)), e)

(** Pushes then ejects. *)
let prefix_rot : type a c. a -> (a, c) buffer -> (a, c) buffer * a
= fun x buf -> match buf with
  | B0 -> B0, x
  | B1 a -> B1  x, a
  | B2 (a, b) -> B2 (x, a), b
  | B3 (a, b, c) -> B3 (x, a, b), c
  | B4 (a, b, c, d) -> B4 (x, a, b, c), d
  | B5 (a, b, c, d, e) -> B5 (x, a, b, c, d), e

(** Injects then pops. *)
let suffix_rot : type a c. (a, c) buffer -> a -> a * (a, c) buffer
= fun buf x -> match buf with
  | B0 -> x, B0
  | B1 a -> a, B1 x
  | B2 (a, b) -> a, B2 (b, x)
  | B3 (a, b, c) -> a, B3 (b, c, x)
  | B4 (a, b, c, d) -> a, B4 (b, c, d, x)
  | B5 (a, b, c, d, e) -> a, B5 (b, c, d, e, x)

(** Merges an option and a pair to create a green buffer. *)
let prefix23 opt (b, c) = match opt with
  | None   -> B2 (b, c)
  | Some a -> B3 (a, b, c)

(** Merges a pair and an option to create a green buffer. *)
let suffix23 (a, b) opt = match opt with
  | None   -> B2 (a, b)
  | Some c -> B3 (a, b, c)

(** Merges an element and an option to create a yellow buffer. *)
let suffix12 x opt = match opt with
  | None   -> B1 x
  | Some y -> B2 (x, y)

(** Returns the decomposed version of a buffer. Here, it is a prefix
    decomposition: when the buffer has 4 or 5 elements, those at the
    end are set apart. *)
let prefix_decompose : type a c. (a, c) buffer -> a decompose
= function
  (* No element, the buffer is underflowed. *)
  | B0   -> Underflow None
  (* One element, the buffer is underflowed. We keep track of the single
      element as an option type. *)
  | B1 x -> Underflow (Some x)
  (* The buffer is green, it's ok. *)
  | (B2 _) as b -> Ok b
  | (B3 _) as b -> Ok b
  (* The buffer is overflowed, we can remove a pair to make it green. *)
  | B4 (a, b, c, d)    -> Overflow (B2 (a, b), (c, d))
  | B5 (a, b, c, d, e) -> Overflow (B3 (a, b, c), (d, e))

(** Returns the decomposed version of a buffer. Here, it is a suffix
    decomposition: when the buffer has 4 or 5 elements, those at the
    start are set apart. *)
let suffix_decompose : type a c. (a, c) buffer -> a decompose
= function
  (* No element, the buffer is underflowed. *)
  | B0   -> Underflow None
  (* One element, the buffer is underflowed. We keep track of the single
      element as an option type. *)
  | B1 x -> Underflow (Some x)
  (* The buffer is green, it's ok. *)
  | (B2 _) as b -> Ok b
  | (B3 _) as b -> Ok b
  (* The buffer is overflowed, we can remove a pair to make it green. *)
  | B4 (a, b, c, d)    -> Overflow (B2 (c, d), (a, b))
  | B5 (a, b, c, d, e) -> Overflow (B3 (c, d, e), (a, b))

(** Returns the sandwiched version of a buffer. *)
let buffer_unsandwich : type a c. (a, c) buffer -> a sandwich
= function
  | B0 -> Alone None
  | B1 a -> Alone (Some a)
  | B2 (a, b) -> Sandwich (a, B0, b)
  | B3 (a, b, c) -> Sandwich (a, B1 b, c)
  | B4 (a, b, c, d) -> Sandwich (a, B2 (b, c), d)
  | B5 (a, b, c, d, e) -> Sandwich (a, B3 (b, c, d), e)

(** Converts a buffer to a buffer of pairs. If the buffer has an odd number of
    elements, the first is returned via an option. *)
let buffer_halve : type a c. (a, c) buffer -> a option * (a * a, red) buffer
= function
  | B0 -> None, B0
  | B1 a -> Some a, B0
  | B2 (a, b) -> None, B1 (a, b)
  | B3 (a, b, c) -> Some a, B1 (b, c)
  | B4 (a, b, c, d) -> None, B2 ((a, b), (c, d))
  | B5 (a, b, c, d, e) -> Some a, B2 ((b, c), (d, e))

(** Makes a non-red buffer yellow. *)
let to_yellow : type a g y. (a, g * y * nored) buffer -> (a, yellow) buffer
= function
  | B1 a -> B1 a
  | B2 (a, b) -> B2 (a, b)
  | B3 (a, b, c) -> B3 (a, b, c)
  | B4 (a, b, c, d) -> B4 (a, b, c, d)

(** Makes a buffer of any color red. *)
let to_red : type a c. (a, c) buffer -> (a, red) buffer
= function
  | B0 -> B0
  | B1 a -> B1 a
  | B2 (a, b) -> B2 (a, b)
  | B3 (a, b, c) -> B3 (a, b, c)
  | B4 (a, b, c, d) -> B4 (a, b, c, d)
  | B5 (a, b, c, d, e) -> B5 (a, b, c, d, e)

(** Takes a buffer of any color and a green buffer of pairs, rearranges elements
    contained in them, and returns a green buffer and a yellow buffer of pairs.
    The order of elements is preserved. *)
let green_prefix_concat
: type a c.
     (a, c) buffer
  -> (a * a, green) buffer
  -> (a, green) buffer * ((a * a), yellow) buffer
= fun b1 b2 ->
  match prefix_decompose b1 with
  (* If the first buffer lacks elements, we pop a pair from the second one and
      inject it onto the first one. *)
  | Underflow opt ->
      let ab, b2 = green_pop b2 in
      prefix23 opt ab, b2
  (* If the first buffer is already green, we have nothing to do. *)
  | Ok b1 -> b1, to_yellow b2
  (* If the first buffer has to much elements, we simply push them onto the
      second one. *)
  | Overflow (b1, ab) -> b1, green_push ab b2

(** Takes a green buffer of pairs and a buffer of any color, rearranges elements
    contained in them, and returns a yellow buffer of pairs and a green buffer.
    The order of elements is preserved. *)
let green_suffix_concat
: type a c.
      (a * a, green) buffer
  -> (a, c) buffer
  -> ((a * a), yellow) buffer * (a, green) buffer
= fun b1 b2 ->
  match suffix_decompose b2 with
  | Underflow opt ->
      let b1, ab = green_eject b1 in
      b1, suffix23 ab opt
  | Ok b2 -> to_yellow b1, b2
  | Overflow (b2, ab) -> green_inject b1 ab, b2

(** Takes a buffer of any color and a yellow buffer of pairs, rearranges
    elements contained in them, and returns a green buffer and a buffer of pairs
    of any color.
    The order of elements is preserved. *)
let prefix_concat b1 b2 =
  match prefix_decompose b1 with
  (* If the first buffer lacks elements, we pop a pair from the second one and
      inject it onto the first one. *)
  | Underflow opt ->
      let ab, b2 = yellow_pop b2 in
      prefix23 opt ab, b2
  (* If the first one is already green, we have nothing to do. *)
  | Ok b1 -> b1, to_red b2
  (* If the first buffer has to much elements, we simply push them onto the
      second one. *)
  | Overflow (b1, ab) -> b1, yellow_push ab b2

(** Takes a yellow buffer of pairs and a buffer of any color, rearranges
    elements contained in them, and returns a buffer of pairs of any color and a
    green buffer.
    The order of elements is preserved. *)
let suffix_concat b1 b2 =
  (* The reasoning is the same as in the previous case. *)
  match suffix_decompose b2 with
  | Underflow opt ->
      let b1, ab = yellow_eject b1 in
      b1, suffix23 ab opt
  | Ok b2 -> to_red b1, b2
  | Overflow (b2, ab) -> yellow_inject b1 ab, b2

(** Takes a prefix buffer, a child buffer, and a suffix buffer, and rearranges
    all elements contained in these buffers to form a green chain.
    The order of elements is preserved. *)
let make_small
= fun b1 b2 b3 ->
  match prefix_decompose b1, suffix_decompose b3 with
  (* Both the prefix and the suffix are underflowed. *)
  | Underflow p1, Underflow s1 ->
    begin match buffer_unsandwich b2 with
    (* If the child buffer has 0 or 1 element, an ending chain is constructed
       containing all elements. *)
    | Alone opt ->
      begin match p1, opt, s1 with
      | None,   None,        None   -> Ending B0
      | Some a, None,        None
      | None,   None,        Some a -> Ending (B1 a)
      | Some a, None,        Some b
      | None,   Some (a, b), None   -> Ending (B2 (a, b))
      | Some a, Some (b, c), None
      | None,   Some (a, b), Some c -> Ending (B3 (a, b, c))
      | Some a, Some (b, c), Some d -> Ending (B4 (a, b, c, d))
      end
    (* If the child buffer has more than 2 elements, it can be sandwiched, and
       green prefix and suffix buffers are constructed. *)
    | Sandwich (ab, rest, cd) ->
        Chain (G, Packet (prefix23 p1 ab, Hole, suffix23 cd s1), Ending rest)
    end

  (* Only the prefix is underflowed. *)
  | Underflow p1, Ok s1 ->
    begin match buffer_pop b2, p1 with
    (* If the child buffer has 0 element, elements from the prefix are pushed
       on the suffix to make an ending chain. *)
    | None, None   -> Ending s1
    | None, Some x -> buffer_push x s1
    (* If the child buffer has more than 1 element, this element is poped and
       added to the prefix to make it green. *)
    | Some (cd, rest), _ ->
        Chain (G, Packet (prefix23 p1 cd, Hole, s1), Ending rest)
    end

  (* The prefix is underflowed, the suffix is overflowed. *)
  | Underflow p1, Overflow (s1, ab) ->
      let cd, center = suffix_rot b2 ab in
      Chain (G, Packet (prefix23 p1 cd, Hole, s1), Ending center)

  (* Only the suffix is underflowed. Similar to the symmetric case. *)
  | Ok p1, Underflow s1 ->
    begin match buffer_eject b2, s1 with
    | None, None   -> Ending p1
    | None, Some x -> buffer_inject p1 x
    | Some (rest, ab), _ ->
        Chain (G, Packet (p1, Hole, suffix23 ab s1), Ending rest)
    end

  (* Both the prefix and the suffix are green. *)
  | Ok p1, Ok s1 -> Chain (G, Packet (p1, Hole, s1), Ending b2)

  (* Only the suffix is overflowed. *)
  | Ok p1, Overflow (s1, ab) ->
      (* Excess elements are injected in the child buffer. *)
      let c2 = buffer_inject b2 ab in
      Chain (G, Packet (p1, Hole, s1), c2)

  (* The prefix is overflowed, the suffix is underflowed. Similar to the
     symmetric case. *)
  | Overflow (p1, cd), Underflow s1 ->
      let center, ab = prefix_rot cd b2 in
      Chain (G, Packet (p1, Hole, suffix23 ab s1), Ending center)

  (* Only the prefix is overflowed. Similar to the symmetric case. *)
  | Overflow (p1, cd), Ok s1 ->
      let c2 = buffer_push cd b2 in
      Chain (G, Packet (p1, Hole, s1), c2)

  (* Both the prefix and the suffix are overflowed. *)
  | Overflow (p1, cd), Overflow (s1, ab) ->
    let x, rest = buffer_halve b2 in
    let p2 = suffix12 cd x in
      Chain (G, Packet (p1, Packet (p2, Hole, B1 ab), s1), Ending rest)

(** Makes a red chain green. *)
let green_of_red
: type a. (a, red) chain -> (a, green) chain
= function
  (* If the red node comes just before the ending, a lot of different cases are
     handled by [make_small]. *)
  | Chain (R, Packet (p1, Hole, s1), Ending buf) ->
      make_small p1 buf s1
  (* Otherwise, the prefix and suffix of the following node are used to make
     the prefix and the suffix of the red node green. *)
  | Chain (R, Packet (p1, Hole, s1), Chain (G, Packet (p2, child, s2), c)) ->
      let p1, p2 = green_prefix_concat p1 p2 in
      let s2, s1 = green_suffix_concat s2 s1 in
      Chain (G, Packet (p1, Packet (p2, child, s2), s1), c)
  | Chain (R, Packet (p1, Packet (p2, child, s2), s1), c) ->
      let p1, p2 = prefix_concat p1 (to_yellow p2) in
      let s2, s1 = suffix_concat (to_yellow s2) s1 in
      Chain (G, Packet (p1, Hole, s1), Chain (R, Packet (p2, child, s2), c))

(** Makes a green or red chain green. *)
let ensure_green
: type a g r. (a, g * noyellow * r) chain -> (a, green) chain
= function
  | Ending b          -> Ending b
  | Chain (G, pkt, c) -> Chain (G, pkt, c)
  | Chain (R, pkt, c) -> green_of_red (Chain (R, pkt, c))

(** Takes a prefix non-red buffer, a child packet and a suffix non-red buffer,
    and a following green or red chain, and makes a deque out of them. *)
let make_yellow p1 child s1 c =
  T (Chain (Y, Packet (to_yellow p1, child, to_yellow s1), ensure_green c))

(** Takes a prefix buffer of any color, a child packet and a suffix buffer of
    any color, and a following green chain, and makes a deque out of them. *)
let make_red p1 child s1 c =
  T (green_of_red (Chain (R, Packet (to_red p1, child, to_red s1), c)))

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(** The empty deque. *)
let empty = T (Ending B0)

(** Emptyness test for deques. *)
let is_empty d = d = empty

(** Pushes on a deque. *)
let push x (T t) = match t with
  | Ending b -> T (buffer_push x b)
  | Chain (G, Packet (p1, child, s1), c) ->
      let p1 = green_push x p1 in
      make_yellow p1 child s1 c
  | Chain (Y, Packet (p1, child, s1), c) ->
      let p1 = yellow_push x p1 in
      make_red p1 child s1 c

(** Injects on a deque. *)
let inject (T t) x = match t with
  | Ending b -> T (buffer_inject b x)
  | Chain (G, Packet (p1, child, s1), c) ->
      let s1 = green_inject s1 x in
      make_yellow p1 child s1 c
  | Chain (Y, Packet (p1, child, s1), c) ->
      let s1 = yellow_inject s1 x in
      make_red p1 child s1 c

(** Pops off a deque. *)
let pop (T t) = match t with
  | Ending b ->
    begin match buffer_pop b with
    | None -> None
    | Some (x, b) -> Some (x, T (Ending b))
    end
  | Chain (G, Packet (p1, child, s1), c) ->
    let x, p1 = green_pop p1 in
    Some (x, make_yellow p1 child s1 c)
  | Chain (Y, Packet (p1, child, s1), c) ->
    let x, p1 = yellow_pop p1 in
    Some (x, make_red p1 child s1 c)

(** Ejects off a deque. *)
let eject (T t) = match t with
  | Ending b ->
    begin match buffer_eject b with
    | None -> None
    | Some (b, x) -> Some (T (Ending b), x)
    end
  | Chain (G, Packet (p1, child, s1), c) ->
    let s1, x = green_eject s1 in
    Some (make_yellow p1 child s1 c, x)
  | Chain (Y, Packet (p1, child, s1), c) ->
    let s1, x = yellow_eject s1 in
    Some (make_red p1 child s1 c, x)

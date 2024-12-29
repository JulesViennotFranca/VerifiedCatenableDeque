open Color.GYOR

module Deque = Deque.Core

(* Support for natural number types. *)

type    z
type 'a s

type 'a ge1 = 'a s
type 'a ge2 = 'a s ge1
type 'a ge3 = 'a s ge2
type 'a ge4 = 'a s ge3
type 'a ge5 = 'a s ge4
type 'a ge6 = 'a s ge5
type 'a ge7 = 'a s ge6
type 'a ge8 = 'a s ge7

type eq0 = z
type eq1 = z s
type eq2 = z ge2
type eq6 = z ge6

(* Some tuple renaming. *)

type 'a four  = 'a * 'a * 'a * 'a
type 'a five  = 'a * 'a * 'a * 'a * 'a
type 'a six   = 'a * 'a * 'a * 'a * 'a * 'a
type 'a eight = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

(* +------------------------------------------------------------------------+ *)
(* |                                Vectors                                 | *)
(* +------------------------------------------------------------------------+ *)

(** A type for vector of size 0 to 6. The second type parameter will always be a
    natural number and represents the maximum number of elements the vector can
    contain. *)
type ('a, 'upperbound) vector =
  | V0 : ('a, 'n) vector
  | V1 : 'a -> ('a, 'n ge1) vector
  | V2 : 'a * 'a -> ('a, 'n ge2) vector
  | V3 : 'a * 'a * 'a -> ('a, 'n ge3) vector
  | V4 : 'a * 'a * 'a * 'a -> ('a, 'n ge4) vector
  | V5 : 'a * 'a * 'a * 'a * 'a -> ('a, 'n ge5) vector
  | V6 : 'a * 'a * 'a * 'a * 'a * 'a -> ('a, 'n ge6) vector

(** Folds right on vectors. *)
let vector_fold_right
: type z a n. (a -> z -> z) -> (a, n) vector -> z -> z
= fun fn v z -> match v with
  | V0 -> z
  | V1 a -> fn a z
  | V2 (a, b) -> fn a (fn b z)
  | V3 (a, b, c) -> fn a (fn b (fn c z))
  | V4 (a, b, c, d) -> fn a (fn b (fn c (fn d z)))
  | V5 (a, b, c, d, e) -> fn a (fn b (fn c (fn d (fn e z))))
  | V6 (a, b, c, d, e, f) -> fn a (fn b (fn c (fn d (fn e (fn f z)))))

(** Folds left on vectors. *)
let vector_fold_left
: type z a n. (z -> a -> z) -> z -> (a, n) vector -> z
= fun fn z v -> match v with
  | V0 -> z
  | V1 a -> fn z a
  | V2 (a, b) -> fn (fn z a) b
  | V3 (a, b, c) -> fn (fn (fn z a) b) c
  | V4 (a, b, c, d) -> fn (fn (fn (fn z a) b) c) d
  | V5 (a, b, c, d, e) -> fn (fn (fn (fn (fn z a) b) c) d) e
  | V6 (a, b, c, d, e, f) -> fn (fn (fn (fn (fn (fn z a) b) c) d) e) f

(* +------------------------------------------------------------------------+ *)
(* |                                Buffers                                 | *)
(* +------------------------------------------------------------------------+ *)

module Buffer : sig

  (** The type of a buffer is parameterized by the type ['a] of
      the elements of the buffer and by a lower bound ['n] on
      the number of elements of the buffer. *)
  type ('a, 'n) t

  (* Different operations needed on buffers, and how they change the size of
     the buffer. *)

  val empty : ('a, z) t

  val push : 'a -> ('a, 'n) t -> ('a, 'n s) t
  val inject : ('a, 'n) t -> 'a -> ('a, 'n s) t
  val pop : ('a, 'n s) t -> 'a * ('a, 'n) t
  val eject : ('a, 'n s) t -> ('a, 'n) t * 'a

  val push2 : 'a * 'a -> ('a, 'n) t -> ('a, 'n s s) t
  val inject2 : ('a, 'n) t -> 'a * 'a -> ('a, 'n s s) t
  val pop2 : ('a, 'n s s) t -> 'a * 'a * ('a, 'n) t
  val eject2 : ('a, 'n s s) t -> ('a, 'n) t * 'a * 'a
  val two : ('a, eq2) t -> 'a * 'a

  val single : 'a -> ('a, z s) t
  val pair   : 'a -> 'a -> ('a, z s s) t

  val push3 : 'a * 'a * 'a -> ('a, 'n) t -> ('a, 'n s s s) t
  val inject3 : ('a, 'n) t -> 'a * 'a * 'a -> ('a, 'n s s s) t

  val push_5vector : 'a five * ('a, _) vector -> ('a, 'n) t -> ('a, 'n ge5) t
  val inject_5vector : ('a, 'n) t -> 'a five * ('a, _) vector -> ('a, 'n ge5) t

  val push6 : 'a six -> ('a, 'n) t -> ('a, 'n ge6) t
  val inject6 : ('a, 'n) t -> 'a six -> ('a, 'n ge6) t

  val inject8 : ('a, 'n) t -> 'a eight -> ('a, 'n ge8) t

  val push_vector : ('a, _) vector -> ('a, 'n) t -> ('a, 'n) t
  val inject_vector : ('a, 'n) t -> ('a, _) vector -> ('a, 'n) t

  (** A type storing either 0 element or a buffer with at least 1 element. *)
  type _ has1 =
    | Exact_0 : 'a has1
    | At_least_1 : ('a, _ ge1) t -> 'a has1
  (** Tells if a given buffer is empty or not. *)
  val has1 : ('a, 'n) t -> 'a has1

  (** A type storing either 0, 1 or 2 elements, or a buffer with at least 3
      elements. *)
  type 'a has3 =
    | Less_than_3 : ('a, eq2) vector -> 'a has3
    | At_least_3 : ('a, _ ge3) t -> 'a has3

  (** Tells if a given buffer of at least 3 elements has 3, 4, 5 elements or
      more than 6. *)
  val has3p : ('a, _ ge3) t -> ('a * 'a * 'a) * 'a has3

  (** Tells if a given buffer of at least 3 elements has 3, 4, 5 elements or
      more than 6. *)
  val has3s : ('a, _ ge3) t -> 'a has3 * ('a * 'a * 'a)

  (** A type storing either 4 elements or a buffer with at least 5 elements. *)
  type 'a has5 =
    | Exact_4 : 'a four -> 'a has5
    | At_least_5 : ('a, _ ge5) t -> 'a has5

  (** Tells if a given buffer of at least 4 elements has just 4 elements or
      more. *)
  val has5   : ('a, _ ge4) t -> 'a has5

  (** A type storing 6 elements or less or a buffer of at least 7 elements. *)
  type 'a has7 =
    | Less_than_7 : ('a, eq6) vector -> 'a has7
    | At_least_7 : ('a, _ ge7) t -> 'a has7

  (** Tells if a given buffer has 6 elements or less, or if it has more than 7
      elements. *)
  val has7 : ('a, _ ge1) t -> 'a has7

  (** A type storing either 5, 6 or 7 elements, or a buffer with at least 8
      elements. *)
  type 'a has8 =
    | Less_than_8 : ('a five * ('a, eq2) vector) -> 'a has8
    | At_least_8 : ('a, _ ge8) t -> 'a has8

  (** Tells if a given buffer of at least 5 elements has 5, 6, 7 elements or
      more than 8. *)
  val has8   : ('a, _ ge5) t -> 'a has8

  (** A type storing 8, 9 or 10 elements, or a buffer of 3 elements and a
      buffer of at least 8 elements. *)
  type 'a has3p8 =
    | Less_than_11 : 'a eight * ('a, eq2) vector -> 'a has3p8
    | At_least_11 : ('a, z ge3) t * ('a, _ ge8) t -> 'a has3p8

  (** Tells if a given buffer of at least 8 elements has 8, 9 or 10 elements,
      or if it has at least 11 elements. In the latter case, returns a buffer of
      3 elements and a buffer of at least 8 elements. *)
  val has3p8 : ('a, _ ge8) t -> 'a has3p8

end = struct
  type ('a, 'quantity) t = 'a Deque.deque

  let empty = Deque.empty
  let push x t = Deque.push x t
  let inject t x = Deque.inject t x

  let pop t = match Deque.pop t with
    | None -> assert false
    | Some (x, t') -> (x, t')
  let eject t = match Deque.eject t with
    | None -> assert false
    | Some (t', x) -> (t', x)

  let single x = push x empty
  let pair x y = push x (single y)

  let pop2 t =
    let x, t = pop t in
    let y, t = pop t in
    x, y, t

  let eject2 t =
    let t, x = eject t in
    let t, y = eject t in
    t, y, x

  let two t =
    let x, y, t = pop2 t in
    assert (Deque.is_empty t) ;
    (x, y)

  type _ has1 =
    | Exact_0 : 'a has1
    | At_least_1 : ('a, _ ge1) t -> 'a has1

  let has1 t =
    if Deque.is_empty t
    then Exact_0
    else At_least_1 t

  let push2 (a, b) t = push a (push b t)
  let inject2 t (a, b) = inject (inject t a) b

  let push3 (a, b, c) t = push a (push2 (b, c) t)
  let inject3 t (a, b, c) = inject (inject2 t (a, b)) c
  let pop3 t = let a, b, t = pop2 t in let c, t = pop t in ((a, b, c), t)
  let eject3 t = let t, b, c = eject2 t in let t, a = eject t in (t, (a, b, c))

  let push6 (a, b, c, d, e, f) t =
    push a (push b (push c (push d (push e (push f t)))))
  let inject6 t (a, b, c, d, e, f) =
    inject (inject (inject (inject (inject (inject t a) b) c) d) e) f

  type 'a has3 =
    | Less_than_3 : ('a, eq2) vector -> 'a has3
    | At_least_3 : ('a, _ ge3) t -> 'a has3

  let has3 t =
    match Deque.pop t with
    | None -> Less_than_3 V0
    | Some (x, s) ->
    match Deque.pop s with
    | None -> Less_than_3 (V1 x)
    | Some (y, s) ->
    match Deque.pop s with
    | None -> Less_than_3 (V2 (x, y))
    | Some _ -> At_least_3 t

  type 'a has8 =
    | Less_than_8 : ('a five * ('a, eq2) vector) -> 'a has8
    | At_least_8 : ('a, _ ge8) t -> 'a has8

  let pop5 t =
    let a, b, t = pop2 t in
    let c, d, t = pop2 t in
    let e, t = pop t in
    (a, b, c, d, e), t

  let has8 buffer =
    let five, t = pop5 buffer in
    match has3 t with
    | Less_than_3 vec -> Less_than_8 (five, vec)
    | At_least_3 _ -> At_least_8 buffer

  let has3p buf =
    let three, buf = pop3 buf in
    three, has3 buf

  let has3s buf =
    let buf, three = eject3 buf in
    has3 buf, three

  type 'a has7 =
    | Less_than_7 : ('a, eq6) vector -> 'a has7
    | At_least_7 : ('a, _ ge7) t -> 'a has7

  let has7 t =
    let a, s = pop t in
    match Deque.pop s with
    | None -> Less_than_7 (V1 a)
    | Some (b, s) ->
    match Deque.pop s with
    | None -> Less_than_7 (V2 (a, b))
    | Some (c, s) ->
    match Deque.pop s with
    | None -> Less_than_7 (V3 (a, b, c))
    | Some (d, s) ->
    match Deque.pop s with
    | None -> Less_than_7 (V4 (a, b, c, d))
    | Some (e, s) ->
    match Deque.pop s with
    | None -> Less_than_7 (V5 (a, b, c, d, e))
    | Some (f, s) ->
    match Deque.pop s with
    | None -> Less_than_7 (V6 (a, b, c, d, e, f))
    | Some _ -> At_least_7 t

  type 'a has5 =
    | Exact_4 : 'a four -> 'a has5
    | At_least_5 : ('a, _ ge5) t -> 'a has5

  let has5 buffer =
    let a, b, t = pop2 buffer in
    let c, d, t = pop2 t in
    match has1 t with
    | Exact_0 -> Exact_4 (a, b, c, d)
    | At_least_1 _ -> At_least_5 buffer

  let push_vector v t = vector_fold_right Deque.push v t
  let inject_vector t v = vector_fold_left Deque.inject t v

  type 'a has3p8 =
    | Less_than_11 : 'a eight * ('a, eq2) vector -> 'a has3p8
    | At_least_11 : ('a, z ge3) t * ('a, _ ge8) t -> 'a has3p8

  let has3p8 t =
    let (a, b, c), (t as t8) = pop3 t in
    let d, e, t = pop2 t in
    let (f, g, h), t = pop3 t in
    match has3 t with
    | Less_than_3 vec -> Less_than_11 ((a, b, c, d, e, f, g, h), vec)
    | At_least_3 _ ->
      let t3 = push a (pair b c) in
      At_least_11 (t3, t8)

  let push5 (a, b, c, d, e) t =
    push a (push b (push c (push d (push e t))))

  let inject5 t (a, b, c, d, e) =
    inject (inject (inject (inject (inject t a) b) c) d) e

  let push_5vector (five, v) t = push5 five (push_vector v t)
  let inject_5vector t (five, v) = inject_vector (inject5 t five) v

  let inject8 t (a, b, c, d, e, f, g, h) =
    inject2 (inject2 (inject2 (inject2 t (a, b)) (c, d)) (e, f)) (g, h)
end

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Prefixes and suffixes are simply buffers. *)

type ('a, 'n) prefix = ('a, 'n) Buffer.t
type ('a, 'n) suffix = ('a, 'n) Buffer.t

(* Types for different kinds of triples and chains. *)

type only
type left
type right

type empty  = eq0
type single = eq1
type pair   = eq2

(** The coloring links a color to the size of a prefix and the size of a suffix. *)
type ('prefix_delta, 'suffix_delta, 'arity, 'color) coloring =
  | Gc : (_ ge3, _ ge3, _ ge1, green ) coloring
  | Yc : (_ ge2, _ ge2, _ ge1, yellow) coloring
  | Oc : (_ ge1, _ ge1, _ ge1, orange) coloring
  | Rc : (    _,     _, _ ge1, red   ) coloring
  | Ec : (    _,     _,   eq0, green ) coloring

(** A node represents a prefix - suffix pair.

    [only] node follow the coloring constraints linking the prefix size, the
    suffix size and the color, except for the ending one, representing solely a
    prefix of at least one element.

    [left] ([right]) node follow the coloring constraints only for the size
    of the prefix (suffix), the suffix (prefix) containing two elements. The
    prefix (suffix) of an ending left (right) node must contain at least
    five elements. *)
type ('a, 'arity, 'kind, 'color) node =
  | Only_end  : ('a, _ ge1) prefix -> ('a, eq0, only, green) node
  | Only  : ('pdelta, 'sdelta, 'n ge1, 'c) coloring
          * ('a, 'pdelta ge5) prefix * ('a, 'sdelta ge5) suffix
         -> ('a, 'n ge1, only, 'c) node
  | Left  : ('pdelta, _, 'arity, 'c) coloring
          * ('a, 'pdelta ge5) prefix * ('a, eq2) suffix
         -> ('a, 'arity, left, 'c) node
  | Right : (_, 'sdelta, 'arity, 'c) coloring
          * ('a, eq2) prefix * ('a, 'sdelta ge5) suffix
         -> ('a, 'arity, right, 'c) node

(** Regularity represents constraints between a node color and its child chain
    parameters. The second parameter keeps track of the color of the single
    chain represented by the node and its child chain. *)
type ('color_top, 'color_chain, 'ckind, 'color_left, 'color_right) regularity =
  | G  : (green , green,      _,     _,     _) regularity
  | Y  : (yellow,   'cl,  _ ge1,   'cl,     _) regularity
  | OS : (orange,    'c, single,    'c,    'c) regularity
  | OP : (orange,   'cr,   pair, green,   'cr) regularity
  | R  : (red   ,   red,  _ ge1, green, green) regularity

(** A stored triple is either small, and made of one buffer, or big, and made
    of prefix - child - suffix triple. *)
type 'a stored_triple =
  | Small : ('a, _ ge3) prefix -> 'a stored_triple
  | Big : ('a, _ ge3) prefix
        * ('a stored_triple, _, only, _, _) chain
        * ('a, _ ge3) suffix
       -> 'a stored_triple

(** A body represents a descending preferred path : it follows yellow and
    orange nodes according to the preferred child relation. A body always end
    with a [Hole]. Its parameters are its input and output types, and its
    input and output kinds. *)
and ('a, 'b, 'head_nkind, 'tail_nkind) body =
  | Hole : ('a, 'a, 'nkind, 'nkind) body
  | Single_child :
       ('a, eq1, 'head_nkind, nogreen * _ * _ * nored) node
     * ('a stored_triple, 'b, only, 'tail_nkind) body
    -> ('a, 'b, 'head_nkind, 'tail_nkind) body
  | Pair_yellow :
       ('a, eq2, 'head_nkind, yellow) node
     * ('a stored_triple, 'b, left, 'tail_nkind) body
     * ('a stored_triple, single, right, 'c, 'c) chain
    -> ('a, 'b, 'head_nkind, 'tail_nkind) body
  | Pair_orange :
       ('a, eq2, 'head_nkind, orange) node
     * ('a stored_triple, single, left, green, green) chain
     * ('a stored_triple, 'b, right, 'tail_nkind) body
    -> ('a, 'b, 'head_nkind, 'tail_nkind) body

(** A packet represents a preferred path and its last node. As the last node is
    not yellow or orange, it is necessarily green or red. The last node take
    the place of the body's hole. Its parameter are its input and output types,
    its input kind, wether or not its last node is an ending one, and the color
    of its last node. *)
and ('a, 'b, 'arity, 'nkind, 'color) packet =
  | Packet :
       ('a, 'b, 'nkind, 'tail_nkind) body
     * ('b, 'arity, 'tail_nkind, _ * noyellow * noorange * _ as 'c) node
    -> ('a, 'b stored_triple, 'arity, 'nkind, 'c) packet

(** A chain represents a semi-regular cadeque with a lot of additional
    information. The first parameter is simply the input type of the cadeque.

    The second parameter concerns the shape of the chain. [single] means that
    the chain starts with one branch, [pair] means that the chain starts with a
    left and a right branch.

    The third parameter is the kind of the chain. Here, this parameter is used
    to describe two concepts. First, as for other types, the kind represents
    wether the top node of the chain is an only, a left or a right one. But
    this only make sense for only chains, as pair chains have two top nodes.
    Hence, the kind of pair chains is considered to be [only]. In fact, the
    [only] kind also tells us that this chain can be a child of a node. [left]
    and [right] chains can solely be only chains, and alone, they cannot be the
    child of a node.

    The fourth parameter is straight forward, it differentiates the empty chain
    from others. Thanks to it, a ending packet is necessarily followed by the
    empty chain.

    The last two parameters concerns the chain coloring. In the case of a pair
    chain, two colors are needed to know the color of the left path and the
    color of the right path. If the chain is only, the left and right colors
    are the same, the color of its only path. *)
and ('a, 'ckind, 'nkind, 'color_left, 'color_right) chain =
  | Empty : ('a, empty, only, green, green) chain
  | Single : ('c, 'c, 'arity, 'cl, 'cr) regularity
           * ('a, 'b, 'arity, 'nk, 'c) packet
           * ('b, 'arity, only, 'cl, 'cr) chain
          -> ('a, single, 'nk, 'c, 'c) chain
  | Pair : ('a, single, left , 'cl, 'cl) chain
         * ('a, single, right, 'cr, 'cr) chain
        -> ('a, pair, only, 'cl, 'cr) chain

(** A type representing prefix and suffix of at least 3 elements. *)
type _ stored_buffer =
  | Stored_buffer : ('a, _ ge3) Buffer.t -> 'a stored_buffer

(** A type for the triple representation of a non-empty cadeque. First comes the
    regularity constraints, then the prefix and suffix as a node, then the
    child cadeque as a chain. *)
type ('a, 'nkind, 'color_chain) triple =
  | Triple :
       ('c, 'cc, 'arity, 'cl, 'cr) regularity
     * ('a, 'arity, 'nk, 'c) node
     * ('a stored_triple, 'arity, only, 'cl, 'cr) chain
    -> ('a, 'nk, 'cc) triple

(** A type used to represent left or right triples. If there is not enough
    elements to make one, they are stored in a vector. *)
type (_, _, _) left_right_triple =
  | Not_enough : ('a, eq6) vector -> ('a, _, green) left_right_triple
  | Ok : ('a, 'k, 'c) triple -> ('a, 'k, 'c) left_right_triple

(** A type used to represent triples after a pop or eject operation. If the
    remaining triple is still valid, it is stored as it is. If not, it means it
    remains six elements if it was a left or right triple, or noorange element if it
    was an only triple. The [pair] parameter is used to differentiate between
    those two possible cases. It translates to : was the modified triple part
    of a pair or not. *)
type ('a, 'ckind, 'nkind) partial_triple =
  | Empty : ('a, single, _) partial_triple
  | End : 'a six -> ('a, pair, _) partial_triple
  | Ok : ('a, 'nk, _) triple -> ('a, _, 'nk) partial_triple

(** The sandwich type contains either one element of type [exter] or an element
    of type [inter] sandwiched into two elements of type [exter]. *)
type ('exter, 'inter) sandwich =
  | Alone : 'exter -> ('exter, _) sandwich
  | Sandwich : 'exter * 'inter * 'exter -> ('exter, 'inter) sandwich

(** A type for semi-regular cadeques. *)
type 'a semi_cadeque = S : ('a, _, only, _, _) chain -> 'a semi_cadeque

(** A type for regular cadeques. *)
type 'a cadeque = T : ('a, _, only, green, green) chain -> 'a cadeque

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(** Pushes on a left node. *)
let push_left_node
: type a ck c. a -> (a, ck, left, c) node -> (a, ck, left, c) node
= fun x store ->
  match store with
  | Left (Gc, p, s) -> Left (Gc, Buffer.push x p, s)
  | Left (Yc, p, s) -> Left (Yc, Buffer.push x p, s)
  | Left (Oc, p, s) -> Left (Oc, Buffer.push x p, s)
  | Left (Rc, p, s) -> Left (Rc, Buffer.push x p, s)
  | Left (Ec, p, s) -> Left (Ec, Buffer.push x p, s)

(** Injects on a right node. *)
let inject_right_node
: type a ck c. (a, ck, right, c) node -> a -> (a, ck, right, c) node
= fun store x ->
  match store with
  | Right (Gc, p, s) -> Right (Gc, p, Buffer.inject s x)
  | Right (Yc, p, s) -> Right (Yc, p, Buffer.inject s x)
  | Right (Oc, p, s) -> Right (Oc, p, Buffer.inject s x)
  | Right (Rc, p, s) -> Right (Rc, p, Buffer.inject s x)
  | Right (Ec, p, s) -> Right (Ec, p, Buffer.inject s x)

(** Pushes on an only node. *)
let push_only_node
: type a ck c. a -> (a, ck, only, c) node -> (a, ck, only, c) node
= fun x store ->
  match store with
  | Only (Gc, p, s) -> Only (Gc, Buffer.push x p, s)
  | Only (Yc, p, s) -> Only (Yc, Buffer.push x p, s)
  | Only (Oc, p, s) -> Only (Oc, Buffer.push x p, s)
  | Only (Rc, p, s) -> Only (Rc, Buffer.push x p, s)
  | Only_end p -> Only_end (Buffer.push x p)

(** Injects on an only node. *)
let inject_only_node
: type a ck c. (a, ck, only, c) node -> a -> (a, ck, only, c) node
= fun store x ->
  match store with
  | Only (Gc, p, s) -> Only (Gc, p, Buffer.inject s x)
  | Only (Yc, p, s) -> Only (Yc, p, Buffer.inject s x)
  | Only (Oc, p, s) -> Only (Oc, p, Buffer.inject s x)
  | Only (Rc, p, s) -> Only (Rc, p, Buffer.inject s x)
  | Only_end p -> Only_end (Buffer.inject p x)

(** Pushes on a left packet. *)
let push_left_packet
: type a b ck c. a -> (a, b, ck, left, c) packet -> (a, b, ck, left, c) packet
= fun x (Packet (body, tail)) ->
  match body with
  | Hole -> Packet (Hole, push_left_node x tail)
  | Single_child (head, body) ->
      Packet (Single_child (push_left_node x head, body), tail)
  | Pair_yellow (head, body, right) ->
      Packet (Pair_yellow (push_left_node x head, body, right), tail)
  | Pair_orange (head, left, body) ->
      Packet (Pair_orange (push_left_node x head, left, body), tail)

(** Injects on a right packet. *)
let inject_right_packet
: type a b ck c. (a, b, ck, right, c) packet -> a -> (a, b, ck, right, c) packet
= fun (Packet (body, tail)) x ->
  match body with
  | Hole -> Packet (Hole, inject_right_node tail x)
  | Single_child (head, body) ->
      Packet (Single_child (inject_right_node head x, body), tail)
  | Pair_yellow (head, body, right) ->
      Packet (Pair_yellow (inject_right_node head x, body, right), tail)
  | Pair_orange (head, left, body) ->
      Packet (Pair_orange (inject_right_node head x, left, body), tail)

(** Pushes on an only packet. *)
let push_only_packet
: type a b ck c. a -> (a, b, ck, only, c) packet -> (a, b, ck, only, c) packet
= fun x (Packet (body, tail)) ->
  match body with
  | Hole -> Packet (Hole, push_only_node x tail)
  | Single_child (head, body) ->
      Packet (Single_child (push_only_node x head, body), tail)
  | Pair_yellow (head, body, right) ->
      Packet (Pair_yellow (push_only_node x head, body, right), tail)
  | Pair_orange (head, left, body) ->
      Packet (Pair_orange (push_only_node x head, left, body), tail)

(** Injects on an only packet. *)
let inject_only_packet
: type a b ck c. (a, b, ck, only, c) packet -> a -> (a, b, ck, only, c) packet
= fun (Packet (body, tail)) x ->
  match body with
  | Hole -> Packet (Hole, inject_only_node tail x)
  | Single_child (head, body) ->
      Packet (Single_child (inject_only_node head x, body), tail)
  | Pair_yellow (head, body, right) ->
      Packet (Pair_yellow (inject_only_node head x, body, right), tail)
  | Pair_orange (head, left, body) ->
      Packet (Pair_orange (inject_only_node head x, left, body), tail)

(** Returns an only node containing one element. *)
let single_node x = Only_end (Buffer.single x)

(** Returns a packet containing one element. *)
let single_packet x = Packet (Hole, single_node x)

(** Returns a chain containing one element. *)
let single_chain x = Single (G, single_packet x, Empty)

(** Pushes on a left chain. *)
let push_left_chain x c = match c with
  | Single (reg, pkt, c) -> Single (reg, push_left_packet x pkt, c)

(** Injects on a right chain. *)
let inject_right_chain c x = match c with
  | Single (reg, pkt, c) -> Single (reg, inject_right_packet pkt x, c)

(** Pushes on a non-empty only chain. *)
let push_ne_chain
: type a n cl cr. a -> (a, n ge1, only, cl, cr) chain
                    -> (a, n ge1, only, cl, cr) chain
= fun x c ->
  match c with
  | Single (reg, pkt, c) -> Single (reg, push_only_packet x pkt, c)
  | Pair (cl, cr) -> Pair (push_left_chain x cl, cr)

(** Injects on a non-empty only chain. *)
let inject_ne_chain
: type a n cl cr. (a, n ge1, only, cl, cr) chain -> a
               -> (a, n ge1, only, cl, cr) chain
= fun c x ->
  match c with
  | Single (reg, pkt, c) -> Single (reg, inject_only_packet pkt x, c)
  | Pair (cl, cr) -> Pair (cl, inject_right_chain cr x)

(** Discerns when its type parameter is empty or not. *)
type _ is_empty =
  | Is_empty  :  empty  is_empty
  | Not_empty : (_ ge1) is_empty

(** Tells if a chain is empty or not. *)
let is_empty : type a ck nk cl cr. (a, ck, nk, cl, cr) chain -> ck is_empty
= function Empty -> Is_empty
  | Single _ -> Not_empty | Pair _ -> Not_empty

(** Pushes on a semi-regular cadeque. *)
let semi_push x (S c) = match is_empty c, c with
  | Is_empty, Empty -> S (single_chain x)
  | Not_empty, c -> S (push_ne_chain x c)

(** Injects on a semi-regular cadeque. *)
let semi_inject (S c) x = match is_empty c, c with
  | Is_empty, Empty -> S (single_chain x)
  | Not_empty, c -> S (inject_ne_chain c x)

(** Returns the regularity rule associated to a yellow or orange node with one
    child. *)
let to_reg
: type a nk y o.
     (a, single, nk, nogreen * y * o * nored as 'c) node
  -> ('c, _, single, _, _) regularity
= function
  | Only  (Yc, _, _) -> Y | Only  (Oc, _, _) -> OS
  | Left  (Yc, _, _) -> Y | Left  (Oc, _, _) -> OS
  | Right (Yc, _, _) -> Y | Right (Oc, _, _) -> OS

(** Returns the triple representation of a non-empty only chain. *)
let triple_of_chain
: type a k c. (a, single, k, c, c) chain -> (a, k, c) triple
= function
  | Single (G, Packet (Hole, tl), child) -> Triple (G, tl, child)
  | Single (reg, Packet (Single_child (hd, bd), tl), child) ->
    Triple (to_reg hd, hd, Single (reg, Packet (bd, tl), child))
  | Single (reg, Packet (Pair_yellow (hd, bd, cr), tl), child) ->
    Triple (Y, hd, Pair (Single (reg, Packet (bd, tl), child), cr))
  | Single (reg, Packet (Pair_orange (hd, cl, bd), tl), child) ->
    Triple (OP, hd, Pair (cl, Single (reg, Packet (bd, tl), child)))
  | Single (R, Packet (Hole, tl), child) -> Triple (R, tl, child)

(** Returns the non-empty only chain associated to a triple. *)
let chain_of_triple
: type a nk c. (a, nk, c) triple -> (a, single, nk, c, c) chain
= function
  | Triple (G, hd, child) -> Single (G, Packet (Hole, hd), child)
  | Triple (Y, hd, Single (reg, Packet (bd, tl), child)) ->
    Single (reg, Packet (Single_child (hd, bd), tl), child)
  | Triple (Y, hd, Pair (Single (reg, Packet (bd, tl), child), cr)) ->
    Single (reg, Packet (Pair_yellow (hd, bd, cr), tl), child)
  | Triple (OS, hd, Single (reg, Packet (bd, tl), child)) ->
    Single (reg, Packet (Single_child (hd, bd), tl), child)
  | Triple (OP, hd, Pair (cl, Single (reg, Packet (bd, tl), child))) ->
    Single (reg, Packet (Pair_orange (hd, cl, bd), tl), child)
  | Triple (R, hd, child) -> Single (R, Packet (Hole, hd), child)

(** Makes a left [left_right_triple] out of an only triple. *)
let left_of_only
: type a c. (a, only, c) triple -> (a, left, c) left_right_triple
= function
  | Triple (G, Only_end p, Empty) ->
    begin match Buffer.has7 p with
    | Less_than_7 v -> Not_enough v
    | At_least_7 p ->
      let p, y, x = Buffer.eject2 p in
      let s = Buffer.pair y x in
      Ok (Triple (G, Left (Ec, p, s), Empty))
    end
  | Triple (reg, Only (coloring, p, s), child) ->
    let s', y, x = Buffer.eject2 s in
    let s = Buffer.pair y x in
    let child = inject_ne_chain child (Small s') in
    Ok (Triple (reg, Left (coloring, p, s), child))

(** Makes a right [left_right_triple] out of an only triple. *)
let right_of_only
: type a c. (a, only, c) triple -> (a, right, c) left_right_triple
= function
  | Triple (G, Only_end s, Empty) ->
    begin match Buffer.has7 s with
    | Less_than_7 v -> Not_enough v
    | At_least_7 s ->
      let x, y, s = Buffer.pop2 s in
      let p = Buffer.pair x y in
      Ok (Triple (G, Right (Ec, p, s), Empty))
    end
  | Triple (reg, Only (coloring, p, s), child) ->
    let x, y, p' = Buffer.pop2 p in
    let p = Buffer.pair x y in
    let child = push_ne_chain (Small p') child in
    Ok (Triple (reg, Right (coloring, p, s), child))

(** Takes a suffix of at least one element, a right prefix, a child chain and a
    right suffix, and returns a stored triple and a left suffix. *)
let make_stored_suffix sleft p child s =
  let pelt = Buffer.two p in
  let p = Buffer.inject2 sleft pelt in
  let s, y, x = Buffer.eject2 s in
  let sleft = Buffer.pair y x in
  (Big (p, child, s), sleft)

(** Takes a left prefix, a child chain, a left suffix and a prefix of at least
    one elements, and returns a right prefix and a stored_triple. *)
let make_prefix_stored p child s pright =
  let selt = Buffer.two s in
  let s = Buffer.push2 selt pright in
  let x, y, p = Buffer.pop2 p in
  let pright = Buffer.pair x y in
  (pright, Big (p, child, s))

(** Takes a suffix of at least one element and a right triple and returns a
    stored triple and a left suffix. *)
let stored_of_right
: type a cr.
     (a, _ ge1) suffix -> (a, right, cr) triple
  -> a stored_triple * (a, eq2) suffix
= fun sl tr ->
  match tr with
  | Triple (_, Right (_, pr, sr), child) ->
    make_stored_suffix sl pr child sr

(** Takes a left triple and a prefix of at least one element and returns a
    right prefix and a stored triple. *)
let stored_of_left
: type a cl.
     (a, left, cl) triple -> (a, _ ge1) prefix
  -> (a, eq2) prefix * a stored_triple
= fun tl pr ->
  match tl with
  | Triple (_, Left (_, pl, sl), child) ->
    make_prefix_stored pl child sl pr

(** Tells if a coloring applies to an ending node or not by looking at the
    emptyness of the child of the node. *)
let is_empty_coloring : type sp ss ck c. (sp, ss, ck, c) coloring -> ck is_empty
= function Ec -> Is_empty
  | Gc -> Not_empty | Yc -> Not_empty | Oc -> Not_empty | Rc -> Not_empty

(** Makes a left triple out of a pair of left and right triples. *)
let left_of_pair
: type a cl cr.
     (a, left , cl) triple
  -> (a, right, cr) triple
  -> (a, left , cl) triple
= fun (Triple (reg, Left (coloring, p, s), child)) tr ->
  match reg, is_empty_coloring coloring with
  | G, Is_empty ->
    let a, s = Buffer.pop s in
    let p = Buffer.inject p a in
    let stored, s = stored_of_right s tr in
    let child = single_chain stored in
    Triple (OS, Left (Oc, p, s), child)
  | reg, Not_empty ->
    let stored, s = stored_of_right s tr in
    let child = inject_ne_chain child stored in
    Triple (reg, Left (coloring, p, s), child)

(** Makes a right triple out of a pair of left and right triples. *)
let right_of_pair
: type a cl cr.
     (a, left , cl) triple
  -> (a, right, cr) triple
  -> (a, right, cr) triple
= fun tl (Triple (reg, Right (coloring, p, s), child)) ->
  match reg, is_empty_coloring coloring with
  | G, Is_empty ->
    let p, a = Buffer.eject p in
    let s = Buffer.push a s in
    let p, stored = stored_of_left tl p in
    let child = single_chain stored in
    Triple (OS, Right (Oc, p, s), child)
  | reg, Not_empty ->
    let p, stored = stored_of_left tl p in
    let child = push_ne_chain stored child in
    Triple (reg, Right (coloring, p, s), child)

(** Makes a left [left_right_triple] out of a chain. *)
let make_left
: type a ck cl cr. (a, ck, only, cl, cr) chain
                -> (a, left, cl) left_right_triple
= function
  | Empty -> Not_enough V0
  | Single _ as c -> left_of_only (triple_of_chain c)
  | Pair (cl, cr) -> Ok (left_of_pair (triple_of_chain cl) (triple_of_chain cr))

(** Makes a right [left_right_triple] out of a chain. *)
let make_right
: type a ck cl cr. (a, ck, only, cl, cr) chain
                -> (a, right, cr) left_right_triple
= function
  | Empty -> Not_enough V0
  | Single _ as c -> right_of_only (triple_of_chain c)
  | Pair (cl, cr) -> Ok (right_of_pair (triple_of_chain cl) (triple_of_chain cr))

(** Concatenates two semi-regular cadeques. *)
let semi_concat (S c1) (S c2) =
  match make_left c1 with
  | Not_enough v -> vector_fold_right semi_push v (S c2)
  | Ok tl ->
    match make_right c2 with
    | Not_enough v -> vector_fold_left semi_inject (S c1) v
    | Ok tr -> S (Pair (chain_of_triple tl, chain_of_triple tr))

(** Returns the orange regularity rule required according to the following
    chain, i.e. if it is an only chain or a pair chain. *)
let orange
: type a n cr.
     (a, n ge1, only, green, cr) chain
  -> (orange, cr, n ge1, green, cr) regularity
= function
  | Single _ -> OS
  | Pair   _ -> OP

(** Pops from a green left triple. *)
let pop_left_green
: type a. (a, left, green) triple -> a * (a, pair, left) partial_triple
= function
  | Triple (reg, Left (coloring, p, s), child) ->
    let a, p = Buffer.pop p in
    match reg, coloring with
    | G , Ec ->
      begin match Buffer.has5 p with
      | Exact_4 (b, c, d, e) ->
        let f, g = Buffer.two s in
        (a, End (b, c, d, e, f, g))
      | At_least_5 p -> (a, Ok (Triple (G, Left (Ec, p, s), Empty)))
      end
    | G , Gc -> (a, Ok (Triple (Y, Left (Yc, p, s), child)))
    | Y , Yc -> (a, Ok (Triple (orange child, Left (Oc, p, s), child)))
    | OS, Oc -> (a, Ok (Triple (R, Left (Rc, p, s), child)))
    | OP, Oc -> (a, Ok (Triple (R, Left (Rc, p, s), child)))

(** Ejects from a green right triple. *)
let eject_right_green
: type a. (a, right, green) triple -> (a, pair, right) partial_triple * a
= function
  | Triple (reg, Right (coloring, p, s), child) ->
    let s, a = Buffer.eject s in
    match reg, coloring with
    | G , Ec ->
      begin match Buffer.has5 s with
      | Exact_4 (e, d, c, b) ->
        let g, f = Buffer.two p in
        (End (g, f, e, d, c, b), a)
      | At_least_5 s -> (Ok (Triple (G, Right (Ec, p, s), Empty)), a)
      end
    | G , Gc -> (Ok (Triple (Y, Right (Yc, p, s), child)), a)
    | Y , Yc -> (Ok (Triple (orange child, Right (Oc, p, s), child)), a)
    | OS, Oc -> (Ok (Triple (R, Right (Rc, p, s), child)), a)
    | OP, Oc -> (Ok (Triple (R, Right (Rc, p, s), child)), a)

(** Pops from an green only triple. *)
let pop_only_green = function
  | Triple (G, Only_end p, Empty) ->
    let a, p = Buffer.pop p in
    begin match Buffer.has1 p with
    | Exact_0 -> (a, Empty)
    | At_least_1 p -> (a, Ok (Triple (G, Only_end p, Empty)))
    end
  | Triple (reg, Only (coloring, p, s), child) ->
    let a, p = Buffer.pop p in
    match reg, coloring with
    | G , Gc -> (a, Ok (Triple (Y, Only (Yc, p, s), child)))
    | Y , Yc -> (a, Ok (Triple (orange child, Only (Oc, p, s), child)))
    | OS, Oc -> (a, Ok (Triple (R, Only (Rc, p, s), child)))
    | OP, Oc -> (a, Ok (Triple (R, Only (Rc, p, s), child)))

(** Ejects from an green only triple. *)
let eject_only_green = function
  | Triple (G, Only_end s, Empty) ->
    let s, a = Buffer.eject s in
    begin match Buffer.has1 s with
    | Exact_0 -> (Empty, a)
    | At_least_1 s -> (Ok (Triple (G, Only_end s, Empty)), a)
    end
  | Triple (reg, Only (coloring, p, s), child) ->
    let s, a = Buffer.eject s in
    match reg, coloring with
    | G , Gc -> (Ok (Triple (Y, Only (Yc, p, s), child)), a)
    | Y , Yc -> (Ok (Triple (orange child, Only (Oc, p, s), child)), a)
    | OS, Oc -> (Ok (Triple (R, Only (Rc, p, s), child)), a)
    | OP, Oc -> (Ok (Triple (R, Only (Rc, p, s), child)), a)

(** Takes an green only triple and represent it as a sandwich. *)
let sandwich_only_green
: type a. (a, only, green) triple
       -> (a, (a, single, only) partial_triple) sandwich
= function
  | Triple (G, Only_end p, Empty) ->
    let a, p = Buffer.pop p in
    begin match Buffer.has1 p with
    | Exact_0 -> Alone a
    | At_least_1 s ->
      let s, z = Buffer.eject s in
      match Buffer.has1 s with
      | Exact_0 -> Sandwich (a, Empty, z)
      | At_least_1 b -> Sandwich (a, Ok (Triple (G, Only_end b, Empty)), z)
    end
  | Triple (reg, Only (coloring, p, s), child) ->
    let a, p = Buffer.pop p in
    let s, z = Buffer.eject s in
    let t = match reg, coloring with
    | G , Gc -> Ok (Triple (Y, Only (Yc, p, s), child))
    | Y , Yc -> Ok (Triple (orange child, Only (Oc, p, s), child))
    | OS, Oc -> Ok (Triple (R, Only (Rc, p, s), child))
    | OP, Oc -> Ok (Triple (R, Only (Rc, p, s), child))
    in
    Sandwich (a, t, z)

(** Adapts a coloring to a prefix of 8 or more elements. *)
let adapt_to_prefix
: type sp sp1 ss1 nc c.
     (sp1, ss1, nc, c) coloring -> (sp ge3, ss1, nc, c) coloring
= function Gc -> Gc | Yc -> Yc | Oc -> Oc | Rc -> Rc | Ec -> Ec

(** Makes an only triple out of six elements and a right triple. *)
let only_of_right
: type a c.
     a six
  -> (a, right, c) triple
  -> (a, only, c) triple
= fun six (Triple (reg, Right (coloring, p, s), child)) ->
  match reg, is_empty_coloring coloring, coloring with
  | G, Is_empty, Ec ->
    let two = Buffer.two p in
    let s = Buffer.push2 two s in
    let s = Buffer.push6 six s in
    Triple (G, Only_end s, child)
  | reg, Not_empty, coloring ->
    let p = Buffer.push6 six p in
    Triple (reg, Only (adapt_to_prefix coloring, p, s), child)

(** Adapts a coloring to a suffix of 8 or more elements. *)
let adapt_to_suffix
: type ss sp1 ss1 nc c.
     (sp1, ss1, nc, c) coloring -> (sp1, ss ge3, nc, c) coloring
= function Gc -> Gc | Yc -> Yc | Oc -> Oc | Rc -> Rc | Ec -> Ec

(** Makes an only triple out of a left triple and six elements. *)
let only_of_left
: type a c.
     (a, left, c) triple
  -> a six
  -> (a, only, c) triple
= fun (Triple (reg, Left (coloring, p, s), child)) six ->
  match reg, is_empty_coloring coloring, coloring with
  | G, Is_empty, Ec ->
    let two = Buffer.two s in
    let p = Buffer.inject2 p two in
    let p = Buffer.inject6 p six in
    Triple (G, Only_end p, Empty)
  | reg, Not_empty, coloring ->
    let s = Buffer.inject6 s six in
    Triple (reg, Only (adapt_to_suffix coloring, p, s), child)

(** Pops from a green pair chain. *)
let pop_pair_green (Pair (cl, cr)) =
  let tl = triple_of_chain cl in
  let a, tl = pop_left_green tl in
  match tl with
  | End (b, c, d, e, f, g) ->
    let tr = triple_of_chain cr in
    let t = only_of_right (b, c, d, e, f, g) tr in
    (a, S (chain_of_triple t))
  | Ok tl -> (a, S (Pair (chain_of_triple tl, cr)))

(** Ejects from a green pair chain. *)
let eject_pair_green (Pair (cl, cr)) =
  let tr = triple_of_chain cr in
  let tr, a = eject_right_green tr in
  match tr with
  | End (g, f, e, d, c, b) ->
    let tl = triple_of_chain cl in
    let t = only_of_left tl (g, f, e, d, c, b) in
    (S (chain_of_triple t), a)
  | Ok tr -> (S (Pair (cl, chain_of_triple tr)), a)

(** Takes a green pair chain and represent it as a sandwich. *)
let sandwich_pair_green (Pair (cl, cr)) =
  let tl = triple_of_chain cl in
  let tr = triple_of_chain cr in
  let a, tl = pop_left_green tl in
  let tr, z = eject_right_green tr in
  match tl, tr with
  | End (b, c, d, e, f, g), End (t, u, v, w, x, y) ->
    let s = Buffer.empty in
    let s = Buffer.push6 (b, c, d, e, f, g) s in
    let s = Buffer.inject6 s (t, u, v, w, x, y) in
    Sandwich (a, S (Single (G, Packet (Hole, Only_end s), Empty)), z)
  | End (b, c, d, e, f, g), Ok tr ->
    let t = only_of_right (b, c, d, e, f, g) tr in
    Sandwich (a, S (chain_of_triple t), z)
  | Ok tl, End (t, u, v, w, x, y) ->
    let t = only_of_left tl (t, u, v, w, x, y) in
    Sandwich (a, S (chain_of_triple t), z)
  | Ok tl, Ok tr ->
    Sandwich (a, S (Pair (chain_of_triple tl, chain_of_triple tr)), z)

(** Pops from a non-empty green chain. *)
let pop_green
: type a n.
     (a, n ge1, only, green, green) chain
  -> a * a semi_cadeque
= function
  | Pair _ as c -> pop_pair_green c
  | Single _ as c ->
    let t = triple_of_chain c in
    let a, t = pop_only_green t in
    match t with
    | Empty -> (a, S Empty)
    | Ok t -> (a, S (chain_of_triple t))

(** Ejects from a non-empty green chain. *)
let eject_green
: type a n.
     (a, n ge1, only, green, green) chain
  -> a semi_cadeque * a
= function
  | Pair _ as c -> eject_pair_green c
  | Single _ as c ->
    let t = triple_of_chain c in
    let t, a = eject_only_green t in
    match t with
    | Empty -> (S Empty, a)
    | Ok t -> (S (chain_of_triple t), a)

(** Takes a non-empty green chain and represent it as a sandwich. *)
let sandwich_green
: type a n.
     (a, n ge1, only, green, green) chain
  -> (a, a semi_cadeque) sandwich
= function
  | Pair _ as c -> sandwich_pair_green c
  | Single _ as c ->
    let t = triple_of_chain c in
    match sandwich_only_green t with
    | Alone a -> Alone a
    | Sandwich (a, Empty, z) -> Sandwich (a, S Empty, z)
    | Sandwich (a, Ok t, z) -> Sandwich (a, S (chain_of_triple t), z)

(** Takes a prefix of at least 5 elements, a prefix of at least 3 elements and
    and a semi-regular cadeque of stored triples. Rearranges the elements of the
    second prefix to make the first one green (i.e. at least 8 elements). *)
let make_green_prefix p1 p2 child =
  match Buffer.has3p p2 with
  | three, Less_than_3 v ->
    let p1 = Buffer.inject3 p1 three in
    let p1 = Buffer.inject_vector p1 v in
    (p1, child)
  | three, At_least_3 p2 ->
    let p1 = Buffer.inject3 p1 three in
    let child = semi_push (Small p2) child in
    (p1, child)

(** Takes a semi-regular cadeque of stored triples, a suffix of at least 3
    elements and a suffix of at least 5 elements. Rearranges the elements of
    the first suffix to make the second one green (i.e. at least 8 elements). *)
let make_green_suffix child s2 s1 =
  match Buffer.has3s s2 with
  | Less_than_3 v, three ->
    let s1 = Buffer.push3 three s1 in
    let s1 = Buffer.push_vector v s1 in
    (child, s1)
  | At_least_3 s2, three ->
    let s1 = Buffer.push3 three s1 in
    let child = semi_inject child (Small s2) in
    (child, s1)

(** Takes a stored triple and a semi-regular cadeque of stored triples. Extracts
    the prefix of the stored triple, the remaining elements and the semi-
    regular cadeque form a new semi-regular cadeque. *)
let extract_prefix stored child = match stored with
  | Small p -> (Stored_buffer p, child)
  | Big (p, stored_child, s) ->
    let child = semi_push (Small s) child in
    let child = semi_concat (S stored_child) child in
    (Stored_buffer p, child)

(** Takes a semi-regular cadeque of stored triples and a stored triple. Extracs
    the suffix of the stored triple, the semi-regular cadeque and the remaining
    elements form a new semi-regular cadeque. *)
let extract_suffix child stored = match stored with
  | Small s -> (child, Stored_buffer s)
  | Big (p, stored_child, s) ->
    let child = semi_inject child (Small p) in
    let child = semi_concat child (S stored_child) in
    (child, Stored_buffer s)

(** Takes a prefix of at least 5 elements and a semi-regular cadeque of stored
    triples. Rearranges elements of the semi-regular cadeque to make the prefix
    green. *)
let ensure_green_prefix p child =
  let stored, child = pop_green child in
  let Stored_buffer p2, child = extract_prefix stored child in
  make_green_prefix p p2 child

(** Takes a semi-regular cadeque of stored triples and a suffix of at least 5
    elements. Rearranges elements of the semi-regular cadeque to make the suffix
    green. *)
let ensure_green_suffix child s =
  let child, stored = eject_green child in
  let child, Stored_buffer s2 = extract_suffix child stored in
  make_green_suffix child s2 s

(** Takes a body, a following red left node and the following green chain,
    and makes a green chain out of them. *)
let green_of_red_left body (Left (Rc, p, s)) child =
  let p, S child = ensure_green_prefix p child in
  match is_empty child, child with
  | Is_empty, Empty -> Single (G, Packet (body, Left (Ec, p, s)), Empty)
  | Not_empty, child -> Single (G, Packet (body, Left (Gc, p, s)), child)

(** Takes a body, a following red right node and the following green chain,
    and makes a green chain out of them. *)
let green_of_red_right body (Right (Rc, p, s)) child =
  let S child, s = ensure_green_suffix child s in
  match is_empty child, child with
  | Is_empty, Empty -> Single (G, Packet (body, Right (Ec, p, s)), Empty)
  | Not_empty, child -> Single (G, Packet (body, Right (Gc, p, s)), child)

(** Takes a body and a following green triple, and makes a green chain out of
    them. *)
let make_green_only body (p, S child, s) =
  match is_empty child, child with
  | Not_empty, child -> Single (G, Packet (body, Only (Gc, p, s)), child)
  | Is_empty, Empty ->
    match Buffer.has3p8 s with
    | Less_than_11 (eight, v) ->
      let p = Buffer.inject8 p eight in
      let p = Buffer.inject_vector p v in
      Single (G, Packet (body, Only_end p), Empty)
    | At_least_11 (small, s) ->
      Single (G, Packet (body, Only (Gc, p, s)), single_chain (Small small))

(** Takes a body, a following red only node and the following green chain,
    and makes a green chain out of them. *)
let green_of_red_only body (Only (Rc, p, s) : _ node) child =
  match Buffer.has8 p, Buffer.has8 s with
  | At_least_8 p, At_least_8 s ->
    Single (G, Packet (body, Only (Gc, p, s)), child)
  | At_least_8 p, Less_than_8 _ ->
    let child, s = ensure_green_suffix child s in
    make_green_only body (p, child, s)
  | Less_than_8 _, At_least_8 s ->
    let p, child = ensure_green_prefix p child in
    make_green_only body (p, child, s)
  | Less_than_8 eltp, Less_than_8 elts ->
    match sandwich_green child with
    | Alone (Small p) ->
      let p = Buffer.push_5vector eltp p in
      let s = Buffer.inject_5vector p elts in
      Single (G, Packet (body, Only_end s), Empty)
    | Alone (Big (p, child, s)) ->
      let p = Buffer.push_5vector eltp p in
      let s = Buffer.inject_5vector s elts in
      make_green_only body (p, S child, s)
    | Sandwich (storedl, child, storedr) ->
      let Stored_buffer p, child = extract_prefix storedl child in
      let child, Stored_buffer s = extract_suffix child storedr in
      let p = Buffer.push_5vector eltp p in
      let s = Buffer.inject_5vector s elts in
      make_green_only body (p, child, s)

(** Takes any chain and makes it green. *)
let rec ensure_green
: type a ck k cl cr.
     (a, ck, k, cl, cr) chain
  -> (a, ck, k, green, green) chain
= function
  | Empty -> Empty
  | Single (G, pkt, c) -> Single (G, pkt, c)
  | Single (R, Packet (body, red), rest) ->
    begin match red with
    | Only  _ as red -> green_of_red_only  body red rest
    | Left  _ as red -> green_of_red_left  body red rest
    | Right _ as red -> green_of_red_right body red rest
    end
  | Single (_, _, _) -> .
  | Pair (cl, cr) ->
    Pair (ensure_green cl, ensure_green cr)

  (** Regularizes a semi-regular cadeque. *)
  let regularize
  : type a. a semi_cadeque -> a cadeque
  = fun (S c) -> T (ensure_green c)

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(** The empty cadeque. *)
let empty = T Empty

(** Pushes on a cadeque. *)
let push x (T c) = regularize (semi_push x (S c))

(** Injects on a cadeque. *)
let inject (T c) x = regularize (semi_inject (S c) x)

(** Pops from a cadeque. *)
let pop (T c) = match is_empty c, c with
  | Is_empty, Empty -> None
  | Not_empty, c ->
    let a, S c = pop_green c in
    Some (a, T (ensure_green c))

(** Ejects from a cadeque. *)
let eject (T c) = match is_empty c, c with
  | Is_empty, Empty -> None
  | Not_empty, c ->
    let S c, z = eject_green c in
    Some (T (ensure_green c), z)

(** Concatenates two cadeques. *)
let concat (T c1) (T c2) = regularize (semi_concat (S c1) (S c2))

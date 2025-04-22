open Color_GYOR

(* Support for natural number types. *)

type    z
type 'n s

type      eq0 = z
type      eq1 = z s
type      eq2 = z s s
type      eq6 = z s s s s s s

type 'n plus1 = 'n s
type 'n plus2 = 'n s plus1
type 'n plus3 = 'n s plus2
type 'n plus4 = 'n s plus3
type 'n plus5 = 'n s plus4
type 'n plus6 = 'n s plus5
type 'n plus7 = 'n s plus6
type 'n plus8 = 'n s plus7

(* Some tupple renaming. *)

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
  | V1 : 'a -> ('a, 'n plus1) vector
  | V2 : 'a * 'a -> ('a, 'n plus2) vector
  | V3 : 'a * 'a * 'a -> ('a, 'n plus3) vector
  | V4 : 'a * 'a * 'a * 'a -> ('a, 'n plus4) vector
  | V5 : 'a * 'a * 'a * 'a * 'a -> ('a, 'n plus5) vector
  | V6 : 'a * 'a * 'a * 'a * 'a * 'a -> ('a, 'n plus6) vector

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

(** A module for buffers. *)
module Buffer : sig

  (** The type for buffer is parametrized by the type of elements it contains
      and the number of elements it contains. *)
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
  val pair   : ('a * 'a) -> ('a, z s s) t

  val push3 : 'a * 'a * 'a -> ('a, 'n) t -> ('a, 'n s s s) t
  val inject3 : ('a, 'n) t -> 'a * 'a * 'a -> ('a, 'n s s s) t

  val push_5vector : 'a five * ('a, _) vector -> ('a, 'n) t -> ('a, 'n plus5) t
  val inject_5vector : ('a, 'n) t -> 'a five * ('a, _) vector -> ('a, 'n plus5) t

  val push6 : 'a six -> ('a, 'n) t -> ('a, 'n plus6) t
  val inject6 : ('a, 'n) t -> 'a six -> ('a, 'n plus6) t

  val inject8 : ('a, 'n) t -> 'a eight -> ('a, 'n plus8) t

  val push_vector : ('a, _) vector -> ('a, 'n) t -> ('a, 'n) t
  val inject_vector : ('a, 'n) t -> ('a, _) vector -> ('a, 'n) t

  (** A type storing either 0 element or a buffer with at least 1 element. *)
  type _ has1 =
    | Exact_0 : 'a has1
    | At_least_1 : ('a, _ plus1) t -> 'a has1

  (** Tells if a given buffer is empty or not. *)
  val has1 : ('a, 'n) t -> 'a has1

  (** A type storing either 0, 1 or 2 elements, or a buffer with at least 3
      elements. *)
  type 'a has3 =
    | Less_than_3 : ('a, eq2) vector -> 'a has3
    | At_least_3 : ('a, _ plus3) t -> 'a has3

  (** Tells if a given buffer of at least 3 elements has 3, 4, 5 elements or
      more than 6. *)
  val has3p : ('a, _ plus3) t -> ('a * 'a * 'a) * 'a has3

  (** Tells if a given buffer of at least 3 elements has 3, 4, 5 elements or
      more than 6. *)
  val has3s : ('a, _ plus3) t -> 'a has3 * ('a * 'a * 'a)

  (** A type storing either 4 elements or a buffer with at least 5 elements. *)
  type 'a has5 =
    | Exact_4 : 'a four -> 'a has5
    | At_least_5 : ('a, _ plus5) t -> 'a has5

  (** Tells if a given buffer of at least 4 elements has just 4 elements or
      more. *)
  val has5   : ('a, _ plus4) t -> 'a has5

  (** A type storing 6 elements or less or a buffer of at least 5 elements and
      2 more elements. *)
  type 'a has7s =
    | Less_than_7 : ('a, eq6) vector -> 'a has7s
    | At_least_7 : ('a, _ plus5) t * ('a * 'a) -> 'a has7s

  (** Tells if a given buffer has at least 7 elements or not. If it has more
      than 7 elements, the last 2 elements are extracted. *)
  val has7s : ('a, _ plus1) t -> 'a has7s

  (** A type storing 6 elements or less or a 2 elements and a buffer of at
      least 5 elements. *)
  type 'a has7p =
    | Less_than_7 : ('a, eq6) vector -> 'a has7p
    | At_least_7 : ('a * 'a) * ('a, _ plus5) t -> 'a has7p

  (** Tells if a given buffer has at least 7 elements or not. If it has more
      than 7 elements, the first 2 elements are extracted. *)
  val has7p : ('a, _ plus1) t -> 'a has7p

  (** A type storing either 5, 6 or 7 elements, or a buffer with at least 8
      elements. *)
  type 'a has8 =
    | Less_than_8 : ('a five * ('a, eq2) vector) -> 'a has8
    | At_least_8 : ('a, _ plus8) t -> 'a has8

  (** Tells if a given buffer of at least 5 elements has 5, 6, 7 elements or
      more than 8. *)
  val has8   : ('a, _ plus5) t -> 'a has8

  (** A type storing 8, 9 or 10 elements, or a buffer of 3 elements and a
      buffer of at least 8 elements. *)
  type 'a has3p8 =
    | Less_than_11 : 'a eight * ('a, eq2) vector -> 'a has3p8
    | At_least_11 : ('a, z plus3) t * ('a, _ plus8) t -> 'a has3p8

  (** Tells if a given buffer of at least 8 elements has 8, 9 or 10 elements,
      or if it has at least 11 elements. If the case latest case, it returns a
      buffer of 3 elements and a buffer of at least 8 elements. *)
  val has3p8 : ('a, _ plus8) t -> 'a has3p8

  (* Different operations needed for the cadeque package. *)

  val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, 'n) t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> ('a, 'n) t -> 'b -> 'b

  val make : int -> 'a -> 'a has1

end = struct

  type ('a, 'quantity) t = 'a Deque.t

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
  let pair (x, y) = push x (single y)
  let triple (x, y, z) = push x (pair (y, z))

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
    | At_least_1 : ('a, _ plus1) t -> 'a has1

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
    | At_least_3 : ('a, _ plus3) t -> 'a has3

  let has3 t = match Deque.length t with
    | 0 -> Less_than_3 V0
    | 1 -> let a, _ = pop t in Less_than_3 (V1 a)
    | 2 -> let a, b, _ = pop2 t in Less_than_3 (V2 (a, b))
    | _ -> At_least_3 t

  let has3p buf =
    let three, buf = pop3 buf in
    three, has3 buf

  let has3s buf =
    let buf, three = eject3 buf in
    has3 buf, three

  type 'a has5 =
    | Exact_4 : 'a four -> 'a has5
    | At_least_5 : ('a, _ plus5) t -> 'a has5

  let has5 buffer =
    let a, b, t = pop2 buffer in
    let c, d, t = pop2 t in
    match has1 t with
    | Exact_0 -> Exact_4 (a, b, c, d)
    | At_least_1 _ -> At_least_5 buffer

  type 'a has7s =
    | Less_than_7 : ('a, eq6) vector -> 'a has7s
    | At_least_7 : ('a, _ plus5) t * ('a * 'a) -> 'a has7s

  let has7s t =
    let t, z = eject t in
    match Deque.length t with
    | 0 -> Less_than_7 (V1 z)
    | 1 -> let _, y = eject t in Less_than_7 (V2 (y, z))
    | 2 -> let _, x, y = eject2 t in Less_than_7 (V3 (x, y, z))
    | 3 -> let _, (w, x, y) = eject3 t in Less_than_7 (V4 (w, x, y, z))
    | 4 -> let t, x, y = eject2 t in let _, v, w = eject2 t in
      Less_than_7 (V5 (v, w, x, y, z))
    | 5 -> let t, (w, x, y) = eject3 t in let _, u, v = eject2 t in
      Less_than_7 (V6 (u, v, w, x, y, z))
    | _ -> let t', y = eject t in At_least_7 (t', (y, z))

  type 'a has7p =
    | Less_than_7 : ('a, eq6) vector -> 'a has7p
    | At_least_7 : ('a * 'a) * ('a, _ plus5) t -> 'a has7p

  let has7p t =
    let a, t = pop t in
    match Deque.length t with
    | 0 -> Less_than_7 (V1 a)
    | 1 -> let b, _ = pop t in Less_than_7 (V2 (a, b))
    | 2 -> let b, c, _ = pop2 t in Less_than_7 (V3 (a, b, c))
    | 3 -> let (b, c, d), _ = pop3 t in Less_than_7 (V4 (a, b, c, d))
    | 4 -> let b, c, t = pop2 t in let d, e, _ = pop2 t in
      Less_than_7 (V5 (a, b, c, d, e))
    | 5 -> let (b, c, d), t = pop3 t in let e, f, _ = pop2 t in
      Less_than_7 (V6 (a, b, c, d, e, f))
    | _ -> let b, t' = pop t in At_least_7 ((a, b), t')

  type 'a has8 =
    | Less_than_8 : ('a five * ('a, eq2) vector) -> 'a has8
    | At_least_8 : ('a, _ plus8) t -> 'a has8

  let pop5 t =
    let (a, b, c), t = pop3 t in
    let d, e, t = pop2 t in
    (a, b, c, d, e), t

  let has8 t = match Deque.length t with
    | 0 | 1 | 2 | 3 | 4 -> assert false
    | 5 -> let five, _ = pop5 t in Less_than_8 (five, V0)
    | 6 -> let five, t = pop5 t in let f, _ = pop t in
      Less_than_8 (five, V1 f)
    | 7 -> let five, t = pop5 t in let f, g, _ = pop2 t in
      Less_than_8 (five, V2 (f, g))
    | _ -> At_least_8 t

  let push_vector v t = vector_fold_right Deque.push v t
  let inject_vector t v = vector_fold_left Deque.inject t v

  type 'a has3p8 =
    | Less_than_11 : 'a eight * ('a, eq2) vector -> 'a has3p8
    | At_least_11 : ('a, z plus3) t * ('a, _ plus8) t -> 'a has3p8

  let has3p8 t =
    let (a, b, c), t' = pop3 t in
    match has8 t' with
    | Less_than_8 ((d, e, f, g, h), v) ->
      Less_than_11 ((a, b, c, d, e, f, g, h), v)
    | At_least_8 t' ->
      At_least_11 (triple (a, b, c), t')

  let push5 (a, b, c, d, e) t =
    push a (push b (push c (push d (push e t))))

  let inject5 t (a, b, c, d, e) =
    inject (inject (inject (inject (inject t a) b) c) d) e

  let push_5vector (five, v) t = push5 five (push_vector v t)
  let inject_5vector t (five, v) = inject_vector (inject5 t five) v

  let inject8 t (a, b, c, d, e, f, g, h) =
    inject2 (inject2 (inject2 (inject2 t (a, b)) (c, d)) (e, f)) (g, h)

  let fold_left = Deque.fold_left
  let fold_right = Deque.fold_right

  let make n a = match n with
    | 0 -> Exact_0
    | _ -> At_least_1 (Deque.make n a)

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

(** The node_coloring relation links the sizes of the prefix and the suffix and
    the arity of a node to its color.  *)
type ('prefix_delta, 'suffix_delta, 'arity, 'c) node_coloring =
  | EN : (      _,       _,     eq0, green ) node_coloring
  | GN : (_ plus3, _ plus3, _ plus1, green ) node_coloring
  | YN : (_ plus2, _ plus2, _ plus1, yellow) node_coloring
  | ON : (_ plus1, _ plus1, _ plus1, orange) node_coloring
  | RN : (      _,       _, _ plus1, red   ) node_coloring

(** A type for nodes. *)
type ('a, 'arity, 'kind, 'c) node =
  | Only      : ('pdelta, 'sdelta, 'n plus1, 'c) node_coloring
              * ('a, 'pdelta plus5) prefix
              * ('a, 'sdelta plus5) suffix
             -> ('a, 'n plus1, only, 'c) node
  | Only_end  : ('a, _ plus1) Buffer.t
             -> ('a, eq0, only, green) node
  | Left      : ('pdelta, _, 'arity, 'c) node_coloring
              * ('a, 'pdelta plus5) prefix
              * ('a * 'a)
             -> ('a, 'arity, left, 'c) node
  | Right     : (_, 'sdelta, 'arity, 'c) node_coloring
              * ('a * 'a)
              * ('a, 'sdelta plus5) suffix
             -> ('a, 'arity, right, 'c) node

(** A type for the regularity relation. *)
type ('pkt_color, 'chain_left_color, 'chain_right_color) regularity =
  | G : (green,     _,     _) regularity
  | R : (  red, green, green) regularity

(** A type for stored triples. *)
type 'a stored =
  | Small : ('a, _ plus3) prefix -> 'a stored
  | Big : ('a, _ plus3) prefix
        * ('a stored, _, only, _, _) chain
        * ('a, _ plus3) suffix
       -> 'a stored

(** A type for bodies of packets. *)
and ('a, 'b, 'head_kind, 'tail_kind) body =
  | Hole : ('a, 'a, 'kind, 'kind) body
  | Single_child :
       ('a, eq1, 'head_kind, nogreen * _ * _ * nored) node
     * ('a stored, 'b, only, 'tail_kind) body
    -> ('a, 'b, 'head_kind, 'tail_kind) body
  | Pair_yellow :
       ('a, eq2, 'head_kind, yellow) node
     * ('a stored, 'b, left, 'tail_kind) body
     * ('a stored, single, right, 'c, 'c) chain
    -> ('a, 'b, 'head_kind, 'tail_kind) body
  | Pair_orange :
       ('a, eq2, 'head_kind, orange) node
     * ('a stored, single, left, green, green) chain
     * ('a stored, 'b, right, 'tail_kind) body
    -> ('a, 'b, 'head_kind, 'tail_kind) body

(** A type for packets. *)
and ('a, 'b, 'arity, 'kind, 'color) packet =
  | Packet :
       ('a, 'b, 'kind, 'tail_kind) body
     * ('b, 'arity, 'tail_kind, _ * noyellow * noorange * _ as 'c) node
    -> ('a, 'b stored, 'arity, 'kind, 'c) packet

(** A type for chains. *)
and ('a, 'arity, 'kind, 'left_color, 'right_color) chain =
  | Empty : ('a, empty, _, _, _) chain
  | Single : ('c, 'lc, 'rc) regularity
           * ('a, 'b, 'arity, 'kind, 'c) packet
           * ('b, 'arity, only, 'lc, 'rc) chain
          -> ('a, single, 'kind, 'c, 'c) chain
  | Pair : ('a, single, left , 'lc, 'lc) chain
         * ('a, single, right, 'rc, 'rc) chain
        -> ('a, pair, _, 'lc, 'rc) chain

(** A type representing prefix and suffix of at least 3 elements. *)
type _ stored_buffer =
  | Stored_buffer : ('a, _ plus3) Buffer.t -> 'a stored_buffer

(** The triple_coloring relation links the color and the arity of the root and
    the left and right color of the child chain of a triple to its color. *)
type ('root_color, 'arity, 'left_color, 'right_color, 'color) triple_coloring =
  | GT  : ( green,        _,     _,     _, green) triple_coloring
  | YT  : (yellow,  _ plus1,   'lc,     _,   'lc) triple_coloring
  | OST : (orange,   single,    'c,    'c,    'c) triple_coloring
  | OPT : (orange,   pair, green,   'rc,   'rc) triple_coloring
  | RT  : (   red,  _ plus1, green, green,   red) triple_coloring

(** A type for the triple representation of a non-empty cadeque. First comes the
    triple coloring, then the prefix and suffix as a node, then the child
    cadeque as a chain. *)
type ('a, 'kind, 'color) triple =
  | Triple :
       ('node_color, 'arity, 'lc, 'rc, 'c) triple_coloring
     * ('a, 'arity, 'kind, 'node_color) node
     * ('a stored, 'arity, only, 'lc, 'rc) chain
    -> ('a, 'kind, 'c) triple

(** A type used to represent left or right triples. If there is not enough
    elements to make one, they are stored in a vector. *)
type (_, _, _) left_right_triple =
  | Not_enough : ('a, eq6) vector -> ('a, _, _) left_right_triple
  | Ok : ('a, 'k, 'c) triple -> ('a, 'k, 'c) left_right_triple

(** A type used to represent triples after a pop or eject operation. If the
    remaining triple is still valid, it is stored as it is. If not, it means it
    remains six elements if it was a left or right triple, or no element if it
    was an only triple. The [pair] parameter is used to differentiate between
    those two possible cases. It translates to : was the modified triple part
    of a pair or not. *)
type ('a, 'arity, 'kind) partial_triple =
  | Empty : ('a, single, _) partial_triple
  | End : 'a six -> ('a, pair, _) partial_triple
  | Ok : ('a, 'kind, _) triple -> ('a, _, 'kind) partial_triple

(** The sandwich type contains either one element of type [exter] or an element
    of type [inter] sandwiched into two elements of type [exter]. *)
type ('exter, 'inter) sandwich =
  | Alone : 'exter -> ('exter, _) sandwich
  | Sandwich : 'exter * 'inter * 'exter -> ('exter, 'inter) sandwich

(** A type for semi-regular cadeques. The kind of the chain being [only] rules
    out single chains having a left or right node as their root. Such chains
    cannot be standalone chains, they are necessarily part of a pair. *)
type 'a semi_cadeque = S : ('a, _, only, _, _) chain -> 'a semi_cadeque

(** A type for regular cadeques. *)
type 'a cadeque = T : ('a, _, only, green, green) chain -> 'a cadeque

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(** Pushes on a left node. *)
let push_left_node
: type a arity c. a -> (a, arity, left, c) node -> (a, arity, left, c) node
= fun x store ->
  match store with
  | Left (GN, p, s) -> Left (GN, Buffer.push x p, s)
  | Left (YN, p, s) -> Left (YN, Buffer.push x p, s)
  | Left (ON, p, s) -> Left (ON, Buffer.push x p, s)
  | Left (RN, p, s) -> Left (RN, Buffer.push x p, s)
  | Left (EN, p, s) -> Left (EN, Buffer.push x p, s)

(** Injects on a right node. *)
let inject_right_node
: type a arity c. (a, arity, right, c) node -> a -> (a, arity, right, c) node
= fun store x ->
  match store with
  | Right (GN, p, s) -> Right (GN, p, Buffer.inject s x)
  | Right (YN, p, s) -> Right (YN, p, Buffer.inject s x)
  | Right (ON, p, s) -> Right (ON, p, Buffer.inject s x)
  | Right (RN, p, s) -> Right (RN, p, Buffer.inject s x)
  | Right (EN, p, s) -> Right (EN, p, Buffer.inject s x)

(** Pushes on an only node. *)
let push_only_node
: type a arity c. a -> (a, arity, only, c) node -> (a, arity, only, c) node
= fun x store ->
  match store with
  | Only (GN, p, s) -> Only (GN, Buffer.push x p, s)
  | Only (YN, p, s) -> Only (YN, Buffer.push x p, s)
  | Only (ON, p, s) -> Only (ON, Buffer.push x p, s)
  | Only (RN, p, s) -> Only (RN, Buffer.push x p, s)
  | Only_end p -> Only_end (Buffer.push x p)

(** Injects on an only node. *)
let inject_only_node
: type a arity c. (a, arity, only, c) node -> a -> (a, arity, only, c) node
= fun store x ->
  match store with
  | Only (GN, p, s) -> Only (GN, p, Buffer.inject s x)
  | Only (YN, p, s) -> Only (YN, p, Buffer.inject s x)
  | Only (ON, p, s) -> Only (ON, p, Buffer.inject s x)
  | Only (RN, p, s) -> Only (RN, p, Buffer.inject s x)
  | Only_end p -> Only_end (Buffer.inject p x)

(** Pushes on a left packet. *)
let push_left_packet
: type a b arity c.
  a -> (a, b, arity, left, c) packet -> (a, b, arity, left, c) packet
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
: type a b arity c.
  (a, b, arity, right, c) packet -> a -> (a, b, arity, right, c) packet
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
: type a b arity c.
  a -> (a, b, arity, only, c) packet -> (a, b, arity, only, c) packet
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
: type a b arity c.
  (a, b, arity, only, c) packet -> a -> (a, b, arity, only, c) packet
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
: type a n lc rc. a -> (a, n plus1, only, lc, rc) chain
                    -> (a, n plus1, only, lc, rc) chain
= fun x c ->
  match c with
  | Single (reg, pkt, c) -> Single (reg, push_only_packet x pkt, c)
  | Pair (lc, rc) -> Pair (push_left_chain x lc, rc)

(** Injects on a non-empty only chain. *)
let inject_ne_chain
: type a n lc rc. (a, n plus1, only, lc, rc) chain -> a
               -> (a, n plus1, only, lc, rc) chain
= fun c x ->
  match c with
  | Single (reg, pkt, c) -> Single (reg, inject_only_packet pkt x, c)
  | Pair (lc, rc) -> Pair (lc, inject_right_chain rc x)

(** Discerns when its type parameter is empty or not. *)
type _ is_empty =
  | Is_empty  :  empty  is_empty
  | Not_empty : (_ plus1) is_empty

(** Tells if a chain is empty or not. *)
let is_empty_chain
: type a arity kind lc rc. (a, arity, kind, lc, rc) chain -> arity is_empty
= function Empty -> Is_empty
  | Single _ -> Not_empty | Pair _ -> Not_empty

(** Pushes on a semi-regular cadeque. *)
let semi_push x (S c) = match is_empty_chain c with
  | Is_empty  -> S (single_chain x)
  | Not_empty -> S (push_ne_chain x c)

(** Injects on a semi-regular cadeque. *)
let semi_inject (S c) x = match is_empty_chain c with
  | Is_empty  -> S (single_chain x)
  | Not_empty -> S (inject_ne_chain c x)

(** Returns the triple coloring associated to a yellow or orange node with one
    child. *)
let to_triple_coloring
: type a kind y o.
     (a, single, kind, nogreen * y * o * nored as 'c) node
  -> ('c, single, _, _, _) triple_coloring
= function
  | Only  (YN, _, _) -> YT | Only  (ON, _, _) -> OST
  | Left  (YN, _, _) -> YT | Left  (ON, _, _) -> OST
  | Right (YN, _, _) -> YT | Right (ON, _, _) -> OST

(** Returns the triple representation of a non-empty only chain. *)
let triple_of_chain
: type a k c. (a, single, k, c, c) chain -> (a, k, c) triple
= function
  | Single (G, Packet (Hole, tl), child) -> Triple (GT, tl, child)
  | Single (reg, Packet (Single_child (hd, bd), tl), child) ->
    Triple (to_triple_coloring hd, hd, Single (reg, Packet (bd, tl), child))
  | Single (reg, Packet (Pair_yellow (hd, bd, rc), tl), child) ->
    Triple (YT, hd, Pair (Single (reg, Packet (bd, tl), child), rc))
  | Single (reg, Packet (Pair_orange (hd, lc, bd), tl), child) ->
    Triple (OPT, hd, Pair (lc, Single (reg, Packet (bd, tl), child)))
  | Single (R, Packet (Hole, tl), child) -> match tl with
    | Only _ -> Triple (RT, tl, child)
    | Left  (RN, _, _) -> Triple (RT, tl, child)
    | Right (RN, _, _) -> Triple (RT, tl, child)

(** Returns the non-empty only chain associated to a triple. *)
let chain_of_triple
: type a kind c. (a, kind, c) triple -> (a, single, kind, c, c) chain
= function
  | Triple (GT, hd, child) -> Single (G, Packet (Hole, hd), child)
  | Triple (YT, hd, Single (reg, Packet (bd, tl), child)) ->
    Single (reg, Packet (Single_child (hd, bd), tl), child)
  | Triple (YT, hd, Pair (Single (reg, Packet (bd, tl), child), rc)) ->
    Single (reg, Packet (Pair_yellow (hd, bd, rc), tl), child)
  | Triple (OST, hd, Single (reg, Packet (bd, tl), child)) ->
    Single (reg, Packet (Single_child (hd, bd), tl), child)
  | Triple (OPT, hd, Pair (lc, Single (reg, Packet (bd, tl), child))) ->
    Single (reg, Packet (Pair_orange (hd, lc, bd), tl), child)
  | Triple (RT, hd, child) -> Single (R, Packet (Hole, hd), child)

(** Makes a left [left_right_triple] out of an only triple. *)
let left_of_only
: type a c. (a, only, c) triple -> (a, left, c) left_right_triple
= function
  | Triple (GT, Only_end p, Empty) ->
    begin match Buffer.has7s p with
    | Less_than_7 v -> Not_enough v
    | At_least_7 (p, s) ->
      Ok (Triple (GT, Left (EN, p, s), Empty))
    end
  | Triple (tc, Only (nc, p, s), child) ->
    let s', y, x = Buffer.eject2 s in
    let child = inject_ne_chain child (Small s') in
    Ok (Triple (tc, Left (nc, p, (y, x)), child))

(** Makes a right [left_right_triple] out of an only triple. *)
let right_of_only
: type a c. (a, only, c) triple -> (a, right, c) left_right_triple
= function
  | Triple (GT, Only_end s, Empty) ->
    begin match Buffer.has7p s with
    | Less_than_7 v -> Not_enough v
    | At_least_7 (p, s) ->
      Ok (Triple (GT, Right (EN, p, s), Empty))
    end
  | Triple (tc, Only (nc, p, s), child) ->
    let x, y, p' = Buffer.pop2 p in
    let child = push_ne_chain (Small p') child in
    Ok (Triple (tc, Right (nc, (x, y), s), child))

(** Takes a suffix of at least one element and a right triple and returns a
    stored triple and a left suffix. *)
let stored_of_right
: type a rc. (a, _ plus1) suffix -> (a, right, rc) triple
          -> a stored * (a * a)
= fun sl (Triple (_, Right (_, p, sr), child)) ->
  let p = Buffer.inject2 sl p in
  let s, y, x = Buffer.eject2 sr in
  (Big (p, child, s), (y, x))

(** Takes a left triple and a prefix of at least one element and returns a
    right prefix and a stored triple. *)
let stored_of_left
: type a lc. (a, left, lc) triple -> (a, _ plus1) prefix
          -> (a * a) * a stored
= fun (Triple (_, Left (_, pl, s), child)) pr ->
  let s = Buffer.push2 s pr in
  let x, y, p = Buffer.pop2 pl in
  ((x, y), Big (p, child, s))

(** Tells if a node is an ending node given its coloring. *)
let is_empty_node :
type sp ss arity c. (sp, ss, arity, c) node_coloring -> arity is_empty
= function EN -> Is_empty
  | GN -> Not_empty | YN -> Not_empty | ON -> Not_empty | RN -> Not_empty

(** Makes a left triple out of a pair of left and right triples. *)
let left_of_pair
: type a lc rc.
     (a, left , lc) triple
  -> (a, right, rc) triple
  -> (a, left , lc) triple
= fun (Triple (tc, Left (nc, p, (y, z)), child)) tr ->
  match tc, is_empty_node nc with
  | GT, Is_empty ->
    let p = Buffer.inject p y in
    let s = Buffer.single z in
    let stored, s = stored_of_right s tr in
    let child = single_chain stored in
    Triple (OST, Left (ON, p, s), child)
  | tc, Not_empty ->
    let s = Buffer.pair (y, z) in
    let stored, s = stored_of_right s tr in
    let child = inject_ne_chain child stored in
    Triple (tc, Left (nc, p, s), child)

(** Makes a right triple out of a pair of left and right triples. *)
let right_of_pair
: type a lc rc.
     (a, left , lc) triple
  -> (a, right, rc) triple
  -> (a, right, rc) triple
= fun tl (Triple (tc, Right (nc, (a, b), s), child)) ->
  match tc, is_empty_node nc with
  | GT, Is_empty ->
    let s = Buffer.push b s in
    let p = Buffer.single a in
    let p, stored = stored_of_left tl p in
    let child = single_chain stored in
    Triple (OST, Right (ON, p, s), child)
  | tc, Not_empty ->
    let p = Buffer.pair (a, b) in
    let p, stored = stored_of_left tl p in
    let child = push_ne_chain stored child in
    Triple (tc, Right (nc, p, s), child)

(** Makes a left [left_right_triple] out of a chain. *)
let make_left
: type a arity lc rc. (a, arity, only, lc, rc) chain
                   -> (a, left, lc) left_right_triple
= function
  | Empty -> Not_enough V0
  | Single _ as c -> left_of_only (triple_of_chain c)
  | Pair (lc, rc) ->
    Ok (left_of_pair (triple_of_chain lc) (triple_of_chain rc))

(** Makes a right [left_right_triple] out of a chain. *)
let make_right
: type a arity lc rc. (a, arity, only, lc, rc) chain
                   -> (a, right, rc) left_right_triple
= function
  | Empty -> Not_enough V0
  | Single _ as c -> right_of_only (triple_of_chain c)
  | Pair (lc, rc) ->
    Ok (right_of_pair (triple_of_chain lc) (triple_of_chain rc))

(** Concatenates two semi-regular cadeques. *)
let semi_concat (S c1) (S c2) = match make_left c1 with
  | Not_enough v -> vector_fold_right semi_push v (S c2)
  | Ok tl -> match make_right c2 with
    | Not_enough v -> vector_fold_left semi_inject (S c1) v
    | Ok tr -> S (Pair (chain_of_triple tl, chain_of_triple tr))

(** Returns the orange triple coloring corresponding to the child chain. *)
let orange_tc
: type a n rc.
     (a, n plus1, only, green, rc) chain
  -> (orange, n plus1, green, rc, rc) triple_coloring
= function
  | Single _ -> OST
  | Pair   _ -> OPT

(** Pops from a green left triple. *)
let pop_left_green
: type a. (a, left, green) triple -> a * (a, pair, left) partial_triple
= function
  | Triple (tc, Left (nc, p, s), child) ->
    let a, p = Buffer.pop p in
    match tc, nc with
    | GT , EN ->
      begin match Buffer.has5 p with
      | Exact_4 (b, c, d, e) ->
        let f, g = s in
        (a, End (b, c, d, e, f, g))
      | At_least_5 p -> (a, Ok (Triple (GT, Left (EN, p, s), Empty)))
      end
    | GT , GN -> (a, Ok (Triple (YT, Left (YN, p, s), child)))
    | YT , YN -> (a, Ok (Triple (orange_tc child, Left (ON, p, s), child)))
    | OST, ON -> (a, Ok (Triple (RT, Left (RN, p, s), child)))
    | OPT, ON -> (a, Ok (Triple (RT, Left (RN, p, s), child)))

(** Ejects from a green right triple. *)
let eject_right_green
: type a. (a, right, green) triple -> (a, pair, right) partial_triple * a
= function
  | Triple (tc, Right (nc, p, s), child) ->
    let s, z = Buffer.eject s in
    match tc, nc with
    | GT , EN ->
      begin match Buffer.has5 s with
      | Exact_4 (v, w, x, y) ->
        let t, u = p in
        (End (t, u, v, w, x, y), z)
      | At_least_5 s -> (Ok (Triple (GT, Right (EN, p, s), Empty)), z)
      end
    | GT , GN -> (Ok (Triple (YT, Right (YN, p, s), child)), z)
    | YT , YN -> (Ok (Triple (orange_tc child, Right (ON, p, s), child)), z)
    | OST, ON -> (Ok (Triple (RT, Right (RN, p, s), child)), z)
    | OPT, ON -> (Ok (Triple (RT, Right (RN, p, s), child)), z)

(** Pops from an green only triple. *)
let pop_only_green = function
  | Triple (GT, Only_end p, Empty) ->
    let a, p = Buffer.pop p in
    begin match Buffer.has1 p with
    | Exact_0 -> (a, Empty)
    | At_least_1 p -> (a, Ok (Triple (GT, Only_end p, Empty)))
    end
  | Triple (tc, Only (nc, p, s), child) ->
    let a, p = Buffer.pop p in
    match tc, nc with
    | GT , GN -> (a, Ok (Triple (YT, Only (YN, p, s), child)))
    | YT , YN -> (a, Ok (Triple (orange_tc child, Only (ON, p, s), child)))
    | OST, ON -> (a, Ok (Triple (RT, Only (RN, p, s), child)))
    | OPT, ON -> (a, Ok (Triple (RT, Only (RN, p, s), child)))

(** Ejects from an green only triple. *)
let eject_only_green = function
  | Triple (GT, Only_end s, Empty) ->
    let s, z = Buffer.eject s in
    begin match Buffer.has1 s with
    | Exact_0 -> (Empty, z)
    | At_least_1 s -> (Ok (Triple (GT, Only_end s, Empty)), z)
    end
  | Triple (tc, Only (nc, p, s), child) ->
    let s, z = Buffer.eject s in
    match tc, nc with
    | GT , GN -> (Ok (Triple (YT, Only (YN, p, s), child)), z)
    | YT , YN -> (Ok (Triple (orange_tc child, Only (ON, p, s), child)), z)
    | OST, ON -> (Ok (Triple (RT, Only (RN, p, s), child)), z)
    | OPT, ON -> (Ok (Triple (RT, Only (RN, p, s), child)), z)

(** Takes an green only triple and represent it as a sandwich. *)
let sandwich_only_green
: type a. (a, only, green) triple
       -> (a, (a, single, only) partial_triple) sandwich
= function
  | Triple (GT, Only_end p, Empty) ->
    let a, p = Buffer.pop p in
    begin match Buffer.has1 p with
    | Exact_0 -> Alone a
    | At_least_1 s ->
      let s, z = Buffer.eject s in
      match Buffer.has1 s with
      | Exact_0 -> Sandwich (a, Empty, z)
      | At_least_1 b -> Sandwich (a, Ok (Triple (GT, Only_end b, Empty)), z)
    end
  | Triple (tc, Only (nc, p, s), child) ->
    let a, p = Buffer.pop p in
    let s, z = Buffer.eject s in
    let t = match tc, nc with
    | GT , GN -> Ok (Triple (YT, Only (YN, p, s), child))
    | YT , YN -> Ok (Triple (orange_tc child, Only (ON, p, s), child))
    | OST, ON -> Ok (Triple (RT, Only (RN, p, s), child))
    | OPT, ON -> Ok (Triple (RT, Only (RN, p, s), child))
    in
    Sandwich (a, t, z)

(** Adapts a node coloring to a prefix of 8 or more elements. *)
let adapt_to_prefix
: type sp sp1 ss1 arity c.
     (sp1, ss1, arity, c) node_coloring -> (sp plus3, ss1, arity, c) node_coloring
= function GN -> GN | YN -> YN | ON -> ON | RN -> RN | EN -> EN

(** Makes an only triple out of six elements and a right triple. *)
let only_of_right
: type a c.
     a six
  -> (a, right, c) triple
  -> (a, only, c) triple
= fun six (Triple (tc, Right (nc, p, s), child)) ->
  match tc, is_empty_node nc with
  | GT, Is_empty ->
    let s = Buffer.push2 p s in
    let s = Buffer.push6 six s in
    Triple (GT, Only_end s, child)
  | tc, Not_empty ->
    let p = Buffer.pair p in
    let p = Buffer.push6 six p in
    Triple (tc, Only (adapt_to_prefix nc, p, s), child)

(** Adapts a node coloring to a suffix of 8 or more elements. *)
let adapt_to_suffix
: type ss sp1 ss1 arity c.
     (sp1, ss1, arity, c) node_coloring -> (sp1, ss plus3, arity, c) node_coloring
= function GN -> GN | YN -> YN | ON -> ON | RN -> RN | EN -> EN

(** Makes an only triple out of a left triple and six elements. *)
let only_of_left
: type a c.
     (a, left, c) triple
  -> a six
  -> (a, only, c) triple
= fun (Triple (tc, Left (nc, p, s), child)) six ->
  match tc, is_empty_node nc with
  | GT, Is_empty ->
    let p = Buffer.inject2 p s in
    let p = Buffer.inject6 p six in
    Triple (GT, Only_end p, Empty)
  | tc, Not_empty ->
    let s = Buffer.pair s in
    let s = Buffer.inject6 s six in
    Triple (tc, Only (adapt_to_suffix nc, p, s), child)

(** Pops from a green pair chain. *)
let pop_pair_green (Pair (lc, rc)) =
  let tl = triple_of_chain lc in
  let a, tl = pop_left_green tl in
  match tl with
  | End (b, c, d, e, f, g) ->
    let tr = triple_of_chain rc in
    let t = only_of_right (b, c, d, e, f, g) tr in
    (a, S (chain_of_triple t))
  | Ok tl -> (a, S (Pair (chain_of_triple tl, rc)))

(** Ejects from a green pair chain. *)
let eject_pair_green (Pair (lc, rc)) =
  let tr = triple_of_chain rc in
  let tr, a = eject_right_green tr in
  match tr with
  | End (g, f, e, d, c, b) ->
    let tl = triple_of_chain lc in
    let t = only_of_left tl (g, f, e, d, c, b) in
    (S (chain_of_triple t), a)
  | Ok tr -> (S (Pair (lc, chain_of_triple tr)), a)

(** Takes a green pair chain and represent it as a sandwich. *)
let sandwich_pair_green (Pair (lc, rc)) =
  let tl = triple_of_chain lc in
  let tr = triple_of_chain rc in
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
     (a, n plus1, only, green, green) chain
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
     (a, n plus1, only, green, green) chain
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
     (a, n plus1, only, green, green) chain
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
let green_of_red_left body (Left (RN, p, s)) child =
  let p, S child = ensure_green_prefix p child in
  match is_empty_chain child with
  | Is_empty -> Single (G, Packet (body, Left (EN, p, s)), Empty)
  | Not_empty -> Single (G, Packet (body, Left (GN, p, s)), child)

(** Takes a body, a following red right node and the following green chain,
    and makes a green chain out of them. *)
let green_of_red_right body (Right (RN, p, s)) child =
  let S child, s = ensure_green_suffix child s in
  match is_empty_chain child with
  | Is_empty -> Single (G, Packet (body, Right (EN, p, s)), Empty)
  | Not_empty -> Single (G, Packet (body, Right (GN, p, s)), child)

(** Takes a body and a following green triple, and makes a green chain out of
    them. *)
let make_green_only body (p, S child, s) =
  match is_empty_chain child with
  | Not_empty -> Single (G, Packet (body, Only (GN, p, s)), child)
  | Is_empty ->
    match Buffer.has3p8 s with
    | Less_than_11 (eight, v) ->
      let p = Buffer.inject8 p eight in
      let p = Buffer.inject_vector p v in
      Single (G, Packet (body, Only_end p), Empty)
    | At_least_11 (small, s) ->
      Single (G, Packet (body, Only (GN, p, s)), single_chain (Small small))

(** Takes a body, a following red only node and the following green chain,
    and makes a green chain out of them. *)
let green_of_red_only body (Only (RN, p, s) : _ node) child =
  match Buffer.has8 p, Buffer.has8 s with
  | At_least_8 p, At_least_8 s ->
    Single (G, Packet (body, Only (GN, p, s)), child)
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

(** Takes any single chain and makes it green. *)
let ensure_green_single
: type a k lc rc.
     (a, single, k, lc, rc) chain -> (a, single, k, green, green) chain
= function
  | Single (G, pkt, c) -> Single (G, pkt, c)
  | Single (R, Packet (body, red), rest) -> match red with
    | Only  (RN, _, _) as red -> green_of_red_only  body red rest
    | Left  (RN, _, _) as red -> green_of_red_left  body red rest
    | Right (RN, _, _) as red -> green_of_red_right body red rest

(** Takes any chain and makes it green. *)
let ensure_green
: type a arity k lc rc.
     (a, arity, k, lc, rc) chain -> (a, arity, k, green, green) chain
= function
  | Empty -> Empty
  | Single _ as c -> ensure_green_single c
  | Pair (lc, rc) -> Pair (ensure_green_single lc, ensure_green_single rc)

(** Regularizes a semi-regular cadeque. *)
let regularize
  : type a. a semi_cadeque -> a cadeque
  = fun (S c) -> (T (ensure_green c))

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(** The empty cadeque. *)
let empty = T Empty

(** Pushes on a cadeque. *)
let push x (T c) = match is_empty_chain c with
  | Is_empty -> T (single_chain x)
  | Not_empty -> T (push_ne_chain x c)

(** Injects on a cadeque. *)
let inject (T c) x = match is_empty_chain c with
  | Is_empty -> T (single_chain x)
  | Not_empty -> T (inject_ne_chain c x)

(** Pops from a cadeque. *)
let pop (T c) = match is_empty_chain c with
  | Is_empty -> None
  | Not_empty ->
    let a, S c = pop_green c in
    Some (a, T (ensure_green c))

(** Ejects from a cadeque. *)
let eject (T c) = match is_empty_chain c with
  | Is_empty -> None
  | Not_empty ->
    let S c, z = eject_green c in
    Some (T (ensure_green c), z)

(** Concatenates two cadeques. *)
let concat (T c1) (T c2) = match make_left c1 with
  | Not_enough v -> vector_fold_right push v (T c2)
  | Ok tl -> match make_right c2 with
    | Not_enough v -> vector_fold_left inject (T c1) v
    | Ok tr -> T (Pair (chain_of_triple tl, chain_of_triple tr))

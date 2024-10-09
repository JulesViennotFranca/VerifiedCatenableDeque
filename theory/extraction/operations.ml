(* # mod # -> commented open Classes *)
open Datatypes
open GYOR
open Buffer
open Core
open Models
open Types

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module D =
 struct
  (** val empty : 'a1 cadeque **)

  let empty =
    T (O, (Empty O))

  type 'a empty_graph =
  | Coq_empty_graph_equation_1

  (** val empty_graph_rect :
      (__ -> 'a1) -> 'a2 cadeque -> 'a2 empty_graph -> 'a1 **)

  let empty_graph_rect f _ _ =
    f __

  (** val empty_graph_correct : 'a1 empty_graph **)

  let empty_graph_correct =
    Coq_empty_graph_equation_1

  (** val empty_elim : (__ -> 'a1) -> 'a1 **)

  let empty_elim f =
    f __

  (** val coq_FunctionalElimination_empty : (__ -> __) -> __ **)

  let coq_FunctionalElimination_empty =
    empty_elim

  (** val coq_FunctionalInduction_empty :
      (__ -> __ cadeque) coq_FunctionalInduction **)

  let coq_FunctionalInduction_empty =
    Obj.magic (fun _ -> empty_graph_correct)

  (** val ground_push_clause_1 :
      'a1 stored_triple -> 'a1 chain -> 'a1 cadeque **)

  let ground_push_clause_1 _ refine =
    T ((S O), refine)

  (** val ground_push_clause_2 :
      'a1 stored_triple -> nat -> nat -> color -> color -> regularity -> 'a1
      packet -> 'a1 chain -> 'a1 chain -> 'a1 cadeque **)

  let ground_push_clause_2 _ _ _ _ _ _ _ _ refine =
    T ((S O), refine)

  (** val ground_push_clause_2_1 :
      'a1 stored_triple -> 'a1 chain -> 'a1 chain -> 'a1 chain -> 'a1 cadeque **)

  let ground_push_clause_2_1 _ _ _ refine =
    T ((S (S O)), refine)

  (** val ground_push : 'a1 stored_triple -> 'a1 cadeque -> 'a1 cadeque **)

  let ground_push x = function
  | T (_, c) ->
    (match c with
     | Empty _ -> T ((S O), (single_chain O x))
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       T ((S O),
         (push_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)) x (Single (O, tlvl, ck,
           Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl, cr, r,
           p, c0))))
     | Pair (_, _, _, c0, c1) ->
       T ((S (S O)),
         (push_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
           (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x (Pair (O, (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), c0, c1)))))

  type 'a ground_push_graph =
  | Coq_ground_push_graph_refinement_1 of 'a stored_triple
     * 'a ground_push_clause_1_graph
  | Coq_ground_push_graph_refinement_2 of 'a stored_triple * nat * nat
     * color * color * regularity * 'a packet * 'a chain
     * 'a ground_push_clause_2_graph
  | Coq_ground_push_graph_refinement_3 of 'a stored_triple * 'a chain
     * 'a chain * 'a ground_push_clause_2_1_graph
  and 'a ground_push_clause_1_graph =
  | Coq_ground_push_clause_1_graph_equation_1 of 'a stored_triple * 'a chain
  and 'a ground_push_clause_2_graph =
  | Coq_ground_push_clause_2_graph_equation_1 of 'a stored_triple * nat *
     nat * color * color * regularity * 'a packet * 'a chain * 'a chain
  and 'a ground_push_clause_2_1_graph =
  | Coq_ground_push_clause_2_1_graph_equation_1 of 'a stored_triple
     * 'a chain * 'a chain * 'a chain

  (** val ground_push_clause_2_1_graph_mut :
      (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
      -> (__ -> __ stored_triple -> nat -> nat -> color -> color ->
      regularity -> __ packet -> __ chain -> __ ground_push_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
      ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
      -> __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ chain -> __
      -> 'a3) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain
      -> __ -> 'a4) -> 'a5 stored_triple -> 'a5 chain -> 'a5 chain -> 'a5
      chain -> 'a5 cadeque -> 'a5 ground_push_clause_2_1_graph -> 'a4 **)

  let ground_push_clause_2_1_graph_mut _ _ _ _ _ f4 _ _ _ _ _ = function
  | Coq_ground_push_clause_2_1_graph_equation_1 (x, c0, c1, c') ->
    Obj.magic f4 __ x c0 c1 c' __

  (** val ground_push_clause_2_graph_mut :
      (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
      -> (__ -> __ stored_triple -> nat -> nat -> color -> color ->
      regularity -> __ packet -> __ chain -> __ ground_push_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
      ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
      -> __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ chain -> __
      -> 'a3) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain
      -> __ -> 'a4) -> 'a5 stored_triple -> nat -> nat -> color -> color ->
      regularity -> 'a5 packet -> 'a5 chain -> 'a5 chain -> 'a5 cadeque ->
      'a5 ground_push_clause_2_graph -> 'a3 **)

  let ground_push_clause_2_graph_mut _ _ _ _ f3 _ _ _ _ _ _ _ _ _ _ _ = function
  | Coq_ground_push_clause_2_graph_equation_1 (x, tlvl, ck0, cl, cr, r, p, c,
                                               c') ->
    Obj.magic f3 __ x tlvl ck0 cl cr r p c c' __

  (** val ground_push_clause_1_graph_mut :
      (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
      -> (__ -> __ stored_triple -> nat -> nat -> color -> color ->
      regularity -> __ packet -> __ chain -> __ ground_push_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
      ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
      -> __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ chain -> __
      -> 'a3) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain
      -> __ -> 'a4) -> 'a5 stored_triple -> 'a5 chain -> 'a5 cadeque -> 'a5
      ground_push_clause_1_graph -> 'a2 **)

  let ground_push_clause_1_graph_mut _ _ _ f2 _ _ _ _ _ = function
  | Coq_ground_push_clause_1_graph_equation_1 (x, c) -> Obj.magic f2 __ x c __

  (** val ground_push_graph_mut :
      (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
      -> (__ -> __ stored_triple -> nat -> nat -> color -> color ->
      regularity -> __ packet -> __ chain -> __ ground_push_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
      ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
      -> __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ chain -> __
      -> 'a3) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain
      -> __ -> 'a4) -> 'a5 stored_triple -> 'a5 cadeque -> 'a5 cadeque -> 'a5
      ground_push_graph -> 'a1 **)

  let ground_push_graph_mut f f0 f1 f2 f3 f4 x d s g =
    let rec f5 _ _ _ = function
    | Coq_ground_push_graph_refinement_1 (x0, hind) ->
      Obj.magic f __ x0 hind
        (f6 __ x0 (single_chain O x0) (T ((S O), (single_chain O x0))) hind)
    | Coq_ground_push_graph_refinement_2 (x0, tlvl, ck0, cl, cr, r, p, c, hind) ->
      Obj.magic f0 __ x0 tlvl ck0 cl cr r p c hind
        (f7 __ x0 tlvl ck0 cl cr r p c
          (push_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x0 (Single (O, tlvl,
            ck0, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl,
            cr, r, p, c))) (T ((S O),
          (push_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x0 (Single (O, tlvl,
            ck0, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl,
            cr, r, p, c))))) hind)
    | Coq_ground_push_graph_refinement_3 (x0, c0, c1, hind) ->
      Obj.magic f1 __ x0 c0 c1 hind
        (f8 __ x0 c0 c1
          (push_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x0 (Pair (O, (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), c0, c1))) (T ((S (S O)),
          (push_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x0 (Pair (O, (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), c0, c1))))) hind)
    and f6 _ _ _ _ = function
    | Coq_ground_push_clause_1_graph_equation_1 (x0, c) ->
      Obj.magic f2 __ x0 c __
    and f7 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_ground_push_clause_2_graph_equation_1 (x0, tlvl, ck0, cl, cr, r, p,
                                                 c, c') ->
      Obj.magic f3 __ x0 tlvl ck0 cl cr r p c c' __
    and f8 _ _ _ _ _ _ = function
    | Coq_ground_push_clause_2_1_graph_equation_1 (x0, c0, c1, c') ->
      Obj.magic f4 __ x0 c0 c1 c' __
    in f5 x d s g

  (** val ground_push_graph_rect :
      (__ -> __ stored_triple -> __ ground_push_clause_1_graph -> 'a2 -> 'a1)
      -> (__ -> __ stored_triple -> nat -> nat -> color -> color ->
      regularity -> __ packet -> __ chain -> __ ground_push_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __
      ground_push_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __ stored_triple
      -> __ chain -> __ -> 'a2) -> (__ -> __ stored_triple -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ chain -> __
      -> 'a3) -> (__ -> __ stored_triple -> __ chain -> __ chain -> __ chain
      -> __ -> 'a4) -> 'a5 stored_triple -> 'a5 cadeque -> 'a5 cadeque -> 'a5
      ground_push_graph -> 'a1 **)

  let ground_push_graph_rect =
    ground_push_graph_mut

  (** val ground_push_graph_correct :
      'a1 stored_triple -> 'a1 cadeque -> 'a1 ground_push_graph **)

  let ground_push_graph_correct x = function
  | T (_, c) ->
    (match c with
     | Empty _ ->
       Coq_ground_push_graph_refinement_1 (x,
         (let refine = single_chain O x in
          Coq_ground_push_clause_1_graph_equation_1 (x, refine)))
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       Coq_ground_push_graph_refinement_2 (x, tlvl, ck, cl, cr, r, p, c0,
         (let refine =
            push_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
              (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x (Single (O,
              tlvl, ck, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), cl, cr, r, p, c0))
          in
          Coq_ground_push_clause_2_graph_equation_1 (x, tlvl, ck, cl, cr, r,
          p, c0, refine)))
     | Pair (_, _, _, c0, c1) ->
       Coq_ground_push_graph_refinement_3 (x, c0, c1,
         (let refine =
            push_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) x (Pair
              (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1))
          in
          Coq_ground_push_clause_2_1_graph_equation_1 (x, c0, c1, refine))))

  (** val ground_push_elim :
      (__ -> __ stored_triple -> __ chain -> __ -> __ -> 'a1) -> (__ -> __
      stored_triple -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ chain -> __ -> __ -> 'a1) -> (__ -> __
      stored_triple -> __ chain -> __ chain -> __ chain -> __ -> __ -> 'a1)
      -> 'a2 stored_triple -> 'a2 cadeque -> 'a1 **)

  let ground_push_elim f2 f3 f4 x d =
    ground_push_graph_mut (fun _ _ _ x0 -> x0 __)
      (fun _ _ _ _ _ _ _ _ _ _ x0 -> x0 __) (fun _ _ _ _ _ x0 -> x0 __) f2 f3
      f4 x d (ground_push x d) (ground_push_graph_correct x d)

  (** val coq_FunctionalElimination_ground_push :
      (__ -> __ stored_triple -> __ chain -> __ -> __ -> __) -> (__ -> __
      stored_triple -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ chain -> __ -> __ -> __) -> (__ -> __
      stored_triple -> __ chain -> __ chain -> __ chain -> __ -> __ -> __) ->
      __ stored_triple -> __ cadeque -> __ **)

  let coq_FunctionalElimination_ground_push =
    ground_push_elim

  (** val coq_FunctionalInduction_ground_push :
      (__ -> __ stored_triple -> __ cadeque -> __ cadeque)
      coq_FunctionalInduction **)

  let coq_FunctionalInduction_ground_push =
    Obj.magic (fun _ -> ground_push_graph_correct)

  (** val ground_inject_clause_1 :
      'a1 stored_triple -> 'a1 chain -> 'a1 cadeque **)

  let ground_inject_clause_1 _ refine =
    T ((S O), refine)

  (** val ground_inject_clause_2 :
      nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain
      -> 'a1 stored_triple -> 'a1 chain -> 'a1 cadeque **)

  let ground_inject_clause_2 _ _ _ _ _ _ _ _ refine =
    T ((S O), refine)

  (** val ground_inject_clause_2_1 :
      'a1 chain -> 'a1 chain -> 'a1 stored_triple -> 'a1 chain -> 'a1 cadeque **)

  let ground_inject_clause_2_1 _ _ _ refine =
    T ((S (S O)), refine)

  (** val ground_inject : 'a1 cadeque -> 'a1 stored_triple -> 'a1 cadeque **)

  let ground_inject d x =
    let T (_, c) = d in
    (match c with
     | Empty _ -> T ((S O), (single_chain O x))
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       T ((S O),
         (inject_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
           (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Single (O, tlvl, ck,
           Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl, cr, r,
           p, c0)) x))
     | Pair (_, _, _, c0, c1) ->
       T ((S (S O)),
         (inject_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Pair (O,
           (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), c0, c1)) x)))

  type 'a ground_inject_graph =
  | Coq_ground_inject_graph_refinement_1 of 'a stored_triple
     * 'a ground_inject_clause_1_graph
  | Coq_ground_inject_graph_refinement_2 of nat * nat * color * color
     * regularity * 'a packet * 'a chain * 'a stored_triple
     * 'a ground_inject_clause_2_graph
  | Coq_ground_inject_graph_refinement_3 of 'a chain * 'a chain
     * 'a stored_triple * 'a ground_inject_clause_2_1_graph
  and 'a ground_inject_clause_1_graph =
  | Coq_ground_inject_clause_1_graph_equation_1 of 'a stored_triple * 'a chain
  and 'a ground_inject_clause_2_graph =
  | Coq_ground_inject_clause_2_graph_equation_1 of nat * nat * color *
     color * regularity * 'a packet * 'a chain * 'a stored_triple * 'a chain
  and 'a ground_inject_clause_2_1_graph =
  | Coq_ground_inject_clause_2_1_graph_equation_1 of 'a chain * 'a chain
     * 'a stored_triple * 'a chain

  (** val ground_inject_clause_2_1_graph_mut :
      (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 ->
      'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet
      -> __ chain -> __ stored_triple -> __ ground_inject_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
      ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __
      stored_triple -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color
      -> color -> regularity -> __ packet -> __ chain -> __ stored_triple ->
      __ chain -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __
      stored_triple -> __ chain -> __ -> 'a4) -> 'a5 chain -> 'a5 chain ->
      'a5 stored_triple -> 'a5 chain -> 'a5 cadeque -> 'a5
      ground_inject_clause_2_1_graph -> 'a4 **)

  let ground_inject_clause_2_1_graph_mut _ _ _ _ _ f4 _ _ _ _ _ = function
  | Coq_ground_inject_clause_2_1_graph_equation_1 (c0, c1, x, c') ->
    Obj.magic f4 __ c0 c1 x c' __

  (** val ground_inject_clause_2_graph_mut :
      (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 ->
      'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet
      -> __ chain -> __ stored_triple -> __ ground_inject_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
      ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __
      stored_triple -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color
      -> color -> regularity -> __ packet -> __ chain -> __ stored_triple ->
      __ chain -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __
      stored_triple -> __ chain -> __ -> 'a4) -> nat -> nat -> color -> color
      -> regularity -> 'a5 packet -> 'a5 chain -> 'a5 stored_triple -> 'a5
      chain -> 'a5 cadeque -> 'a5 ground_inject_clause_2_graph -> 'a3 **)

  let ground_inject_clause_2_graph_mut _ _ _ _ f3 _ _ _ _ _ _ _ _ _ _ _ = function
  | Coq_ground_inject_clause_2_graph_equation_1 (tlvl, ck0, cl, cr, r, p, c,
                                                 x, c') ->
    Obj.magic f3 __ tlvl ck0 cl cr r p c x c' __

  (** val ground_inject_clause_1_graph_mut :
      (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 ->
      'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet
      -> __ chain -> __ stored_triple -> __ ground_inject_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
      ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __
      stored_triple -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color
      -> color -> regularity -> __ packet -> __ chain -> __ stored_triple ->
      __ chain -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __
      stored_triple -> __ chain -> __ -> 'a4) -> 'a5 stored_triple -> 'a5
      chain -> 'a5 cadeque -> 'a5 ground_inject_clause_1_graph -> 'a2 **)

  let ground_inject_clause_1_graph_mut _ _ _ f2 _ _ _ _ _ = function
  | Coq_ground_inject_clause_1_graph_equation_1 (x, c) ->
    Obj.magic f2 __ x c __

  (** val ground_inject_graph_mut :
      (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 ->
      'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet
      -> __ chain -> __ stored_triple -> __ ground_inject_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
      ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __
      stored_triple -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color
      -> color -> regularity -> __ packet -> __ chain -> __ stored_triple ->
      __ chain -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __
      stored_triple -> __ chain -> __ -> 'a4) -> 'a5 cadeque -> 'a5
      stored_triple -> 'a5 cadeque -> 'a5 ground_inject_graph -> 'a1 **)

  let ground_inject_graph_mut f f0 f1 f2 f3 f4 d x s g =
    let rec f5 _ _ _ = function
    | Coq_ground_inject_graph_refinement_1 (x0, hind) ->
      Obj.magic f __ x0 hind
        (f6 __ x0 (single_chain O x0) (T ((S O), (single_chain O x0))) hind)
    | Coq_ground_inject_graph_refinement_2 (tlvl, ck0, cl, cr, r, p, c, x0,
                                            hind) ->
      Obj.magic f0 __ tlvl ck0 cl cr r p c x0 hind
        (f7 __ tlvl ck0 cl cr r p c x0
          (inject_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Single (O, tlvl,
            ck0, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl,
            cr, r, p, c)) x0) (T ((S O),
          (inject_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Single (O, tlvl,
            ck0, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl,
            cr, r, p, c)) x0))) hind)
    | Coq_ground_inject_graph_refinement_3 (c0, c1, x0, hind) ->
      Obj.magic f1 __ c0 c1 x0 hind
        (f8 __ c0 c1 x0
          (inject_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Pair (O,
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), c0, c1)) x0) (T ((S (S O)),
          (inject_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Pair (O,
            (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), c0, c1)) x0))) hind)
    and f6 _ _ _ _ = function
    | Coq_ground_inject_clause_1_graph_equation_1 (x0, c) ->
      Obj.magic f2 __ x0 c __
    and f7 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_ground_inject_clause_2_graph_equation_1 (tlvl, ck0, cl, cr, r, p,
                                                   c, x0, c') ->
      Obj.magic f3 __ tlvl ck0 cl cr r p c x0 c' __
    and f8 _ _ _ _ _ _ = function
    | Coq_ground_inject_clause_2_1_graph_equation_1 (c0, c1, x0, c') ->
      Obj.magic f4 __ c0 c1 x0 c' __
    in f5 d x s g

  (** val ground_inject_graph_rect :
      (__ -> __ stored_triple -> __ ground_inject_clause_1_graph -> 'a2 ->
      'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __ packet
      -> __ chain -> __ stored_triple -> __ ground_inject_clause_2_graph ->
      'a3 -> 'a1) -> (__ -> __ chain -> __ chain -> __ stored_triple -> __
      ground_inject_clause_2_1_graph -> 'a4 -> 'a1) -> (__ -> __
      stored_triple -> __ chain -> __ -> 'a2) -> (__ -> nat -> nat -> color
      -> color -> regularity -> __ packet -> __ chain -> __ stored_triple ->
      __ chain -> __ -> 'a3) -> (__ -> __ chain -> __ chain -> __
      stored_triple -> __ chain -> __ -> 'a4) -> 'a5 cadeque -> 'a5
      stored_triple -> 'a5 cadeque -> 'a5 ground_inject_graph -> 'a1 **)

  let ground_inject_graph_rect =
    ground_inject_graph_mut

  (** val ground_inject_graph_correct :
      'a1 cadeque -> 'a1 stored_triple -> 'a1 ground_inject_graph **)

  let ground_inject_graph_correct d x =
    let T (_, c) = d in
    (match c with
     | Empty _ ->
       Coq_ground_inject_graph_refinement_1 (x,
         (let refine = single_chain O x in
          Coq_ground_inject_clause_1_graph_equation_1 (x, refine)))
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       Coq_ground_inject_graph_refinement_2 (tlvl, ck, cl, cr, r, p, c0, x,
         (let refine =
            inject_ne_chain O O (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
              (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Single (O, tlvl,
              ck, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cl,
              cr, r, p, c0)) x
          in
          Coq_ground_inject_clause_2_graph_equation_1 (tlvl, ck, cl, cr, r,
          p, c0, x, refine)))
     | Pair (_, _, _, c0, c1) ->
       Coq_ground_inject_graph_refinement_3 (c0, c1, x,
         (let refine =
            inject_ne_chain O (S O) (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Pair (O,
              (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)), c0, c1)) x
          in
          Coq_ground_inject_clause_2_1_graph_equation_1 (c0, c1, x, refine))))

  (** val ground_inject_elim :
      (__ -> __ stored_triple -> __ chain -> __ -> __ -> 'a1) -> (__ -> nat
      -> nat -> color -> color -> regularity -> __ packet -> __ chain -> __
      stored_triple -> __ chain -> __ -> __ -> 'a1) -> (__ -> __ chain -> __
      chain -> __ stored_triple -> __ chain -> __ -> __ -> 'a1) -> 'a2
      cadeque -> 'a2 stored_triple -> 'a1 **)

  let ground_inject_elim f2 f3 f4 d x =
    ground_inject_graph_mut (fun _ _ _ x0 -> x0 __)
      (fun _ _ _ _ _ _ _ _ _ _ x0 -> x0 __) (fun _ _ _ _ _ x0 -> x0 __) f2 f3
      f4 d x (ground_inject d x) (ground_inject_graph_correct d x)

  (** val coq_FunctionalElimination_ground_inject :
      (__ -> __ stored_triple -> __ chain -> __ -> __ -> __) -> (__ -> nat ->
      nat -> color -> color -> regularity -> __ packet -> __ chain -> __
      stored_triple -> __ chain -> __ -> __ -> __) -> (__ -> __ chain -> __
      chain -> __ stored_triple -> __ chain -> __ -> __ -> __) -> __ cadeque
      -> __ stored_triple -> __ **)

  let coq_FunctionalElimination_ground_inject =
    ground_inject_elim

  (** val coq_FunctionalInduction_ground_inject :
      (__ -> __ cadeque -> __ stored_triple -> __ cadeque)
      coq_FunctionalInduction **)

  let coq_FunctionalInduction_ground_inject =
    Obj.magic (fun _ -> ground_inject_graph_correct)

  (** val push : 'a1 -> 'a1 cadeque -> 'a1 cadeque **)

  let push x d =
    ground_push (Ground x) d

  type 'a push_graph =
  | Coq_push_graph_equation_1 of 'a * 'a cadeque

  (** val push_graph_rect :
      (__ -> __ -> __ cadeque -> 'a1) -> 'a2 -> 'a2 cadeque -> 'a2 cadeque ->
      'a2 push_graph -> 'a1 **)

  let push_graph_rect f _ _ _ = function
  | Coq_push_graph_equation_1 (x, d) -> Obj.magic f __ x d

  (** val push_graph_correct : 'a1 -> 'a1 cadeque -> 'a1 push_graph **)

  let push_graph_correct x d =
    Coq_push_graph_equation_1 (x, d)

  (** val push_elim :
      (__ -> __ -> __ cadeque -> 'a1) -> 'a2 -> 'a2 cadeque -> 'a1 **)

  let push_elim f x d =
    let Coq_push_graph_equation_1 (x0, d0) = push_graph_correct x d in
    Obj.magic f __ x0 d0

  (** val coq_FunctionalElimination_push :
      (__ -> __ -> __ cadeque -> __) -> __ -> __ cadeque -> __ **)

  let coq_FunctionalElimination_push =
    push_elim

  (** val coq_FunctionalInduction_push :
      (__ -> __ -> __ cadeque -> __ cadeque) coq_FunctionalInduction **)

  let coq_FunctionalInduction_push =
    Obj.magic (fun _ -> push_graph_correct)

  (** val inject : 'a1 cadeque -> 'a1 -> 'a1 cadeque **)

  let inject d x =
    ground_inject d (Ground x)

  type 'a inject_graph =
  | Coq_inject_graph_equation_1 of 'a cadeque * 'a

  (** val inject_graph_rect :
      (__ -> __ cadeque -> __ -> 'a1) -> 'a2 cadeque -> 'a2 -> 'a2 cadeque ->
      'a2 inject_graph -> 'a1 **)

  let inject_graph_rect f _ _ _ = function
  | Coq_inject_graph_equation_1 (d, x) -> Obj.magic f __ d x

  (** val inject_graph_correct : 'a1 cadeque -> 'a1 -> 'a1 inject_graph **)

  let inject_graph_correct d x =
    Coq_inject_graph_equation_1 (d, x)

  (** val inject_elim :
      (__ -> __ cadeque -> __ -> 'a1) -> 'a2 cadeque -> 'a2 -> 'a1 **)

  let inject_elim f d x =
    let Coq_inject_graph_equation_1 (d0, x0) = inject_graph_correct d x in
    Obj.magic f __ d0 x0

  (** val coq_FunctionalElimination_inject :
      (__ -> __ cadeque -> __ -> __) -> __ cadeque -> __ -> __ **)

  let coq_FunctionalElimination_inject =
    inject_elim

  (** val coq_FunctionalInduction_inject :
      (__ -> __ cadeque -> __ -> __ cadeque) coq_FunctionalInduction **)

  let coq_FunctionalInduction_inject =
    Obj.magic (fun _ -> inject_graph_correct)

  (** val pop_clause_2_clause_1 :
      nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain
      -> 'a1 -> nat -> color -> color -> 'a1 chain -> 'a1 chain -> ('a1, 'a1
      cadeque) prod option **)

  let pop_clause_2_clause_1 _ _ _ _ _ _ _ x ck _ _ _ refine =
    Some (Coq_pair (x, (T (ck, refine))))

  (** val pop_clause_2 :
      nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain
      -> ('a1 stored_triple, 'a1 semi_cadeque) prod -> ('a1, 'a1 cadeque)
      prod option **)

  let pop_clause_2 _ _ _ _ _ _ _ = function
  | Coq_pair (s, s0) ->
    (match s with
     | Ground y ->
       let Semi (_, ck, cl, cr, c) = s0 in
       Some (Coq_pair (y, (T (ck, (ensure_green O ck Coq_only cl cr c)))))
     | _ -> assert false (* absurd case *))

  (** val pop_clause_2_1_clause_1 :
      'a1 chain -> 'a1 chain -> 'a1 -> nat -> color -> color -> 'a1 chain ->
      'a1 chain -> ('a1, 'a1 cadeque) prod option **)

  let pop_clause_2_1_clause_1 _ _ x ck _ _ _ refine =
    Some (Coq_pair (x, (T (ck, refine))))

  (** val pop_clause_2_1 :
      'a1 chain -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) prod ->
      ('a1, 'a1 cadeque) prod option **)

  let pop_clause_2_1 _ _ = function
  | Coq_pair (s, s0) ->
    (match s with
     | Ground y ->
       let Semi (_, ck, cl, cr, c) = s0 in
       Some (Coq_pair (y, (T (ck, (ensure_green O ck Coq_only cl cr c)))))
     | _ -> assert false (* absurd case *))

  (** val pop : 'a1 cadeque -> ('a1, 'a1 cadeque) prod option **)

  let pop = function
  | T (_, c) ->
    (match c with
     | Empty _ -> None
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       let Coq_pair (s, s0) =
         pop_green O O (Single (O, tlvl, ck, Coq_only, (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), cl, cr, r, p, c0))
       in
       (match s with
        | Ground y ->
          let Semi (_, ck0, cl0, cr0, c1) = s0 in
          Some (Coq_pair (y, (T (ck0,
          (ensure_green O ck0 Coq_only cl0 cr0 c1)))))
        | _ -> assert false (* absurd case *))
     | Pair (_, _, _, c0, c1) ->
       let Coq_pair (s, s0) =
         pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1))
       in
       (match s with
        | Ground y ->
          let Semi (_, ck, cl, cr, c2) = s0 in
          Some (Coq_pair (y, (T (ck, (ensure_green O ck Coq_only cl cr c2)))))
        | _ -> assert false (* absurd case *)))

  type 'a pop_graph =
  | Coq_pop_graph_equation_1
  | Coq_pop_graph_refinement_2 of nat * nat * color * color * regularity
     * 'a packet * 'a chain * 'a pop_clause_2_graph
  | Coq_pop_graph_refinement_3 of 'a chain * 'a chain
     * 'a pop_clause_2_1_graph
  and 'a pop_clause_2_graph =
  | Coq_pop_clause_2_graph_refinement_1 of nat * nat * color * color
     * regularity * 'a packet * 'a chain * 'a * nat * color * color
     * 'a chain * 'a pop_clause_2_clause_1_graph
  and 'a0 pop_clause_2_clause_1_graph =
  | Coq_pop_clause_2_clause_1_graph_equation_1 of nat * nat * color *
     color * regularity * 'a0 packet * 'a0 chain * 'a0 * nat * color *
     color * 'a0 chain * 'a0 chain
  and 'a pop_clause_2_1_graph =
  | Coq_pop_clause_2_1_graph_refinement_1 of 'a chain * 'a chain * 'a *
     nat * color * color * 'a chain * 'a pop_clause_2_1_clause_1_graph
  and 'a0 pop_clause_2_1_clause_1_graph =
  | Coq_pop_clause_2_1_clause_1_graph_equation_1 of 'a0 chain * 'a0 chain
     * 'a0 * nat * color * color * 'a0 chain * 'a0 chain

  (** val pop_clause_2_1_clause_1_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
      chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
      nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ -> __
      pop_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ -> nat ->
      color -> color -> __ chain -> __ chain -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ ->
      __ pop_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain -> __
      -> __ -> 'a5) -> 'a6 chain -> 'a6 chain -> 'a6 -> nat -> color -> color
      -> 'a6 chain -> 'a6 chain -> ('a6, 'a6 cadeque) prod option -> 'a6
      pop_clause_2_1_clause_1_graph -> 'a5 **)

  let pop_clause_2_1_clause_1_graph_mut _ _ _ _ _ _ f5 _ _ _ _ _ _ _ _ _ = function
  | Coq_pop_clause_2_1_clause_1_graph_equation_1 (c0, c1, x, ck, cl, cr,
                                                  chain1, chain2) ->
    Obj.magic f5 __ c0 c1 x ck cl cr chain1 chain2 __ __

  (** val pop_clause_2_1_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
      chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
      nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ -> __
      pop_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ -> nat ->
      color -> color -> __ chain -> __ chain -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ ->
      __ pop_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain -> __
      -> __ -> 'a5) -> 'a6 chain -> 'a6 chain -> ('a6 stored_triple, 'a6
      semi_cadeque) prod -> ('a6, 'a6 cadeque) prod option -> 'a6
      pop_clause_2_1_graph -> 'a4 **)

  let pop_clause_2_1_graph_mut f f0 f1 f2 f3 f4 f5 c0 c1 refine s p =
    let rec _f6 _ _ _ = function (* # mod # -> added _ before f6 *)
    | Coq_pop_graph_equation_1 -> f __
    | Coq_pop_graph_refinement_2 (tlvl, ck0, cl, cr, r, p1, c, hind) ->
      f0 __ tlvl ck0 cl cr r p1 c hind
        (_f7 __ tlvl ck0 cl cr r p1 c
          (pop_green O O (Single (O, tlvl, ck0, Coq_only, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), cl, cr, r, p1, c)))
          (let Coq_pair (s0, s1) =
             pop_green O O (Single (O, tlvl, ck0, Coq_only, (Mix (SomeGreen,
               NoYellow, NoOrange, NoRed)), cl, cr, r, p1, c))
           in
           (match s0 with
            | Ground y ->
              let Semi (_, ck, cl0, cr0, c2) = s1 in
              Some (Coq_pair (y, (T (ck,
              (ensure_green O ck Coq_only cl0 cr0 c2)))))
            | _ -> assert false (* absurd case *))) hind)
    | Coq_pop_graph_refinement_3 (c2, c3, hind) ->
      f1 __ c2 c3 hind
        (Obj.magic f9 c2 c3
          (pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c2, c3)))
          (let Coq_pair (s0, s1) =
             pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c2, c3))
           in
           (match s0 with
            | Ground y ->
              let Semi (_, ck, cl, cr, c) = s1 in
              Some (Coq_pair (y, (T (ck,
              (ensure_green O ck Coq_only cl cr c)))))
            | _ -> assert false (* absurd case *))) hind)
    and _f7 _ _ _ _ _ _ _ _ _ _ = function (* # mod # -> added _ before f7 *)
    | Coq_pop_clause_2_graph_refinement_1 (tlvl, ck0, cl, cr, r, p1, c, x,
                                           ck, cl0, cr0, chain1, hind) ->
      f2 __ tlvl ck0 cl cr r p1 c x ck cl0 cr0 chain1 __ hind
        (_f8 __ tlvl ck0 cl cr r p1 c x ck cl0 cr0 chain1
          (ensure_green O ck Coq_only cl0 cr0 chain1) __ (Some (Coq_pair (x,
          (T (ck, (ensure_green O ck Coq_only cl0 cr0 chain1)))))) hind)
    and _f8 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
      (* # mod # -> added _ before f8 *)
    | Coq_pop_clause_2_clause_1_graph_equation_1 (tlvl, ck0, cl, cr, r, p1,
                                                  c, x, ck, cl0, cr0, chain1,
                                                  chain2) ->
      f3 __ tlvl ck0 cl cr r p1 c x ck cl0 cr0 chain1 chain2 __ __
    and f9 _ _ _ _ = function
    | Coq_pop_clause_2_1_graph_refinement_1 (c2, c3, x, ck, cl, cr, chain1,
                                             hind) ->
      Obj.magic f4 __ c2 c3 x ck cl cr chain1 __ hind
        (f10 __ c2 c3 x ck cl cr chain1
          (ensure_green O ck Coq_only cl cr chain1) __ (Some (Coq_pair (x, (T
          (ck, (ensure_green O ck Coq_only cl cr chain1)))))) hind)
    and f10 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_pop_clause_2_1_clause_1_graph_equation_1 (c2, c3, x, ck, cl, cr,
                                                    chain1, chain2) ->
      Obj.magic f5 __ c2 c3 x ck cl cr chain1 chain2 __ __
    in f9 c0 c1 refine s p

  (** val pop_clause_2_clause_1_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
      chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
      nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ -> __
      pop_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ -> nat ->
      color -> color -> __ chain -> __ chain -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ ->
      __ pop_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain -> __
      -> __ -> 'a5) -> nat -> nat -> color -> color -> regularity -> 'a6
      packet -> 'a6 chain -> 'a6 -> nat -> color -> color -> 'a6 chain -> 'a6
      chain -> ('a6, 'a6 cadeque) prod option -> 'a6
      pop_clause_2_clause_1_graph -> 'a3 **)

  let pop_clause_2_clause_1_graph_mut _ _ _ _ f3 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
  | Coq_pop_clause_2_clause_1_graph_equation_1 (tlvl, ck0, cl, cr, r, p, c,
                                                x, ck, cl0, cr0, chain1,
                                                chain2) ->
    Obj.magic f3 __ tlvl ck0 cl cr r p c x ck cl0 cr0 chain1 chain2 __ __

  (** val pop_clause_2_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
      chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
      nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ -> __
      pop_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ -> nat ->
      color -> color -> __ chain -> __ chain -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ ->
      __ pop_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain -> __
      -> __ -> 'a5) -> nat -> nat -> color -> color -> regularity -> 'a6
      packet -> 'a6 chain -> ('a6 stored_triple, 'a6 semi_cadeque) prod ->
      ('a6, 'a6 cadeque) prod option -> 'a6 pop_clause_2_graph -> 'a2 **)

  let pop_clause_2_graph_mut f f0 f1 f2 f3 f4 f5 tlvl ck0 cl cr r p c refine s p0 =
    let rec _f6 _ _ _ = function (* # mod # -> added _ before f6 *)
    | Coq_pop_graph_equation_1 -> f __
    | Coq_pop_graph_refinement_2 (tlvl0, ck1, cl0, cr0, r0, p2, c0, hind) ->
      Obj.magic f0 __ tlvl0 ck1 cl0 cr0 r0 p2 c0 hind
        (f7 tlvl0 ck1 cl0 cr0 r0 p2 c0
          (pop_green O O (Single (O, tlvl0, ck1, Coq_only, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), cl0, cr0, r0, p2, c0)))
          (let Coq_pair (s0, s1) =
             pop_green O O (Single (O, tlvl0, ck1, Coq_only, (Mix (SomeGreen,
               NoYellow, NoOrange, NoRed)), cl0, cr0, r0, p2, c0))
           in
           (match s0 with
            | Ground y ->
              let Semi (_, ck, cl1, cr1, c1) = s1 in
              Some (Coq_pair (y, (T (ck,
              (ensure_green O ck Coq_only cl1 cr1 c1)))))
            | _ -> assert false (* absurd case *))) hind)
    | Coq_pop_graph_refinement_3 (c0, c1, hind) ->
      Obj.magic f1 __ c0 c1 hind
        (_f9 __ c0 c1
          (pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1)))
          (let Coq_pair (s0, s1) =
             pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1))
           in
           (match s0 with
            | Ground y ->
              let Semi (_, ck, cl0, cr0, c2) = s1 in
              Some (Coq_pair (y, (T (ck,
              (ensure_green O ck Coq_only cl0 cr0 c2)))))
            | _ -> assert false (* absurd case *))) hind)
    and f7 _ _ _ _ _ _ _ _ _ = function
    | Coq_pop_clause_2_graph_refinement_1 (tlvl0, ck1, cl0, cr0, r0, p2, c0,
                                           x, ck, cl1, cr1, chain1, hind) ->
      Obj.magic f2 __ tlvl0 ck1 cl0 cr0 r0 p2 c0 x ck cl1 cr1 chain1 __ hind
        (f8 __ tlvl0 ck1 cl0 cr0 r0 p2 c0 x ck cl1 cr1 chain1
          (ensure_green O ck Coq_only cl1 cr1 chain1) __ (Some (Coq_pair (x,
          (T (ck, (ensure_green O ck Coq_only cl1 cr1 chain1)))))) hind)
    and f8 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_pop_clause_2_clause_1_graph_equation_1 (tlvl0, ck1, cl0, cr0, r0,
                                                  p2, c0, x, ck, cl1, cr1,
                                                  chain1, chain2) ->
      Obj.magic f3 __ tlvl0 ck1 cl0 cr0 r0 p2 c0 x ck cl1 cr1 chain1 chain2
        __ __
    and _f9 _ _ _ _ _ = function (* # mod # -> added _ before f9 *)
    | Coq_pop_clause_2_1_graph_refinement_1 (c0, c1, x, ck, cl0, cr0, chain1,
                                             hind) ->
      Obj.magic f4 __ c0 c1 x ck cl0 cr0 chain1 __ hind
        (_f10 __ c0 c1 x ck cl0 cr0 chain1
          (ensure_green O ck Coq_only cl0 cr0 chain1) __ (Some (Coq_pair (x,
          (T (ck, (ensure_green O ck Coq_only cl0 cr0 chain1)))))) hind)
    and _f10 _ _ _ _ _ _ _ _ _ _ _ = function
        (* # mod # -> added _ before f10 *)
    | Coq_pop_clause_2_1_clause_1_graph_equation_1 (c0, c1, x, ck, cl0, cr0,
                                                    chain1, chain2) ->
      Obj.magic f5 __ c0 c1 x ck cl0 cr0 chain1 chain2 __ __
    in f7 tlvl ck0 cl cr r p c refine s p0

  (** val pop_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
      chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
      nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ -> __
      pop_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ -> nat ->
      color -> color -> __ chain -> __ chain -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ ->
      __ pop_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain -> __
      -> __ -> 'a5) -> 'a6 cadeque -> ('a6, 'a6 cadeque) prod option -> 'a6
      pop_graph -> 'a1 **)

  let pop_graph_mut f f0 f1 f2 f3 f4 f5 d s p =
    let rec f6 _ _ = function
    | Coq_pop_graph_equation_1 -> f __
    | Coq_pop_graph_refinement_2 (tlvl, ck0, cl, cr, r, p1, c, hind) ->
      Obj.magic f0 __ tlvl ck0 cl cr r p1 c hind
        (f7 __ tlvl ck0 cl cr r p1 c
          (pop_green O O (Single (O, tlvl, ck0, Coq_only, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), cl, cr, r, p1, c)))
          (let Coq_pair (s0, s1) =
             pop_green O O (Single (O, tlvl, ck0, Coq_only, (Mix (SomeGreen,
               NoYellow, NoOrange, NoRed)), cl, cr, r, p1, c))
           in
           (match s0 with
            | Ground y ->
              let Semi (_, ck, cl0, cr0, c0) = s1 in
              Some (Coq_pair (y, (T (ck,
              (ensure_green O ck Coq_only cl0 cr0 c0)))))
            | _ -> assert false (* absurd case *))) hind)
    | Coq_pop_graph_refinement_3 (c0, c1, hind) ->
      Obj.magic f1 __ c0 c1 hind
        (f9 __ c0 c1
          (pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1)))
          (let Coq_pair (s0, s1) =
             pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1))
           in
           (match s0 with
            | Ground y ->
              let Semi (_, ck, cl, cr, c) = s1 in
              Some (Coq_pair (y, (T (ck,
              (ensure_green O ck Coq_only cl cr c)))))
            | _ -> assert false (* absurd case *))) hind)
    and f7 _ _ _ _ _ _ _ _ _ _ = function
    | Coq_pop_clause_2_graph_refinement_1 (tlvl, ck0, cl, cr, r, p1, c, x,
                                           ck, cl0, cr0, chain1, hind) ->
      Obj.magic f2 __ tlvl ck0 cl cr r p1 c x ck cl0 cr0 chain1 __ hind
        (f8 __ tlvl ck0 cl cr r p1 c x ck cl0 cr0 chain1
          (ensure_green O ck Coq_only cl0 cr0 chain1) __ (Some (Coq_pair (x,
          (T (ck, (ensure_green O ck Coq_only cl0 cr0 chain1)))))) hind)
    and f8 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_pop_clause_2_clause_1_graph_equation_1 (tlvl, ck0, cl, cr, r, p1,
                                                  c, x, ck, cl0, cr0, chain1,
                                                  chain2) ->
      Obj.magic f3 __ tlvl ck0 cl cr r p1 c x ck cl0 cr0 chain1 chain2 __ __
    and f9 _ _ _ _ _ = function
    | Coq_pop_clause_2_1_graph_refinement_1 (c0, c1, x, ck, cl, cr, chain1,
                                             hind) ->
      Obj.magic f4 __ c0 c1 x ck cl cr chain1 __ hind
        (f10 __ c0 c1 x ck cl cr chain1
          (ensure_green O ck Coq_only cl cr chain1) __ (Some (Coq_pair (x, (T
          (ck, (ensure_green O ck Coq_only cl cr chain1)))))) hind)
    and f10 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_pop_clause_2_1_clause_1_graph_equation_1 (c0, c1, x, ck, cl, cr,
                                                    chain1, chain2) ->
      Obj.magic f5 __ c0 c1 x ck cl cr chain1 chain2 __ __
    in f6 d s p

  (** val pop_graph_rect :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ pop_clause_2_graph -> 'a2 -> 'a1) -> (__ -> __
      chain -> __ chain -> __ pop_clause_2_1_graph -> 'a4 -> 'a1) -> (__ ->
      nat -> nat -> color -> color -> regularity -> __ packet -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ -> __
      pop_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> __ -> nat ->
      color -> color -> __ chain -> __ chain -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> __ -> nat -> color -> color -> __ chain -> __ ->
      __ pop_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> __ -> nat -> color -> color -> __ chain -> __ chain -> __
      -> __ -> 'a5) -> 'a6 cadeque -> ('a6, 'a6 cadeque) prod option -> 'a6
      pop_graph -> 'a1 **)

  let pop_graph_rect =
    pop_graph_mut

  (** val pop_graph_correct : 'a1 cadeque -> 'a1 pop_graph **)

  let pop_graph_correct = function
  | T (_, c) ->
    (match c with
     | Empty _ -> Coq_pop_graph_equation_1
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       Coq_pop_graph_refinement_2 (tlvl, ck, cl, cr, r, p, c0,
         (let refine =
            pop_green O O (Single (O, tlvl, ck, Coq_only, (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)), cl, cr, r, p, c0))
          in
          let Coq_pair (s, s0) = refine in
          (match s with
           | Ground y ->
             let Semi (_, ck0, cl0, cr0, c1) = s0 in
             Coq_pop_clause_2_graph_refinement_1 (tlvl, ck, cl, cr, r, p, c0,
             y, ck0, cl0, cr0, c1,
             (let refine0 = ensure_green O ck0 Coq_only cl0 cr0 c1 in
              Coq_pop_clause_2_clause_1_graph_equation_1 (tlvl, ck, cl, cr,
              r, p, c0, y, ck0, cl0, cr0, c1, refine0)))
           | _ -> assert false (* absurd case *))))
     | Pair (_, _, _, c0, c1) ->
       Coq_pop_graph_refinement_3 (c0, c1,
         (let refine =
            pop_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1))
          in
          let Coq_pair (s, s0) = refine in
          (match s with
           | Ground y ->
             let Semi (_, ck, cl, cr, c2) = s0 in
             Coq_pop_clause_2_1_graph_refinement_1 (c0, c1, y, ck, cl, cr,
             c2,
             (let refine0 = ensure_green O ck Coq_only cl cr c2 in
              Coq_pop_clause_2_1_clause_1_graph_equation_1 (c0, c1, y, ck,
              cl, cr, c2, refine0)))
           | _ -> assert false (* absurd case *)))))

  (** val pop_elim :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __
      chain -> __ -> __ -> __ -> __ -> 'a1) -> (__ -> __ chain -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
      -> __ -> 'a1) -> 'a2 cadeque -> 'a1 **)

  let pop_elim f f3 f5 d =
    pop_graph_mut f (fun _ _ _ _ _ _ _ _ _ x -> x __) (fun _ _ _ _ x ->
      x __) (fun _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x -> x __) f3
      (fun _ _ _ _ _ _ _ _ _ _ x -> x __) f5 d (pop d) (pop_graph_correct d)

  (** val coq_FunctionalElimination_pop :
      (__ -> __) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ -> nat -> color -> color -> __ chain -> __
      chain -> __ -> __ -> __ -> __ -> __) -> (__ -> __ chain -> __ chain ->
      __ -> nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __
      -> __ -> __) -> __ cadeque -> __ **)

  let coq_FunctionalElimination_pop =
    pop_elim

  (** val coq_FunctionalInduction_pop :
      (__ -> __ cadeque -> (__, __ cadeque) prod option)
      coq_FunctionalInduction **)

  let coq_FunctionalInduction_pop =
    Obj.magic (fun _ -> pop_graph_correct)

  (** val eject_clause_2_clause_1 :
      nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain
      -> nat -> color -> color -> 'a1 chain -> 'a1 chain -> 'a1 -> ('a1
      cadeque, 'a1) prod option **)

  let eject_clause_2_clause_1 _ _ _ _ _ _ _ ck _ _ _ refine x =
    Some (Coq_pair ((T (ck, refine)), x))

  (** val eject_clause_2 :
      nat -> nat -> color -> color -> regularity -> 'a1 packet -> 'a1 chain
      -> ('a1 semi_cadeque, 'a1 stored_triple) prod -> ('a1 cadeque, 'a1)
      prod option **)

  let eject_clause_2 _ _ _ _ _ _ _ = function
  | Coq_pair (s, s0) ->
    let Semi (_, ck, cl, cr, c) = s in
    (match s0 with
     | Ground y ->
       Some (Coq_pair ((T (ck, (ensure_green O ck Coq_only cl cr c))), y))
     | _ -> assert false (* absurd case *))

  (** val eject_clause_2_1_clause_1 :
      'a1 chain -> 'a1 chain -> nat -> color -> color -> 'a1 chain -> 'a1
      chain -> 'a1 -> ('a1 cadeque, 'a1) prod option **)

  let eject_clause_2_1_clause_1 _ _ ck _ _ _ refine x =
    Some (Coq_pair ((T (ck, refine)), x))

  (** val eject_clause_2_1 :
      'a1 chain -> 'a1 chain -> ('a1 semi_cadeque, 'a1 stored_triple) prod ->
      ('a1 cadeque, 'a1) prod option **)

  let eject_clause_2_1 _ _ = function
  | Coq_pair (s, s0) ->
    let Semi (_, ck, cl, cr, c) = s in
    (match s0 with
     | Ground y ->
       Some (Coq_pair ((T (ck, (ensure_green O ck Coq_only cl cr c))), y))
     | _ -> assert false (* absurd case *))

  (** val eject : 'a1 cadeque -> ('a1 cadeque, 'a1) prod option **)

  let eject = function
  | T (_, c) ->
    (match c with
     | Empty _ -> None
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       let Coq_pair (s, s0) =
         eject_green O O (Single (O, tlvl, ck, Coq_only, (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), cl, cr, r, p, c0))
       in
       let Semi (_, ck0, cl0, cr0, c1) = s in
       (match s0 with
        | Ground y ->
          Some (Coq_pair ((T (ck0,
            (ensure_green O ck0 Coq_only cl0 cr0 c1))), y))
        | _ -> assert false (* absurd case *))
     | Pair (_, _, _, c0, c1) ->
       let Coq_pair (s, s0) =
         eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1))
       in
       let Semi (_, ck, cl, cr, c2) = s in
       (match s0 with
        | Ground y ->
          Some (Coq_pair ((T (ck, (ensure_green O ck Coq_only cl cr c2))), y))
        | _ -> assert false (* absurd case *)))

  type 'a eject_graph =
  | Coq_eject_graph_equation_1
  | Coq_eject_graph_refinement_2 of nat * nat * color * color * regularity
     * 'a packet * 'a chain * 'a eject_clause_2_graph
  | Coq_eject_graph_refinement_3 of 'a chain * 'a chain
     * 'a eject_clause_2_1_graph
  and 'a eject_clause_2_graph =
  | Coq_eject_clause_2_graph_refinement_1 of nat * nat * color * color
     * regularity * 'a packet * 'a chain * nat * color * color * 'a chain
     * 'a * 'a eject_clause_2_clause_1_graph
  and 'a0 eject_clause_2_clause_1_graph =
  | Coq_eject_clause_2_clause_1_graph_equation_1 of nat * nat * color *
     color * regularity * 'a0 packet * 'a0 chain * nat * color * color
     * 'a0 chain * 'a0 chain * 'a0
  and 'a eject_clause_2_1_graph =
  | Coq_eject_clause_2_1_graph_refinement_1 of 'a chain * 'a chain *
     nat * color * color * 'a chain * 'a * 'a eject_clause_2_1_clause_1_graph
  and 'a0 eject_clause_2_1_clause_1_graph =
  | Coq_eject_clause_2_1_clause_1_graph_equation_1 of 'a0 chain * 'a0 chain
     * nat * color * color * 'a0 chain * 'a0 chain * 'a0

  (** val eject_clause_2_1_clause_1_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ ->
      __ chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__
      -> nat -> nat -> color -> color -> regularity -> __ packet -> __ chain
      -> nat -> color -> color -> __ chain -> __ -> __ -> __
      eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> nat -> color
      -> color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ ->
      __ eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __
      -> __ -> 'a5) -> 'a6 chain -> 'a6 chain -> nat -> color -> color -> 'a6
      chain -> 'a6 chain -> 'a6 -> ('a6 cadeque, 'a6) prod option -> 'a6
      eject_clause_2_1_clause_1_graph -> 'a5 **)

  let eject_clause_2_1_clause_1_graph_mut _ _ _ _ _ _ f5 _ _ _ _ _ _ _ _ _ = function
  | Coq_eject_clause_2_1_clause_1_graph_equation_1 (c0, c1, ck, cl, cr,
                                                    chain1, chain2, x) ->
    Obj.magic f5 __ c0 c1 ck cl cr chain1 chain2 __ x __

  (** val eject_clause_2_1_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ ->
      __ chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__
      -> nat -> nat -> color -> color -> regularity -> __ packet -> __ chain
      -> nat -> color -> color -> __ chain -> __ -> __ -> __
      eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> nat -> color
      -> color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ ->
      __ eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __
      -> __ -> 'a5) -> 'a6 chain -> 'a6 chain -> ('a6 semi_cadeque, 'a6
      stored_triple) prod -> ('a6 cadeque, 'a6) prod option -> 'a6
      eject_clause_2_1_graph -> 'a4 **)

  let eject_clause_2_1_graph_mut f f0 f1 f2 f3 f4 f5 c0 c1 refine s e =
    let rec _f6 _ _ _ = function (* # mod # -> added _ before f6 *)
    | Coq_eject_graph_equation_1 -> f __
    | Coq_eject_graph_refinement_2 (tlvl, ck0, cl, cr, r, p, c, hind) ->
      f0 __ tlvl ck0 cl cr r p c hind
        (_f7 __ tlvl ck0 cl cr r p c
          (eject_green O O (Single (O, tlvl, ck0, Coq_only, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), cl, cr, r, p, c)))
          (let Coq_pair (s0, s1) =
             eject_green O O (Single (O, tlvl, ck0, Coq_only, (Mix
               (SomeGreen, NoYellow, NoOrange, NoRed)), cl, cr, r, p, c))
           in
           let Semi (_, ck, cl0, cr0, c2) = s0 in
           (match s1 with
            | Ground y ->
              Some (Coq_pair ((T (ck,
                (ensure_green O ck Coq_only cl0 cr0 c2))), y))
            | _ -> assert false (* absurd case *))) hind)
    | Coq_eject_graph_refinement_3 (c2, c3, hind) ->
      f1 __ c2 c3 hind
        (Obj.magic f9 c2 c3
          (eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c2, c3)))
          (let Coq_pair (s0, s1) =
             eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow,
               NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), c2, c3))
           in
           let Semi (_, ck, cl, cr, c) = s0 in
           (match s1 with
            | Ground y ->
              Some (Coq_pair ((T (ck, (ensure_green O ck Coq_only cl cr c))),
                y))
            | _ -> assert false (* absurd case *))) hind)
    and _f7 _ _ _ _ _ _ _ _ _ _ = function (* # mod # -> added _ before f7 *)
    | Coq_eject_clause_2_graph_refinement_1 (tlvl, ck0, cl, cr, r, p, c, ck,
                                             cl0, cr0, chain1, x, hind) ->
      f2 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1 x __ hind
        (_f8 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1
          (ensure_green O ck Coq_only cl0 cr0 chain1) x __ (Some (Coq_pair
          ((T (ck, (ensure_green O ck Coq_only cl0 cr0 chain1))), x))) hind)
    and _f8 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
        (* # mod # -> added _ before f8 *)
    | Coq_eject_clause_2_clause_1_graph_equation_1 (tlvl, ck0, cl, cr, r, p,
                                                    c, ck, cl0, cr0, chain1,
                                                    chain2, x) ->
      f3 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1 chain2 __ x __
    and f9 _ _ _ _ = function
    | Coq_eject_clause_2_1_graph_refinement_1 (c2, c3, ck, cl, cr, chain1, x,
                                               hind) ->
      Obj.magic f4 __ c2 c3 ck cl cr chain1 x __ hind
        (f10 __ c2 c3 ck cl cr chain1
          (ensure_green O ck Coq_only cl cr chain1) x __ (Some (Coq_pair ((T
          (ck, (ensure_green O ck Coq_only cl cr chain1))), x))) hind)
    and f10 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_eject_clause_2_1_clause_1_graph_equation_1 (c2, c3, ck, cl, cr,
                                                      chain1, chain2, x) ->
      Obj.magic f5 __ c2 c3 ck cl cr chain1 chain2 __ x __
    in f9 c0 c1 refine s e

  (** val eject_clause_2_clause_1_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ ->
      __ chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__
      -> nat -> nat -> color -> color -> regularity -> __ packet -> __ chain
      -> nat -> color -> color -> __ chain -> __ -> __ -> __
      eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> nat -> color
      -> color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ ->
      __ eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __
      -> __ -> 'a5) -> nat -> nat -> color -> color -> regularity -> 'a6
      packet -> 'a6 chain -> nat -> color -> color -> 'a6 chain -> 'a6 chain
      -> 'a6 -> ('a6 cadeque, 'a6) prod option -> 'a6
      eject_clause_2_clause_1_graph -> 'a3 **)

  let eject_clause_2_clause_1_graph_mut _ _ _ _ f3 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
  | Coq_eject_clause_2_clause_1_graph_equation_1 (tlvl, ck0, cl, cr, r, p, c,
                                                  ck, cl0, cr0, chain1,
                                                  chain2, x) ->
    Obj.magic f3 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1 chain2 __ x __

  (** val eject_clause_2_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ ->
      __ chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__
      -> nat -> nat -> color -> color -> regularity -> __ packet -> __ chain
      -> nat -> color -> color -> __ chain -> __ -> __ -> __
      eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> nat -> color
      -> color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ ->
      __ eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __
      -> __ -> 'a5) -> nat -> nat -> color -> color -> regularity -> 'a6
      packet -> 'a6 chain -> ('a6 semi_cadeque, 'a6 stored_triple) prod ->
      ('a6 cadeque, 'a6) prod option -> 'a6 eject_clause_2_graph -> 'a2 **)

  let eject_clause_2_graph_mut f f0 f1 f2 f3 f4 f5 tlvl ck0 cl cr r p c refine s e =
    let rec _f6 _ _ _ = function (* # mod # -> added _ before f6 *)
    | Coq_eject_graph_equation_1 -> f __
    | Coq_eject_graph_refinement_2 (tlvl0, ck1, cl0, cr0, r0, p0, c0, hind) ->
      Obj.magic f0 __ tlvl0 ck1 cl0 cr0 r0 p0 c0 hind
        (f7 tlvl0 ck1 cl0 cr0 r0 p0 c0
          (eject_green O O (Single (O, tlvl0, ck1, Coq_only, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), cl0, cr0, r0, p0, c0)))
          (let Coq_pair (s0, s1) =
             eject_green O O (Single (O, tlvl0, ck1, Coq_only, (Mix
               (SomeGreen, NoYellow, NoOrange, NoRed)), cl0, cr0, r0, p0, c0))
           in
           let Semi (_, ck, cl1, cr1, c1) = s0 in
           (match s1 with
            | Ground y ->
              Some (Coq_pair ((T (ck,
                (ensure_green O ck Coq_only cl1 cr1 c1))), y))
            | _ -> assert false (* absurd case *))) hind)
    | Coq_eject_graph_refinement_3 (c0, c1, hind) ->
      Obj.magic f1 __ c0 c1 hind
        (_f9 __ c0 c1
          (eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1)))
          (let Coq_pair (s0, s1) =
             eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow,
               NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), c0, c1))
           in
           let Semi (_, ck, cl0, cr0, c2) = s0 in
           (match s1 with
            | Ground y ->
              Some (Coq_pair ((T (ck,
                (ensure_green O ck Coq_only cl0 cr0 c2))), y))
            | _ -> assert false (* absurd case *))) hind)
    and f7 _ _ _ _ _ _ _ _ _ = function
    | Coq_eject_clause_2_graph_refinement_1 (tlvl0, ck1, cl0, cr0, r0, p0,
                                             c0, ck, cl1, cr1, chain1, x, hind) ->
      Obj.magic f2 __ tlvl0 ck1 cl0 cr0 r0 p0 c0 ck cl1 cr1 chain1 x __ hind
        (f8 __ tlvl0 ck1 cl0 cr0 r0 p0 c0 ck cl1 cr1 chain1
          (ensure_green O ck Coq_only cl1 cr1 chain1) x __ (Some (Coq_pair
          ((T (ck, (ensure_green O ck Coq_only cl1 cr1 chain1))), x))) hind)
    and f8 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_eject_clause_2_clause_1_graph_equation_1 (tlvl0, ck1, cl0, cr0, r0,
                                                    p0, c0, ck, cl1, cr1,
                                                    chain1, chain2, x) ->
      Obj.magic f3 __ tlvl0 ck1 cl0 cr0 r0 p0 c0 ck cl1 cr1 chain1 chain2 __
        x __
    and _f9 _ _ _ _ _ = function (* # mod # -> added _ before f9 *)
    | Coq_eject_clause_2_1_graph_refinement_1 (c0, c1, ck, cl0, cr0, chain1,
                                               x, hind) ->
      Obj.magic f4 __ c0 c1 ck cl0 cr0 chain1 x __ hind
        (_f10 __ c0 c1 ck cl0 cr0 chain1
          (ensure_green O ck Coq_only cl0 cr0 chain1) x __ (Some (Coq_pair
          ((T (ck, (ensure_green O ck Coq_only cl0 cr0 chain1))), x))) hind)
    and _f10 _ _ _ _ _ _ _ _ _ _ _ = function
        (* # mod # -> added _ before f10 *)
    | Coq_eject_clause_2_1_clause_1_graph_equation_1 (c0, c1, ck, cl0, cr0,
                                                      chain1, chain2, x) ->
      Obj.magic f5 __ c0 c1 ck cl0 cr0 chain1 chain2 __ x __
    in f7 tlvl ck0 cl cr r p c refine s e

  (** val eject_graph_mut :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ ->
      __ chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__
      -> nat -> nat -> color -> color -> regularity -> __ packet -> __ chain
      -> nat -> color -> color -> __ chain -> __ -> __ -> __
      eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> nat -> color
      -> color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ ->
      __ eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __
      -> __ -> 'a5) -> 'a6 cadeque -> ('a6 cadeque, 'a6) prod option -> 'a6
      eject_graph -> 'a1 **)

  let eject_graph_mut f f0 f1 f2 f3 f4 f5 d s e =
    let rec f6 _ _ = function
    | Coq_eject_graph_equation_1 -> f __
    | Coq_eject_graph_refinement_2 (tlvl, ck0, cl, cr, r, p, c, hind) ->
      Obj.magic f0 __ tlvl ck0 cl cr r p c hind
        (f7 __ tlvl ck0 cl cr r p c
          (eject_green O O (Single (O, tlvl, ck0, Coq_only, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), cl, cr, r, p, c)))
          (let Coq_pair (s0, s1) =
             eject_green O O (Single (O, tlvl, ck0, Coq_only, (Mix
               (SomeGreen, NoYellow, NoOrange, NoRed)), cl, cr, r, p, c))
           in
           let Semi (_, ck, cl0, cr0, c0) = s0 in
           (match s1 with
            | Ground y ->
              Some (Coq_pair ((T (ck,
                (ensure_green O ck Coq_only cl0 cr0 c0))), y))
            | _ -> assert false (* absurd case *))) hind)
    | Coq_eject_graph_refinement_3 (c0, c1, hind) ->
      Obj.magic f1 __ c0 c1 hind
        (f9 __ c0 c1
          (eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c0, c1)))
          (let Coq_pair (s0, s1) =
             eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow,
               NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), c0, c1))
           in
           let Semi (_, ck, cl, cr, c) = s0 in
           (match s1 with
            | Ground y ->
              Some (Coq_pair ((T (ck, (ensure_green O ck Coq_only cl cr c))),
                y))
            | _ -> assert false (* absurd case *))) hind)
    and f7 _ _ _ _ _ _ _ _ _ _ = function
    | Coq_eject_clause_2_graph_refinement_1 (tlvl, ck0, cl, cr, r, p, c, ck,
                                             cl0, cr0, chain1, x, hind) ->
      Obj.magic f2 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1 x __ hind
        (f8 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1
          (ensure_green O ck Coq_only cl0 cr0 chain1) x __ (Some (Coq_pair
          ((T (ck, (ensure_green O ck Coq_only cl0 cr0 chain1))), x))) hind)
    and f8 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_eject_clause_2_clause_1_graph_equation_1 (tlvl, ck0, cl, cr, r, p,
                                                    c, ck, cl0, cr0, chain1,
                                                    chain2, x) ->
      Obj.magic f3 __ tlvl ck0 cl cr r p c ck cl0 cr0 chain1 chain2 __ x __
    and f9 _ _ _ _ _ = function
    | Coq_eject_clause_2_1_graph_refinement_1 (c0, c1, ck, cl, cr, chain1, x,
                                               hind) ->
      Obj.magic f4 __ c0 c1 ck cl cr chain1 x __ hind
        (f10 __ c0 c1 ck cl cr chain1
          (ensure_green O ck Coq_only cl cr chain1) x __ (Some (Coq_pair ((T
          (ck, (ensure_green O ck Coq_only cl cr chain1))), x))) hind)
    and f10 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_eject_clause_2_1_clause_1_graph_equation_1 (c0, c1, ck, cl, cr,
                                                      chain1, chain2, x) ->
      Obj.magic f5 __ c0 c1 ck cl cr chain1 chain2 __ x __
    in f6 d s e

  (** val eject_graph_rect :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> __ eject_clause_2_graph -> 'a2 -> 'a1) -> (__ ->
      __ chain -> __ chain -> __ eject_clause_2_1_graph -> 'a4 -> 'a1) -> (__
      -> nat -> nat -> color -> color -> regularity -> __ packet -> __ chain
      -> nat -> color -> color -> __ chain -> __ -> __ -> __
      eject_clause_2_clause_1_graph -> 'a3 -> 'a2) -> (__ -> nat -> nat ->
      color -> color -> regularity -> __ packet -> __ chain -> nat -> color
      -> color -> __ chain -> __ chain -> __ -> __ -> __ -> 'a3) -> (__ -> __
      chain -> __ chain -> nat -> color -> color -> __ chain -> __ -> __ ->
      __ eject_clause_2_1_clause_1_graph -> 'a5 -> 'a4) -> (__ -> __ chain ->
      __ chain -> nat -> color -> color -> __ chain -> __ chain -> __ -> __
      -> __ -> 'a5) -> 'a6 cadeque -> ('a6 cadeque, 'a6) prod option -> 'a6
      eject_graph -> 'a1 **)

  let eject_graph_rect =
    eject_graph_mut

  (** val eject_graph_correct : 'a1 cadeque -> 'a1 eject_graph **)

  let eject_graph_correct = function
  | T (_, c) ->
    (match c with
     | Empty _ -> Coq_eject_graph_equation_1
     | Single (_, tlvl, ck, _, _, cl, cr, r, p, c0) ->
       Coq_eject_graph_refinement_2 (tlvl, ck, cl, cr, r, p, c0,
         (let refine =
            eject_green O O (Single (O, tlvl, ck, Coq_only, (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)), cl, cr, r, p, c0))
          in
          let Coq_pair (s, s0) = refine in
          let Semi (_, ck0, cl0, cr0, c1) = s in
          (match s0 with
           | Ground y ->
             Coq_eject_clause_2_graph_refinement_1 (tlvl, ck, cl, cr, r, p,
               c0, ck0, cl0, cr0, c1, y,
               (let refine0 = ensure_green O ck0 Coq_only cl0 cr0 c1 in
                Coq_eject_clause_2_clause_1_graph_equation_1 (tlvl, ck, cl,
                cr, r, p, c0, ck0, cl0, cr0, c1, refine0, y)))
           | _ -> assert false (* absurd case *))))
     | Pair (_, _, _, c0, c1) ->
       Coq_eject_graph_refinement_3 (c0, c1,
         (let refine =
            eject_green O (S O) (Pair (O, (Mix (SomeGreen, NoYellow,
              NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), c0, c1))
          in
          let Coq_pair (s, s0) = refine in
          let Semi (_, ck, cl, cr, c2) = s in
          (match s0 with
           | Ground y ->
             Coq_eject_clause_2_1_graph_refinement_1 (c0, c1, ck, cl, cr, c2,
               y,
               (let refine0 = ensure_green O ck Coq_only cl cr c2 in
                Coq_eject_clause_2_1_clause_1_graph_equation_1 (c0, c1, ck,
                cl, cr, c2, refine0, y)))
           | _ -> assert false (* absurd case *)))))

  (** val eject_elim :
      (__ -> 'a1) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> nat -> color -> color -> __ chain -> __ chain ->
      __ -> __ -> __ -> __ -> __ -> 'a1) -> (__ -> __ chain -> __ chain ->
      nat -> color -> color -> __ chain -> __ chain -> __ -> __ -> __ -> __
      -> __ -> 'a1) -> 'a2 cadeque -> 'a1 **)

  let eject_elim f f3 f5 d =
    eject_graph_mut f (fun _ _ _ _ _ _ _ _ _ x -> x __) (fun _ _ _ _ x ->
      x __) (fun _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x -> x __) f3
      (fun _ _ _ _ _ _ _ _ _ _ x -> x __) f5 d (eject d)
      (eject_graph_correct d)

  (** val coq_FunctionalElimination_eject :
      (__ -> __) -> (__ -> nat -> nat -> color -> color -> regularity -> __
      packet -> __ chain -> nat -> color -> color -> __ chain -> __ chain ->
      __ -> __ -> __ -> __ -> __ -> __) -> (__ -> __ chain -> __ chain -> nat
      -> color -> color -> __ chain -> __ chain -> __ -> __ -> __ -> __ -> __
      -> __) -> __ cadeque -> __ **)

  let coq_FunctionalElimination_eject =
    eject_elim

  (** val coq_FunctionalInduction_eject :
      (__ -> __ cadeque -> (__ cadeque, __) prod option)
      coq_FunctionalInduction **)

  let coq_FunctionalInduction_eject =
    Obj.magic (fun _ -> eject_graph_correct)

  (** val concat_clause_1_clause_1 :
      nat -> 'a1 chain -> 'a1 stored_triple vector -> nat -> 'a1 chain -> 'a1
      cadeque -> 'a1 cadeque **)

  let concat_clause_1_clause_1 _ _ _ _ _ refine =
    refine

  (** val concat_clause_1_clause_2_clause_1 :
      nat -> 'a1 chain -> 'a1 triple -> nat -> 'a1 chain -> 'a1 stored_triple
      vector -> 'a1 cadeque -> 'a1 cadeque **)

  let concat_clause_1_clause_2_clause_1 _ _ _ _ _ _ refine =
    refine

  (** val concat_clause_1_clause_2_clause_2_clause_1 :
      nat -> 'a1 chain -> 'a1 triple -> 'a1 chain -> nat -> 'a1 chain -> 'a1
      triple -> 'a1 chain -> 'a1 cadeque **)

  let concat_clause_1_clause_2_clause_2_clause_1 _ _ _ refine _ _ _ refine0 =
    T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
      (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), refine, refine0)))

  (** val concat_clause_1_clause_2_clause_2 :
      nat -> 'a1 chain -> 'a1 triple -> 'a1 chain -> nat -> 'a1 chain -> 'a1
      triple -> 'a1 cadeque **)

  let concat_clause_1_clause_2_clause_2 _ _ _ refine _ _ tr =
    T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
      (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), refine,
      (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) tr))))

  (** val concat_clause_1_clause_2 :
      nat -> 'a1 chain -> 'a1 triple -> nat -> 'a1 chain -> 'a1
      left_right_triple -> 'a1 cadeque **)

  let concat_clause_1_clause_2 ck c1 tl _ _ = function
  | Not_enough (_, _, v) ->
    vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T (ck,
      c1)) (S (S (S (S (S (S O)))))) v
  | Ok_lrt (_, _, _, t) ->
    T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
      (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
      (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) tl),
      (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) t))))

  (** val concat_clause_1 :
      nat -> 'a1 chain -> 'a1 left_right_triple -> nat -> 'a1 chain -> 'a1
      cadeque **)

  let concat_clause_1 ck c1 refine ck0 c2 =
    match refine with
    | Not_enough (_, _, v) ->
      vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S (S
        (S (S O)))))) v (T (ck0, c2))
    | Ok_lrt (_, _, _, t) ->
      (match make_right O ck0 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
               (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c2 with
       | Not_enough (_, _, v) ->
         vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
           (ck, c1)) (S (S (S (S (S (S O)))))) v
       | Ok_lrt (_, _, _, t0) ->
         T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
           (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
             NoRed)) t),
           (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
             NoRed)) t0)))))

  (** val concat : 'a1 cadeque -> 'a1 cadeque -> 'a1 cadeque **)

  let concat d1 d2 =
    let T (ck, c) = d1 in
    let T (ck0, c0) = d2 in
    (match make_left O ck (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
             (SomeGreen, NoYellow, NoOrange, NoRed)) c with
     | Not_enough (_, _, v) ->
       vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S (S
         (S (S O)))))) v (T (ck0, c0))
     | Ok_lrt (_, _, _, t) ->
       (match make_right O ck0 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
                (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c0 with
        | Not_enough (_, _, v) ->
          vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck, c)) (S (S (S (S (S (S O)))))) v
        | Ok_lrt (_, _, _, t0) ->
          T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
            (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)) t),
            (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)) t0))))))

  type 'a concat_graph =
  | Coq_concat_graph_refinement_1 of nat * 'a chain * nat * 'a chain
     * 'a concat_clause_1_graph
  and 'a concat_clause_1_graph =
  | Coq_concat_clause_1_graph_refinement_1 of nat * 'a chain
     * 'a stored_triple vector * nat * 'a chain
     * 'a concat_clause_1_clause_1_graph
  | Coq_concat_clause_1_graph_refinement_2 of nat * 'a chain * 'a triple
     * nat * 'a chain * 'a concat_clause_1_clause_2_graph
  and 'a0 concat_clause_1_clause_1_graph =
  | Coq_concat_clause_1_clause_1_graph_equation_1 of nat * 'a0 chain
     * 'a0 stored_triple vector * nat * 'a0 chain * 'a0 cadeque
  and 'a1 concat_clause_1_clause_2_graph =
  | Coq_concat_clause_1_clause_2_graph_refinement_1 of nat * 'a1 chain
     * 'a1 triple * nat * 'a1 chain * 'a1 stored_triple vector
     * 'a1 concat_clause_1_clause_2_clause_1_graph
  | Coq_concat_clause_1_clause_2_graph_refinement_2 of nat * 'a1 chain
     * 'a1 triple * nat * 'a1 chain * 'a1 triple
     * 'a1 concat_clause_1_clause_2_clause_2_graph
  and 'a concat_clause_1_clause_2_clause_1_graph =
  | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 of nat * 'a chain
     * 'a triple * nat * 'a chain * 'a stored_triple vector * 'a cadeque
  and 'a0 concat_clause_1_clause_2_clause_2_graph =
  | Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 of nat
     * 'a0 chain * 'a0 triple * 'a0 chain * nat * 'a0 chain * 'a0 triple
     * 'a0 concat_clause_1_clause_2_clause_2_clause_1_graph
  and 'a0 concat_clause_1_clause_2_clause_2_clause_1_graph =
  | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 of
     nat * 'a0 chain * 'a0 triple * 'a0 chain * nat * 'a0 chain * 'a0 triple
     * 'a0 chain

  (** val concat_clause_1_clause_2_clause_2_clause_1_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain
      -> 'a8 triple -> 'a8 chain -> nat -> 'a8 chain -> 'a8 triple -> 'a8
      chain -> 'a8 cadeque -> 'a8
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 **)

  let concat_clause_1_clause_2_clause_2_clause_1_graph_mut _ _ _ _ _ _ _ _ f7 _ _ _ _ _ _ _ _ _ = function
  | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 (
      ck, c1, tl, cl, ck0, c2, tr, cr) ->
    Obj.magic f7 __ ck c1 tl cl __ __ ck0 c2 tr cr __ __

  (** val concat_clause_1_clause_2_clause_2_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain
      -> 'a8 triple -> 'a8 chain -> nat -> 'a8 chain -> 'a8 triple -> 'a8
      cadeque -> 'a8 concat_clause_1_clause_2_clause_2_graph -> 'a6 **)

  let concat_clause_1_clause_2_clause_2_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 ck c1 tl refine ck0 c2 tr s c =
    let rec _f8 _ _ _ _ = function (* # mod # -> added _ before f8 *)
    | Coq_concat_graph_refinement_1 (ck1, c3, ck2, c4, hind) ->
      f __ ck1 c3 ck2 c4 hind
        (_f9 __ ck1 c3
          (make_left O ck1 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c3) ck2 c4
          (match make_left O ck1 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c3 with
           | Not_enough (_, _, v) ->
             vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S
               (S (S (S (S O)))))) v (T (ck2, c4))
           | Ok_lrt (_, _, _, t) ->
             (match make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange,
                      NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c4 with
              | Not_enough (_, _, v) ->
                vector_inject cadeque_seq (stored_triple_seq O) ground_inject
                  (T (ck1, c3)) (S (S (S (S (S (S O)))))) v
              | Ok_lrt (_, _, _, t0) ->
                T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
                  NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                  (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t),
                  (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t0)))))) hind)
    and _f9 _ _ _ _ _ _ _ = function (* # mod # -> added _ before f9 *)
    | Coq_concat_clause_1_graph_refinement_1 (ck1, c3, v, ck2, c4, hind) ->
      f0 __ ck1 c3 v __ ck2 c4 hind
        (_f10 __ ck1 c3 v __ ck2 c4
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck2, c4)))
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck2, c4))) hind)
    | Coq_concat_clause_1_graph_refinement_2 (ck1, c3, tl0, ck2, c4, hind) ->
      f1 __ ck1 c3 tl0 __ ck2 c4 hind
        (_f11 __ ck1 c3 tl0 __ ck2 c4
          (make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c4)
          (match make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange,
                   NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c4 with
           | Not_enough (_, _, v) ->
             vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
               (ck1, c3)) (S (S (S (S (S (S O)))))) v
           | Ok_lrt (_, _, _, t) ->
             T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
               (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) tl0),
               (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) t))))) hind)
    and _f10 _ _ _ _ _ _ _ _ _ = function (* # mod # -> added _ before f10 *)
    | Coq_concat_clause_1_clause_1_graph_equation_1 (ck1, c3, v, ck2, c4, c5) ->
      f2 __ ck1 c3 v __ ck2 c4 c5 __
    and _f11 _ _ _ _ _ _ _ _ _ = function (* # mod # -> added _ before f11 *)
    | Coq_concat_clause_1_clause_2_graph_refinement_1 (ck1, c3, tl0, ck2, c4,
                                                       v, hind) ->
      f3 __ ck1 c3 tl0 __ ck2 c4 v __ hind
        (_f12 __ ck1 c3 tl0 __ ck2 c4 v
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck1, c3)) (S (S (S (S (S (S O)))))) v) __
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck1, c3)) (S (S (S (S (S (S O)))))) v) hind)
    | Coq_concat_clause_1_clause_2_graph_refinement_2 (ck1, c3, tl0, ck2, c4,
                                                       tr0, hind) ->
      f4 __ ck1 c3 tl0 __ ck2 c4 tr0 __ hind
        (Obj.magic f13 ck1 c3 tl0
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl0) ck2 c4 tr0 (T ((S (S O)), (Pair (O, (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)),
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl0),
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr0))))) hind)
    and _f12 _ _ _ _ _ _ _ _ _ _ _ = function
        (* # mod # -> added _ before f12 *)
    | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 (ck1, c3, tl0,
                                                              ck2, c4, v, c5) ->
      f5 __ ck1 c3 tl0 __ ck2 c4 v c5 __ __
    and f13 _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 (ck1, c3, tl0,
                                                                refine0, ck2,
                                                                c4, tr0, hind) ->
      Obj.magic f6 __ ck1 c3 tl0 refine0 __ ck2 c4 tr0 __ hind
        (f14 __ ck1 c3 tl0 refine0 __ ck2 c4 tr0
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr0) __ (T ((S (S O)), (Pair (O, (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), refine0,
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr0))))) hind)
    and f14 _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 (
        ck1, c3, tl0, cl, ck2, c4, tr0, cr) ->
      Obj.magic f7 __ ck1 c3 tl0 cl __ __ ck2 c4 tr0 cr __ __
    in f13 ck c1 tl refine ck0 c2 tr s c

  (** val concat_clause_1_clause_2_clause_1_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain
      -> 'a8 triple -> nat -> 'a8 chain -> 'a8 stored_triple vector -> 'a8
      cadeque -> 'a8 cadeque -> 'a8 concat_clause_1_clause_2_clause_1_graph
      -> 'a5 **)

  let concat_clause_1_clause_2_clause_1_graph_mut _ _ _ _ _ _ f5 _ _ _ _ _ _ _ _ _ _ = function
  | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 (ck, c1, tl, ck0,
                                                            c2, v, c3) ->
    Obj.magic f5 __ ck c1 tl __ ck0 c2 v c3 __ __

  (** val concat_clause_1_clause_2_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain
      -> 'a8 triple -> nat -> 'a8 chain -> 'a8 left_right_triple -> 'a8
      cadeque -> 'a8 concat_clause_1_clause_2_graph -> 'a4 **)

  let concat_clause_1_clause_2_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 ck c1 tl ck0 c2 refine s c =
    let rec _f8 _ _ _ _ = function (* # mod # -> added _ before f8 *)
    | Coq_concat_graph_refinement_1 (ck1, c3, ck2, c4, hind) ->
      f __ ck1 c3 ck2 c4 hind
        (_f9 __ ck1 c3
          (make_left O ck1 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c3) ck2 c4
          (match make_left O ck1 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c3 with
           | Not_enough (_, _, v) ->
             vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S
               (S (S (S (S O)))))) v (T (ck2, c4))
           | Ok_lrt (_, _, _, t) ->
             (match make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange,
                      NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c4 with
              | Not_enough (_, _, v) ->
                vector_inject cadeque_seq (stored_triple_seq O) ground_inject
                  (T (ck1, c3)) (S (S (S (S (S (S O)))))) v
              | Ok_lrt (_, _, _, t0) ->
                T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
                  NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                  (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t),
                  (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t0)))))) hind)
    and _f9 _ _ _ _ _ _ _ = function (* # mod # -> added _ before f9 *)
    | Coq_concat_clause_1_graph_refinement_1 (ck1, c3, v, ck2, c4, hind) ->
      f0 __ ck1 c3 v __ ck2 c4 hind
        (_f10 __ ck1 c3 v __ ck2 c4
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck2, c4)))
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck2, c4))) hind)
    | Coq_concat_clause_1_graph_refinement_2 (ck1, c3, tl0, ck2, c4, hind) ->
      f1 __ ck1 c3 tl0 __ ck2 c4 hind
        (Obj.magic f11 ck1 c3 tl0 ck2 c4
          (make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c4)
          (match make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange,
                   NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c4 with
           | Not_enough (_, _, v) ->
             vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
               (ck1, c3)) (S (S (S (S (S (S O)))))) v
           | Ok_lrt (_, _, _, t) ->
             T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
               (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) tl0),
               (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) t))))) hind)
    and _f10 _ _ _ _ _ _ _ _ _ = function (* # mod # -> added _ before f10 *)
    | Coq_concat_clause_1_clause_1_graph_equation_1 (ck1, c3, v, ck2, c4, c5) ->
      f2 __ ck1 c3 v __ ck2 c4 c5 __
    and f11 _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_graph_refinement_1 (ck1, c3, tl0, ck2, c4,
                                                       v, hind) ->
      Obj.magic f3 __ ck1 c3 tl0 __ ck2 c4 v __ hind
        (f12 __ ck1 c3 tl0 __ ck2 c4 v
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck1, c3)) (S (S (S (S (S (S O)))))) v) __
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck1, c3)) (S (S (S (S (S (S O)))))) v) hind)
    | Coq_concat_clause_1_clause_2_graph_refinement_2 (ck1, c3, tl0, ck2, c4,
                                                       tr, hind) ->
      Obj.magic f4 __ ck1 c3 tl0 __ ck2 c4 tr __ hind
        (f13 __ ck1 c3 tl0
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl0) __ ck2 c4 tr __ (T ((S (S O)), (Pair (O, (Mix
          (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)),
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl0),
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr))))) hind)
    and f12 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 (ck1, c3, tl0,
                                                              ck2, c4, v, c5) ->
      Obj.magic f5 __ ck1 c3 tl0 __ ck2 c4 v c5 __ __
    and f13 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 (ck1, c3, tl0,
                                                                refine0, ck2,
                                                                c4, tr, hind) ->
      Obj.magic f6 __ ck1 c3 tl0 refine0 __ ck2 c4 tr __ hind
        (f14 __ ck1 c3 tl0 refine0 __ ck2 c4 tr
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr) __ (T ((S (S O)), (Pair (O, (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), refine0,
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr))))) hind)
    and f14 _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 (
        ck1, c3, tl0, cl, ck2, c4, tr, cr) ->
      Obj.magic f7 __ ck1 c3 tl0 cl __ __ ck2 c4 tr cr __ __
    in f11 ck c1 tl ck0 c2 refine s c

  (** val concat_clause_1_clause_1_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain
      -> 'a8 stored_triple vector -> nat -> 'a8 chain -> 'a8 cadeque -> 'a8
      cadeque -> 'a8 concat_clause_1_clause_1_graph -> 'a3 **)

  let concat_clause_1_clause_1_graph_mut _ _ _ f2 _ _ _ _ _ _ _ _ _ _ _ _ = function
  | Coq_concat_clause_1_clause_1_graph_equation_1 (ck, c1, v, ck0, c2, c3) ->
    Obj.magic f2 __ ck c1 v __ ck0 c2 c3 __

  (** val concat_clause_1_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> nat -> 'a8 chain
      -> 'a8 left_right_triple -> nat -> 'a8 chain -> 'a8 cadeque -> 'a8
      concat_clause_1_graph -> 'a2 **)

  let concat_clause_1_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 ck c1 refine ck0 c2 s c =
    let rec _f8 _ _ _ _ = function (* # mod # -> added _ before f8 *)
    | Coq_concat_graph_refinement_1 (ck1, c3, ck2, c4, hind) ->
      Obj.magic f __ ck1 c3 ck2 c4 hind
        (f9 ck1 c3
          (make_left O ck1 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c3) ck2 c4
          (match make_left O ck1 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c3 with
           | Not_enough (_, _, v) ->
             vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S
               (S (S (S (S O)))))) v (T (ck2, c4))
           | Ok_lrt (_, _, _, t) ->
             (match make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange,
                      NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c4 with
              | Not_enough (_, _, v) ->
                vector_inject cadeque_seq (stored_triple_seq O) ground_inject
                  (T (ck1, c3)) (S (S (S (S (S (S O)))))) v
              | Ok_lrt (_, _, _, t0) ->
                T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
                  NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                  (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t),
                  (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t0)))))) hind)
    and f9 _ _ _ _ _ _ = function
    | Coq_concat_clause_1_graph_refinement_1 (ck1, c3, v, ck2, c4, hind) ->
      Obj.magic f0 __ ck1 c3 v __ ck2 c4 hind
        (f10 __ ck1 c3 v __ ck2 c4
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck2, c4)))
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck2, c4))) hind)
    | Coq_concat_clause_1_graph_refinement_2 (ck1, c3, tl, ck2, c4, hind) ->
      Obj.magic f1 __ ck1 c3 tl __ ck2 c4 hind
        (f11 __ ck1 c3 tl __ ck2 c4
          (make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c4)
          (match make_right O ck2 (Mix (SomeGreen, NoYellow, NoOrange,
                   NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c4 with
           | Not_enough (_, _, v) ->
             vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
               (ck1, c3)) (S (S (S (S (S (S O)))))) v
           | Ok_lrt (_, _, _, t) ->
             T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
               (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) tl),
               (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) t))))) hind)
    and f10 _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_1_graph_equation_1 (ck1, c3, v, ck2, c4, c5) ->
      Obj.magic f2 __ ck1 c3 v __ ck2 c4 c5 __
    and f11 _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_graph_refinement_1 (ck1, c3, tl, ck2, c4,
                                                       v, hind) ->
      Obj.magic f3 __ ck1 c3 tl __ ck2 c4 v __ hind
        (f12 __ ck1 c3 tl __ ck2 c4 v
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck1, c3)) (S (S (S (S (S (S O)))))) v) __
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck1, c3)) (S (S (S (S (S (S O)))))) v) hind)
    | Coq_concat_clause_1_clause_2_graph_refinement_2 (ck1, c3, tl, ck2, c4,
                                                       tr, hind) ->
      Obj.magic f4 __ ck1 c3 tl __ ck2 c4 tr __ hind
        (f13 __ ck1 c3 tl
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl) __ ck2 c4 tr __ (T ((S (S O)), (Pair (O, (Mix
          (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)),
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl),
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr))))) hind)
    and f12 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 (ck1, c3, tl,
                                                              ck2, c4, v, c5) ->
      Obj.magic f5 __ ck1 c3 tl __ ck2 c4 v c5 __ __
    and f13 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 (ck1, c3, tl,
                                                                refine0, ck2,
                                                                c4, tr, hind) ->
      Obj.magic f6 __ ck1 c3 tl refine0 __ ck2 c4 tr __ hind
        (f14 __ ck1 c3 tl refine0 __ ck2 c4 tr
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr) __ (T ((S (S O)), (Pair (O, (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), refine0,
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr))))) hind)
    and f14 _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 (
        ck1, c3, tl, cl, ck2, c4, tr, cr) ->
      Obj.magic f7 __ ck1 c3 tl cl __ __ ck2 c4 tr cr __ __
    in f9 ck c1 refine ck0 c2 s c

  (** val concat_graph_mut :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> 'a8 cadeque ->
      'a8 cadeque -> 'a8 cadeque -> 'a8 concat_graph -> 'a1 **)

  let concat_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 d1 d2 s c =
    let rec f8 _ _ _ = function
    | Coq_concat_graph_refinement_1 (ck, c1, ck0, c2, hind) ->
      Obj.magic f __ ck c1 ck0 c2 hind
        (f9 __ ck c1
          (make_left O ck (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c1) ck0 c2
          (match make_left O ck (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c1 with
           | Not_enough (_, _, v) ->
             vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S
               (S (S (S (S O)))))) v (T (ck0, c2))
           | Ok_lrt (_, _, _, t) ->
             (match make_right O ck0 (Mix (SomeGreen, NoYellow, NoOrange,
                      NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c2 with
              | Not_enough (_, _, v) ->
                vector_inject cadeque_seq (stored_triple_seq O) ground_inject
                  (T (ck, c1)) (S (S (S (S (S (S O)))))) v
              | Ok_lrt (_, _, _, t0) ->
                T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
                  NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                  (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t),
                  (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t0)))))) hind)
    and f9 _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_graph_refinement_1 (ck, c1, v, ck0, c2, hind) ->
      Obj.magic f0 __ ck c1 v __ ck0 c2 hind
        (f10 __ ck c1 v __ ck0 c2
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck0, c2)))
          (vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S (S
            (S (S (S O)))))) v (T (ck0, c2))) hind)
    | Coq_concat_clause_1_graph_refinement_2 (ck, c1, tl, ck0, c2, hind) ->
      Obj.magic f1 __ ck c1 tl __ ck0 c2 hind
        (f11 __ ck c1 tl __ ck0 c2
          (make_right O ck0 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)) c2)
          (match make_right O ck0 (Mix (SomeGreen, NoYellow, NoOrange,
                   NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c2 with
           | Not_enough (_, _, v) ->
             vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
               (ck, c1)) (S (S (S (S (S (S O)))))) v
           | Ok_lrt (_, _, _, t) ->
             T ((S (S O)), (Pair (O, (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
               (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) tl),
               (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                 NoOrange, NoRed)) t))))) hind)
    and f10 _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_1_graph_equation_1 (ck, c1, v, ck0, c2, c3) ->
      Obj.magic f2 __ ck c1 v __ ck0 c2 c3 __
    and f11 _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_graph_refinement_1 (ck, c1, tl, ck0, c2,
                                                       v, hind) ->
      Obj.magic f3 __ ck c1 tl __ ck0 c2 v __ hind
        (f12 __ ck c1 tl __ ck0 c2 v
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck, c1)) (S (S (S (S (S (S O)))))) v) __
          (vector_inject cadeque_seq (stored_triple_seq O) ground_inject (T
            (ck, c1)) (S (S (S (S (S (S O)))))) v) hind)
    | Coq_concat_clause_1_clause_2_graph_refinement_2 (ck, c1, tl, ck0, c2,
                                                       tr, hind) ->
      Obj.magic f4 __ ck c1 tl __ ck0 c2 tr __ hind
        (f13 __ ck c1 tl
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl) __ ck0 c2 tr __ (T ((S (S O)), (Pair (O, (Mix
          (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)),
          (chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tl),
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr))))) hind)
    and f12 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 (ck, c1, tl,
                                                              ck0, c2, v, c3) ->
      Obj.magic f5 __ ck c1 tl __ ck0 c2 v c3 __ __
    and f13 _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 (ck, c1, tl,
                                                                refine, ck0,
                                                                c2, tr, hind) ->
      Obj.magic f6 __ ck c1 tl refine __ ck0 c2 tr __ hind
        (f14 __ ck c1 tl refine __ ck0 c2 tr
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr) __ (T ((S (S O)), (Pair (O, (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), refine,
          (chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) tr))))) hind)
    and f14 _ _ _ _ _ _ _ _ _ _ _ _ = function
    | Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1 (
        ck, c1, tl, cl, ck0, c2, tr, cr) ->
      Obj.magic f7 __ ck c1 tl cl __ __ ck0 c2 tr cr __ __
    in f8 d1 d2 s c

  (** val concat_graph_rect :
      (__ -> nat -> __ chain -> nat -> __ chain -> __ concat_clause_1_graph
      -> 'a2 -> 'a1) -> (__ -> nat -> __ chain -> __ stored_triple vector ->
      __ -> nat -> __ chain -> __ concat_clause_1_clause_1_graph -> 'a3 ->
      'a2) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __ chain ->
      __ concat_clause_1_clause_2_graph -> 'a4 -> 'a2) -> (__ -> nat -> __
      chain -> __ stored_triple vector -> __ -> nat -> __ chain -> __ cadeque
      -> __ -> 'a3) -> (__ -> nat -> __ chain -> __ triple -> __ -> nat -> __
      chain -> __ stored_triple vector -> __ -> __
      concat_clause_1_clause_2_clause_1_graph -> 'a5 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_graph -> 'a6 -> 'a4) -> (__ -> nat ->
      __ chain -> __ triple -> __ -> nat -> __ chain -> __ stored_triple
      vector -> __ cadeque -> __ -> __ -> 'a5) -> (__ -> nat -> __ chain ->
      __ triple -> __ chain -> __ -> nat -> __ chain -> __ triple -> __ -> __
      concat_clause_1_clause_2_clause_2_clause_1_graph -> 'a7 -> 'a6) -> (__
      -> nat -> __ chain -> __ triple -> __ chain -> __ -> __ -> nat -> __
      chain -> __ triple -> __ chain -> __ -> __ -> 'a7) -> 'a8 cadeque ->
      'a8 cadeque -> 'a8 cadeque -> 'a8 concat_graph -> 'a1 **)

  let concat_graph_rect =
    concat_graph_mut

  (** val concat_graph_correct :
      'a1 cadeque -> 'a1 cadeque -> 'a1 concat_graph **)

  let concat_graph_correct d1 d2 =
    let T (ck, c) = d1 in
    let T (ck0, c0) = d2 in
    Coq_concat_graph_refinement_1 (ck, c, ck0, c0,
    (let refine =
       make_left O ck (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
         (SomeGreen, NoYellow, NoOrange, NoRed)) c
     in
     match refine with
     | Not_enough (_, _, v) ->
       Coq_concat_clause_1_graph_refinement_1 (ck, c, v, ck0, c0,
         (let refine0 =
            vector_push cadeque_seq (stored_triple_seq O) ground_push (S (S
              (S (S (S (S O)))))) v (T (ck0, c0))
          in
          Coq_concat_clause_1_clause_1_graph_equation_1 (ck, c, v, ck0, c0,
          refine0)))
     | Ok_lrt (_, _, _, t) ->
       Coq_concat_clause_1_graph_refinement_2 (ck, c, t, ck0, c0,
         (let refine0 =
            make_right O ck0 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
              (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c0
          in
          match refine0 with
          | Not_enough (_, _, v) ->
            Coq_concat_clause_1_clause_2_graph_refinement_1 (ck, c, t, ck0,
              c0, v,
              (let refine1 =
                 vector_inject cadeque_seq (stored_triple_seq O)
                   ground_inject (T (ck, c)) (S (S (S (S (S (S O)))))) v
               in
               Coq_concat_clause_1_clause_2_clause_1_graph_equation_1 (ck, c,
               t, ck0, c0, v, refine1)))
          | Ok_lrt (_, _, _, t0) ->
            Coq_concat_clause_1_clause_2_graph_refinement_2 (ck, c, t, ck0,
              c0, t0,
              (let refine1 =
                 chain_of_triple O Coq_left (Mix (SomeGreen, NoYellow,
                   NoOrange, NoRed)) t
               in
               Coq_concat_clause_1_clause_2_clause_2_graph_refinement_1 (ck,
               c, t, refine1, ck0, c0, t0,
               (let refine2 =
                  chain_of_triple O Coq_right (Mix (SomeGreen, NoYellow,
                    NoOrange, NoRed)) t0
                in
                Coq_concat_clause_1_clause_2_clause_2_clause_1_graph_equation_1
                (ck, c, t, refine1, ck0, c0, t0, refine2)))))))))

  (** val concat_elim :
      (__ -> nat -> __ chain -> __ stored_triple vector -> __ -> nat -> __
      chain -> __ cadeque -> __ -> __ -> __ -> 'a1) -> (__ -> nat -> __ chain
      -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector -> __
      cadeque -> __ -> __ -> __ -> __ -> __ -> 'a1) -> (__ -> nat -> __ chain
      -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain -> __ triple ->
      __ chain -> __ -> __ -> __ -> __ -> __ -> __ -> 'a1) -> 'a2 cadeque ->
      'a2 cadeque -> 'a1 **)

  let concat_elim f2 f5 f7 d1 d2 =
    concat_graph_mut (fun _ _ _ _ _ _ x -> x __) (fun _ _ _ _ _ _ _ _ x ->
      x __) (fun _ _ _ _ _ _ _ _ x -> x __) f2 (fun _ _ _ _ _ _ _ _ _ _ x ->
      x __) (fun _ _ _ _ _ _ _ _ _ _ x -> x __) f5
      (fun _ _ _ _ _ _ _ _ _ _ _ x -> x __) f7 d1 d2 (concat d1 d2)
      (concat_graph_correct d1 d2)

  (** val coq_FunctionalElimination_concat :
      (__ -> nat -> __ chain -> __ stored_triple vector -> __ -> nat -> __
      chain -> __ cadeque -> __ -> __ -> __ -> __) -> (__ -> nat -> __ chain
      -> __ triple -> __ -> nat -> __ chain -> __ stored_triple vector -> __
      cadeque -> __ -> __ -> __ -> __ -> __ -> __) -> (__ -> nat -> __ chain
      -> __ triple -> __ chain -> __ -> __ -> nat -> __ chain -> __ triple ->
      __ chain -> __ -> __ -> __ -> __ -> __ -> __ -> __) -> __ cadeque -> __
      cadeque -> __ **)

  let coq_FunctionalElimination_concat =
    concat_elim

  (** val coq_FunctionalInduction_concat :
      (__ -> __ cadeque -> __ cadeque -> __ cadeque) coq_FunctionalInduction **)

  let coq_FunctionalInduction_concat =
    Obj.magic (fun _ -> concat_graph_correct)
 end

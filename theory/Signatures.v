From Coq Require Import List.
Import ListNotations.

(* The following module types, or signatures, sum up the types and operations
   that must be offered by an implementation of catenable deques, as well as
   the correctness properties that these operations must satisfy.

   We propose two signatures. The signature [CADEQUE_INTRINSIC] is expressed
   in intrinsic style: the correctness statement of each operation is built
   into its type via a subset type, also known as a Σ type, or [sig] in Rocq.
   The signature [CADEQUE_EXTRINSIC] is in extrinsic style: each operation has
   a simple type, and each correctness statement forms a separate lemma. *)

Module Type CADEQUE_INTRINSIC.

  (* The type [cadeque A] describes a catenable deque, or cadeque,
     whose elements have type [A]. *)
  Parameter cadeque : Type -> Type.

  (* The model of a cadeque is the sequence of its elements.
     To represent mathematical sequences, we use Rocq's lists.
     Thus, if [d] is a cadeque then [model d] is a list.

     This operation is not intended to be used at runtime
     (although it could). It is needed in order to express
     the correctness statement of each operation. *)
  Parameter model :
    forall {A}, cadeque A -> list A.

  (* [empty] is the empty cadeque. *)
  Parameter empty :
    forall {A},
    { d : cadeque A |
      model d = []
    }.

  (* [push x d] inserts [x] at the front end of the cadeque [d]. *)
  Parameter push :
    forall {A} (x : A) (d : cadeque A),
    { d' : cadeque A |
      model d' = [x] ++ model d
    }.

  (* [inject x d] inserts [x] at the rear end of the cadeque [d]. *)
  Parameter inject :
    forall {A} (d : cadeque A) (x : A),
    { d' : cadeque A |
      model d' = model d ++ [x]
    }.

  (* [pop d] attempts to extract an element at the front end of the cadeque
     [d]. It returns either nothing or a pair of an element and a cadeque. *)
  Parameter pop :
    forall {A} (d : cadeque A),
    { o : option (A * cadeque A) |
      model d = match o with
                      | None => []
                      | Some (x, d') => [x] ++ model d'
                      end
    }.

  (* [eject d] attempts to extract an element at the rear end of the cadeque
     [d]. It returns either nothing or a pair of a cadeque and an element. *)
  Parameter eject :
    forall {A} (d : cadeque A),
    { o : option (cadeque A * A) |
      model d = match o with
                      | None => []
                      | Some (d', x) => model d' ++ [x]
                      end
    }.

  (* [concat] concatenates two cadeques. *)
  Parameter concat :
    forall {A : Type} (d1 d2 : cadeque A),
    { d : cadeque A |
      model d = model d1 ++ model d2
    }.

End CADEQUE_INTRINSIC.

Module Type CADEQUE_EXTRINSIC.

  Parameter cadeque : Type -> Type.

  Parameter model :
    forall {A}, cadeque A -> list A.

  Parameter empty :
    forall {A}, cadeque A.

  Parameter empty_correct :
    forall {A},
    model (empty : cadeque A) = [].

  Parameter push :
    forall {A}, A -> cadeque A -> cadeque A.

  Parameter push_correct :
    forall {A} (x : A) (d : cadeque A),
    model (push x d) = [x] ++ model d.

  Parameter inject :
    forall {A}, cadeque A -> A -> cadeque A.

  Parameter inject_correct :
    forall {A} (d : cadeque A) (x : A),
    model (inject d x) = model d ++ [x].

  Parameter pop :
    forall {A}, cadeque A -> option (A * cadeque A).

  Parameter pop_correct :
    forall {A} (d : cadeque A),
    model d = match pop d with
              | None => []
              | Some (x, d') => [x] ++ model d'
              end.

  Parameter eject :
    forall {A}, cadeque A -> option (cadeque A * A).

  Parameter eject_correct :
    forall {A} (d : cadeque A),
    model d = match eject d with
              | None => []
              | Some (d', x) => model d' ++ [x]
              end.

  Parameter concat :
    forall {A : Type}, cadeque A -> cadeque A -> cadeque A.

  Parameter concat_correct :
    forall {A : Type} (d1 d2 : cadeque A),
    model (concat d1 d2) = model d1 ++ model d2.

End CADEQUE_EXTRINSIC.

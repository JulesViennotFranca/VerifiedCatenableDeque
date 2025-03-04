(** This module is used to manage a database elements of type ['a]. *)
module Database : sig

  (** The database of elements of type ['a]. *)
  type 'a t

  (** Create a database containing no elements. *)
  val empty : 'a t

  (** Create a database containing a single element. *)
  val singleton : 'a -> 'a t

  (** Return the database. *)
  val database : 'a t -> 'a list

  (** Apply a unary operation to one element of the database choosen at random,
      the new element obtained is added to the database. *)
  val apply_unary : ('a -> 'a) -> 'a t -> 'a t

  (** Apply a binary operation to two elements of the database choosen at
      random, the new element obtained is added to the database. *)
  val apply_binary : ('a -> 'a -> 'a) -> 'a t -> 'a t

  (** Apply a function to all elements of the database, returning a new
      database. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Apply a unary function to all elements of the database. *)
  val uiter : ('a -> unit) -> 'a t -> unit

  (** Apply a binary function to all elements of the database. *)
  val biter : ('a -> 'a -> unit) -> 'a t -> unit

  (** Do a fold left on the database. *)
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** Merge two databeses into one. *)
  val merge : 'a t -> 'a t -> 'a t
end = struct
  (** [sum] is equal to sum_{k=1}^{n} 1 / (k ^ beta)
                where beta is parameter and n is the size of the database.
     [n] is the size of the database.
     [data] is the database. *)
  type 'a t = { sum : float ; n : int ; data : 'a list }

  (** [beta] is a parameter chosen by the user. *)
  let beta = 3.

  let empty = { sum = 0. ; n = 0 ; data = [] }

  let singleton a = { sum = 1. ; n = 1 ; data = [a] }

  let database { data ; _ } = data

  (** Add a new element to the database, update all field accordingly. *)
  let add { sum ; n ; data } a = {
    sum = sum +. (1. /. (float_of_int (n + 1) ** beta));
    n = n + 1;
    data = a :: data
  }

  (** Choose a random element in the database.
      We don't want to choose the element uniformly.
      Instead, we choose the element over the distribution:
        \[ 𝛼 / 1 ^ ß ; 𝛼 / 2 ^ ß ; ... ; 𝛼 / n ^ ß \]
      where, for a database [{ sum ; n ; data }],
        n = [n],
        𝛼 = [1. /. sum],
        ß = [beta]. *)
  let choose { sum ; data ; _ } =
    let rec aux i proba = function
      | [] -> assert false
      | [a] -> a
      | a :: data ->
        let p = Random.float 1. in
        if p < proba then
          a
        else
          let new_proba =
            ((float_of_int i /. float_of_int (i+1)) ** beta) *.
            (proba /. (1. -. proba))
          in
          aux (i+1) new_proba data
    in
    aux 1 (1. /. sum) data

  let apply_unary u t =
    let a = choose t in
    let new_a = u a in
    add t new_a

  let apply_binary b t =
    let a1 = choose t in
    let a2 = choose t in
    let new_a = b a1 a2 in
    add t new_a

  let map f { sum ; n ; data } = { sum ; n ; data = List.map f data }

  let uiter f { data ; _ } = List.iter f data

  let biter f { data ; _ } = List.iter2 f data data

  let fold_left f a { data ; _ } = List.fold_left f a data

  let merge
    { sum = sum1 ; n = n1 ; data = data1 }
    { n = n2 ; data = data2 ; _ } =
    let new_sum = ref sum1 in
    for k = n1 + 1 to n1 + n2 do
      new_sum := !new_sum +. (1. /. (float_of_int k ** beta))
    done;
    { sum = !new_sum ; n = n1 + n2 ; data = data1 @ data2 }
end

(** Trace of execution. *)
type trace =
  | Stop : trace
  | Unary : (string * trace) -> trace
  | Binary : (string * trace) -> trace

(** Parallel traces of execution. *)
type traces = trace array

(** Provided unary and binary operations, generate parallel traces of
    execution. *)
let gen_traces uoperations boperations length parallel =
  let nbr_op =
    let n = List.fold_left (fun n (_, p) -> n + p) 0 uoperations in
    List.fold_left (fun n (_, p) -> n + p) n boperations
  in
  let operations =
    let res = Array.make nbr_op (true, "") in
    let fill unary k (name, p) =
      for i = 0 to p - 1 do
        res.(k + i) <- (unary, name)
      done;
      k + p
    in
    let k = List.fold_left (fill true) 0 uoperations in
    ignore (List.fold_left (fill false) k boperations);
    res
  in
  let make_trace () =
    let construct trace (unary, name) =
      if unary then Unary (name, trace) else Binary (name, trace) in
    let indexes = List.init length (fun _ -> Random.int nbr_op) in
    let ops = List.map (Array.get operations) indexes in
    List.fold_left construct Stop ops
  in
  Array.init parallel (fun _ -> make_trace ())

(** The signature of modules used to measure datas at each unary and binary
    operations. *)
module type Measure = sig

  (** The type of data that is measured for each datastructure. *)
  type s

  (** The type of data that is measured for each unary operations. *)
  type ut

  (** The type of data that is measured for each binary operations. *)
  type bt

  val wrap_uop :
    string ->
    ('a -> 'a) ->
    ('a -> s) ->
    (s -> int) ->
    'a ->
    'a

  val wrap_bop :
    string ->
    ('a -> 'a -> 'a) ->
    ('a -> s) ->
    (s -> s -> int) ->
    'a ->
    'a ->
    'a
end

(** The signature of modules used to represent the datastructure. *)
module type Datastruct = sig

  (** The type of the datastructure. *)
  type t

  (** An empty datastructure. *)
  val empty : t

  (** The type of data that is measured from the datastructure. *)
  type measure

  (** Return datas measured from a datastructure. *)
  val measure : t -> measure

  (** The list of unary operations supported by the datastructure along with
      the number of times they should be repeated based on the datas measure
      from the datastructure. *)
  val unary_operations : (string * (t -> t) * (measure -> int) * int) list

  (** The list of binary operations supported by the datastructure along with
      the number of times they should be repeated based on the datas measure
      from the datastructure. *)
  val binary_operations :
    (string * (t -> t -> t) * (measure -> measure -> int) * int) list
end

type ('a, 's) operation =
  | Unary : string * ('a -> 'a) * ('s -> int) -> ('a, 's) operation
  | Binary : string * ('a -> 'a -> 'a) * ('s -> 's -> int) -> ('a, 's) operation

let make_operations uoperations boperations =
  let add_uop accu (name, f, steps, it) =
    List.init it (Fun.const (Unary (name, f, steps))) @ accu
  in
  let add_bop accu (name, f, steps, it) =
    List.init it (Fun.const (Binary (name, f, steps))) @ accu
  in
  let uops = List.fold_left add_uop [] uoperations in
  let ops = List.fold_left add_bop uops boperations in
  Array.of_list ops

let remove_operation op_name operations =
  let l = Array.to_list operations in
  let filter = function
    | Unary (name, _, _) -> name <> op_name
    | Binary (name, _, _) -> name <> op_name
  in
  let l = List.filter filter l in
  Array.of_list l

(** Provided a measure module, a datastructure module, and parrallel traces,
    return an database containing all the different instances of datastructure
    created when executing the parallel traces. *)
let run
: type s t.
     (module Measure with type s = s)
  -> (module Datastruct with type measure = s and type t = t)
  -> int
  -> t Database.t
= fun (module M) (module DS) parallel ->

  let operations = make_operations DS.unary_operations DS.binary_operations in

  let get_rand a = a.(Random.int (Array.length a)) in

  let make_one_run _i =
    let db = Database.singleton DS.empty in
    let rec aux ops db =
      try match get_rand ops with
      | Unary (n, f, s)  ->
        begin try
          let wraped_uop = M.wrap_uop n f DS.measure s in
          let db = Database.apply_unary wraped_uop db in
          aux ops db
        with _ -> db end
      | Binary (n, f, s) ->
        begin try
          let wraped_bop = M.wrap_bop n f DS.measure s in
          let db = Database.apply_binary wraped_bop db in
          aux ops db
        with _ -> aux (remove_operation n ops) db end
      with Invalid_argument _ -> db
    in
    aux operations db
  in

  let databases = Array.init parallel make_one_run in
  Array.fold_left Database.merge Database.empty databases

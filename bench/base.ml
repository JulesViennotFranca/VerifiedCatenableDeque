(** This module is used to manage a collection of elements of type ['a]. *)
module DataBase : sig

  (** The database of elements of type ['a]. *)
  type 'a t

  (** Construct an empty database.*)
  val empty : 'a t

  (** Apply a function to all elements of the database, returning a new
      database. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Apply a unary function to all elements of the database. *)
  val uiter : ('a -> unit) -> 'a t -> unit

  (** Apply a binary function to all elements of the database. As [biter] can
      take a lot of time, a proportion is passed as argument. The computation
      stops when the ratio of pairs already seen over the total possible pairs
      exceed the proportion. *)
  val biter : ('a * 'a -> unit) -> float -> 'a t -> unit

  (** Return the number of elements in the database. *)
  val length : 'a t -> int

  (** Do a fold left on the database. *)
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** Merge two databeses into one. *)
  val merge : 'a t -> 'a t -> 'a t

  (** Create a database out of a list. *)
  val of_list : 'a list -> 'a t

end = struct

  type 'a t = 'a list * int

  let empty = ([], 0)
  let map f (l, n) = (List.map f l, n)
  let uiter f (l, _) = List.iter f l

  let shuffle l =
    let a = Array.of_list l in
    let n = Array.length a in
    for i = n - 1 downto 1 do
      let k = Random.int (i+1) in
      let x = a.(k) in
      a.(k) <- a.(i);
      a.(i) <- x
    done;
    Array.to_list a

  let prefix l n =
    let rec aux accu n l = match l, n with
      | [], _ -> List.rev accu
      | _, 0 -> List.rev accu
      | x :: l, n -> aux (x :: accu) (n-1) l
    in
    aux [] n l

  let biter f prop (l, n) =
    let sprop = sqrt prop in
    let n = int_of_float (sprop *. float_of_int n) in
    let l1 = prefix (shuffle l) n in
    let l2 = prefix (shuffle l) n in
    List.iter (fun d1 -> List.iter (fun d2 -> f (d1, d2)) l1) l2

  let length = snd
  let fold_left f a (l, _) = List.fold_left f a l
  let merge (d1, n1) (d2, n2) = (List.rev_append d1 d2, n1 + n2)

  let of_list l = (l, List.length l)

end


(** This module is used to construct a database of elements of type ['a]. *)
module DataConstr = struct

  (** [sum] is equal to sum_{k=1}^{n} 1 / (k ^ beta)
                where beta is parameter and n is the size of the database.
     [n] is the size of the database.
     [data] is the database. *)
  type 'a t = { sum : float ; n : int ; data : 'a list }

  (** [beta] is a parameter chosen by the user. *)
  let beta = 3.

  (** Create a database containing a single element. *)
  let singleton a = { sum = 1. ; n = 1 ; data = [a] }

  (** Return the constructed database. *)
  let to_database { data ; _ } = DataBase.of_list data

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

  (** The maximum size allowed for each data structure. *)
  let max_size = 2000

  exception TooBig

  (** Apply an operation to one element of the database choosen at random, the
      new element obtained is added to the database. *)
  let apply length f t =
    let a = choose t in
    if length a > max_size then raise TooBig;
    let new_a = f a in
    add t new_a

end

(** The signature of modules used to represent the datastructure. *)
module type DataStruct = sig

  (** The type of the datastructure. *)
  type t

  (** An empty datastructure. *)
  val empty : t

  (** Return the length of the datastructure. *)
  val length : t -> int

  (** A list of operations used to build a database of elements for this data
      structure. Each new element is created by randomly selecting an
      operation. To allow users to specify a distribution, each operation is
      assigned an integer representing its occurrence count. Operations are
      then sampled proportionally to their assigned counts. *)
  val constr_operations : ((t -> t) * int) list

end

(** Given a list of operations along with their occurence count, return a
    function sampling an operation proportionally to the operations occurence
    counts.*)
let random_operation operations =
  let operations =
    List.map (fun (op, oc) -> List.init oc (fun _ -> op)) operations |>
    List.concat |>
    Array.of_list
  in
  let n = Array.length operations in
  fun () -> operations.(Random.int n)

(** Provided a datastructure, return an database containing all the different
    instances of the datastructure created by drawing construction operations
    at random. *)
let run
: type t.
     (module DataStruct with type t = t)
  -> parallel:int
  -> t DataBase.t
= fun (module D) ~parallel ->

  let get_op = random_operation D.constr_operations in

  let make_one_run _i =
    let db = DataConstr.singleton D.empty in
    let rec aux db =
      let op = get_op () in
      try
        aux (DataConstr.apply D.length op db)
      with DataConstr.TooBig -> db
    in
    aux db
  in

  let datas = Array.init parallel make_one_run in
  let datas = Array.map DataConstr.to_database datas in
  Array.fold_left DataBase.merge DataBase.empty datas

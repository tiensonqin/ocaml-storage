module Vector:
sig
  include module type of Containers.Vector
  val safe_get : ('a, 'b) t -> int -> 'a -> 'a
  val safe_set : ('a, rw) t -> int -> 'a -> unit
  val empty : ('a, 'b) t -> bool
end

module type Comparable =
sig type t val compare : t -> t -> int val show : t -> unit end

module Make_skiplist :
  functor (Endpoint : Comparable) ->
  sig
    type t
    val create : int -> t
    val height : t -> int
    val insert : t -> Endpoint.t -> unit
    val delete : t -> Endpoint.t -> unit
    val fold_left : t -> ('a -> Endpoint.t -> 'a) -> 'a -> 'a
    val to_list : t -> Endpoint.t list
    val from_list : ?max_height:int -> Endpoint.t list -> t
    val length : t -> int
    val contains: t -> Endpoint.t -> bool
    val print : t -> unit
  end

module Int_skiplist :
sig
  type t
  val create : int -> t
  val height : t -> int
  val insert : t -> Int.t -> unit
  val delete : t -> Int.t -> unit
  val fold_left : t -> ('a -> Int.t -> 'a) -> 'a -> 'a
  val to_list : t -> Int.t list
  val from_list : ?max_height:int -> Int.t list -> t
  val length : t -> int
  val contains: t -> Int.t -> bool
  val print : t -> unit
end

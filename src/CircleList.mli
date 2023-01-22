(** The CircleList file includes the CircList module which is used in game.ml
    for the list of players and other round functionalities. *)

module CircleList : sig
  (** CircleList is like a regular list except it "wraps around", meaning 
      that the last element of the list points back to the first element of the
      list *)

  exception Empty
  (** Raised when a function that shouldn't be run on an empty CircleList is 
      ran on an empty CircleList  *)

  val empty : 'a list
  (** [empty] returns the empty CirceList *)

  val is_empty : 'a list -> bool
  (** [is_empty lst] checks if the CircleList is empty. If it is empty, true is
      returned. If not, false is returned*)

  val add : 'a -> 'a list -> 'a list
  (** [add person lst] returns a list with person added to the front of lst*)

  val next : 'a list -> 'a list
  (** [next lst] returns a new list with the first element moved to the end of
     the list*)

  val pop : 'a list -> 'a list
  (** [pop lst] removes the first element of the CircleList and returns the new 
      list*)

  val length : 'a list -> int
  (** [length lst] returns the number of elements in lst*)

  val nth : 'a list -> int -> 'a
  (** [nth lst n] returns the nth element of the CircleList lst *)

  val peek : 'a list -> 'a
  (** [peek lst] returns the first element of the CirceList lst  *)

  val last : 'a list -> 'a
  (** [last lst] returns the last element of the CircleList lst  *)

  val mem : 'a list -> 'a -> bool
  (** [mem lst e] returns true if e is in the lst and false if it isnt'  *)
end
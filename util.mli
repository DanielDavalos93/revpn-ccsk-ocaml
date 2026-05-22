module Util : sig

  val ( -- ) : int -> int -> int list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val unzip : ('a * 'b) list -> 'a list * 'b list
  val append_disj : ('a -> bool) -> 'a list -> 'a list * 'a list
  val ( !! ) : 'a list -> int -> 'a
  val init : 'a list -> 'a list
  val last : 'a list -> 'a
  val set_of_list : 'a list -> 'a list
  val bin_prod : 'a list -> 'b list -> ('a * 'b) list
  val max_list : 'a list -> 'a
  val min_list : 'a list -> 'a
  val suprime : 'a -> 'a list -> 'a list
  val sort_increasing : 'a list -> 'a list
  val sort_increasing_pair_left : (string * 'a) list -> (string * 'a) list
  val setminus : 'a list -> 'a list -> 'a list

end

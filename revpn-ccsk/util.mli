val ( -- ) : int -> int -> int list
val ( |>> ) : 'a option -> ('a -> 'b option) -> 'b option
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val append_disj : ('a -> bool) -> 'a list -> 'a list * 'a list
val ( !! ) : 'a list -> int -> 'a
val init : 'a list -> 'a list
val last : 'a list -> 'a
val insert : 'a -> 'a list -> 'a list list
val perm : 'a list -> 'a list list
val all_comb : 'a list -> 'a list list
val set_of_list : 'a list -> 'a list
val bin_prod : 'a list -> 'b list -> ('a * 'b) list
val max_list : string list -> string
val min_list : string list -> string
val suprime : 'a -> 'a list -> 'a list
val sort_increasing : string list -> string list
val sort_increasing_pair_left : (string * 'a) list -> (string * 'a) list
val remove_one_token : 'a list -> 'a -> 'a list
val setminus : 'a list -> 'a list -> 'a list
val intersect : 'a list -> 'a list -> 'a list
val is_subset : 'a list -> 'a list -> bool
val exists_unique : 'a list -> ('a -> bool) -> 'a -> bool
val encode_string : string -> int

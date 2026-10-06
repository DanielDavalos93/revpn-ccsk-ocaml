val ( -- ) : int -> int -> int list
    (** range from i to j: [i--j] generes the list [[i;i+1;..;j]] *)

val ( |>> ) : 'a option -> ('a -> 'b option) -> 'b option
val zip : 'a list -> 'b list -> ('a * 'b) list
    (** [zip [l1,l2,..] [t1,t2,..]] returns the list of pairs [[(l1,t1), (l2,t2), ..]]. *)

val unzip : ('a * 'b) list -> 'a list * 'b list
val append_disj : ('a -> bool) -> 'a list -> 'a list * 'a list

val ( !! ) : 'a list -> int -> 'a
(** Get the n-th element of a list. For a list [ls] and a positive integer
    number [i], [ls !! i] return the element of the list [ls] in the 
    i-th position. *)

    val init : 'a list -> 'a list
val last : 'a list -> 'a
val insert : 'a -> 'a list -> 'a list list
val perm : 'a list -> 'a list list
val all_comb : 'a list -> 'a list list
val set_of_list : 'a list -> 'a list
val bin_prod : 'a list -> 'b list -> ('a * 'b) list
    (** Binary product. Given two sets [A] and [B], [bin_prod A B] return the
 list of pairs [(a, b)] where {m a \in A \wedge b \in B}. *)

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

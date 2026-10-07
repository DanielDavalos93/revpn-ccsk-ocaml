type label = string
type state = { n_states : int; init : int; }
  (** {b State.} For practicy we use [[n] = 0..n-1] for states instead other type like [string]. 
    The current state [init] should be less or equal than [n_states].
  *)

type trans = (int * label * int) list
  (** {b Transition.} A pair [(n1, a, n2)] is in a transition if there is an
    arc such that [n1 --[a]--> n2].
  *)

type lts = { states : state; trans : trans; }
module LTS :
  sig
    val make : state -> trans -> lts
    val succ : lts -> int -> label -> int list
    val pred : lts -> int -> label -> int list
    val all_labels : lts -> label list
    val postset : lts -> int list -> label -> int list
  end

module PairSet :
  sig
    type elt = int * int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
val refine_step : lts -> label list -> PairSet.t -> PairSet.t

val bisim_naive : lts -> PairSet.t
  (** 
    {b NAIVE ALGORITHM.} Let be {m R_0 = S \times S} for the
    base case and for the inductive step:

    {math R_{k+1} = \{ (s,t) \in R_k \mid
        \forall a, \forall s', s\overrightarrow{a}s' \Rightarrow 
        \exists t', t \underrightarrow{a} t' \wedge (s',t') \in R_k
        \wedge
        \forall a, \forall t', t \underrightarrow{a} t' \Rightarrow
        \exists s', s \underrightarrow{a} s' \wedge (s',t') \in R_k \}
        }
        
        Stops when {m R_{k+1} = R_k} for any {m k\geq k_0} (find a fix point).

   *)

val pre_a : lts -> int array -> int -> label -> int list
(** [pre_a(B)]: states with some succ. [a] in the block [B]. *)

val split_block : int array -> int -> int -> int list -> int ref -> bool
  (** Splits the block `c_id` if their states are in `splitter_set` *)

type partition_result = {
  block_of : int array;
  n_blocks : int;
  blocks : int list array;
}

val bisim_partition : lts -> partition_result
val tau_closure : lts -> int -> int list
val weak_LTS_succ : lts -> int -> label -> int list
val bisim_weak : lts -> PairSet.t

(** 
   WEAK BISIMULATION

  [s ≈ t] if there is [R] shuch that every [(s,t) ∈ R] satisfies:
       - if {m s \overset{a}{\longrightarrow} s'}  [(a ≠ τ)] then {m \exists t', t \overset{a}{\Longrightarrow} t'} and [(s',t') ∈ R]
             where {m \overset{a}{\Longrightarrow}} means  [τ* · a · τ*]
       - if {m s \overset{\tau}{\longrightarrow} s'} then [\exists t', t \overset{\varepsilon}{\Longrightarrow} t'] and [(s',t') ∈ R]
             where {m \overset{\varepsilon}{\Longrightarrow}} means  [τ*]
       (and symmetrically for [t])
 
    Implementation for closure of [τ] by [BFS/DFS], then fix point.

   *)

val minimize_lts : lts -> partition_result -> lts
(**
   {b Minimization.} Given a LTS and a partition, returns the minimal LTS 
  *)
  
val print_partition : partition_result -> unit
val print_lts_explicit : string -> lts -> unit
val print_bisim_relation : PairSet.t -> int -> unit
val check_bisim_pair : lts -> PairSet.t -> int -> int -> unit
val combine : lts -> lts -> lts
val are_bisimilar_strong : lts -> lts -> bool
val are_bisimilar_weak : lts -> lts -> bool
val string_of_pair : int * int -> string
val string_of_pairset : PairSet.t -> string
val string_of_transition_list : (int * string * int) list -> string
val print_bisimilar_strong : lts -> lts -> unit
val print_bisimilar_weak : lts -> lts -> unit

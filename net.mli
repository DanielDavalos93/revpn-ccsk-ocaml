(** Petri nets definitions *)

module Net : sig
(**
The formal structure of a {b Petri net} {m N = (S, T, F, \lambda, A)}, where:

{ul
 {li {m S} is the set of {i places},}
 {li {m T} is the set of {i transitions},}
 {li {m F \subseteq (S \times T) \cup (T \times S)} is a set of {i acs}, and}
 {li {m \lambda : T \to A} is a {i labelling map} for a set {m A}.}
}
*)

  type place_id = string

  type transition_id = string

  type place = {p_id : place_id;}

  type transition = {t_id : transition_id; t_label : transition_id;}

  type arc = PT of (place_id *transition_id * int) | TP of (transition_id * place_id * int)

  type marking = (place_id * int) list

  type labelled_net = {
    places : place list;
    transitions : transition list;
    arcs : arc list;
    set : transition_id list;
    label : transition list;
  }

  val make_label_net :
    place list ->
    transition list ->
    arc list ->
    transition_id list ->
    ((transition -> transition) -> transition list) ->
    labelled_net 

end

type place_id = string
type transition_id = place_id
type place = { p_id : transition_id; }
type transition = { t_id : transition_id; t_label : transition_id; }
type arc =
    PT of (transition_id * transition_id)
  | TP of (transition_id * transition_id)
type marking = transition_id list
type labelled_net = {
  places : place list;
  transitions : transition list;
  arcs : arc list;
  set : transition_id list;
  label_map : transition -> transition;
}
val make_label_net :
  place list ->
  transition list ->
  arc list -> string list -> (transition -> transition) -> labelled_net
val make_place : place_id -> place
val generate_place : int -> place list
val make_transition : transition_id -> transition
val generate_transition : int -> transition list
val make_label : ('a -> 'b) -> 'a list -> 'b list
type int_projection = Fst | Snd
val pi : arc -> int_projection -> transition_id
type marked_net = { net : labelled_net; marking : marking; }
val empty_transition : transition
val empty_marked_net : marked_net
val make_marked_net : labelled_net -> marking -> marked_net
val get_transition : labelled_net -> transition_id list
val get_place : labelled_net -> marking
val tokens : marked_net -> place_id -> int
val input_arcs : labelled_net -> transition_id -> marking
val output_arcs : labelled_net -> transition_id -> marking
val preset_of_transition : labelled_net -> transition_id -> place list
val postset_of_transition : labelled_net -> transition_id -> place list
val preset_of_place : labelled_net -> place_id -> transition list
val postset_of_place : labelled_net -> place_id -> transition list
val is_enabled : marked_net -> transition_id -> bool
val fire : marked_net -> transition_id -> marked_net option
val un_opt : marked_net option -> marked_net
val firing_sequence : marked_net -> transition_id list -> marked_net option
val enabled_transitions : marked_net -> transition list
val pair_fir :
  marked_net -> transition_id -> marking * transition_id * marking
val marking_key : marking -> place_id
val normalize_marking : marking -> string list
val marking_graph : marked_net -> (marking * transition_id * marking) list
val reachable_markings : marked_net -> marking list
val ccs_net : labelled_net -> bool
val is_safe : marked_net -> bool
val print_preset_postset : labelled_net -> unit
val print_marking : marked_net -> unit
val print_enabled : marked_net -> unit
val pl : place list
val tr : transition list
val arcs : arc list
val set : string list
val lambda : transition -> transition
val label_trans : transition -> transition
val net1 : labelled_net
val init_marking : string list
val mnet1 : marked_net
val pl2 : place list
val tr2 : transition list
val arcs2 : arc list
val init2 : string list
val set2 : string list
val lambda2 : transition -> transition
val label_trans2 : transition -> transition
val net2 : labelled_net
val init2 : string list
val mnet2 : marked_net

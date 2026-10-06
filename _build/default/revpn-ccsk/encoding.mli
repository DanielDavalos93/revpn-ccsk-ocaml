val var_of_place : string -> string
val var_of_trans : string -> string
val action_of_sync : string -> string
val label_of_transition : Net.labelled_net -> string -> string
val act_of_label : string -> Ccsk.act
val is_sync : Net.labelled_net -> string -> bool
val choice_of_list : Ccsk.CCS.process list -> Ccsk.CCS.process
val parallel_of_list : Ccsk.CCS.process list -> Ccsk.CCS.process
val init_place_equations : Net.labelled_net -> Ccsk.CCS.equations
val subst_in_equations :
  string -> Ccsk.CCS.process -> Ccsk.CCS.equations -> Ccsk.CCS.equations
val encode_simple_transitions :
  Net.labelled_net -> Ccsk.CCS.equations -> Ccsk.CCS.equations
val subst_var_in_one_equation :
  string ->
  Ccsk.CCS.process -> string -> Ccsk.CCS.equations -> Ccsk.CCS.equations
val encode_sync_transitions :
  Net.labelled_net -> Ccsk.CCS.equations -> Ccsk.CCS.equations * string list
val replicate : int -> Ccsk.CCS.process -> Ccsk.CCS.process
val assemble_marking : Net.labelled_net -> Net.marked_net -> Ccsk.CCS.process
val restrict_all : string list -> Ccsk.CCS.process -> Ccsk.CCS.process
val lts_of_ccs :
  Net.marked_net -> Ccsk.CCS.process -> Ccsk.CCS.equations -> Lts.lts
val lts_of_marked_net : Net.marked_net -> Lts.lts
type encoding = {
  process : Ccsk.CCS.process;
  equations : Ccsk.CCS.equations;
}
val process_of_marked_net : Net.marked_net -> encoding
val encode : Net.marked_net -> Lts.lts
val string_of_equations : Ccsk.CCS.equations -> string
val print_result : encoding -> unit

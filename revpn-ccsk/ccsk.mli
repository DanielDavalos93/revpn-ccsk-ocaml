type act = Input of string | Output of string | Silent
val co_action : act -> act
  (** We can tested that: [co_action(co_action a) = a], for [a] in [act_t]*)

val notation_act : act -> string
type relabel = string -> string
module CCS :
  sig
    type process =
        Zero
      | Prefix of act * process
      | Choice of process * process
      | Parallel of process * process
      | Restriction of process * string list
      | Var of string
      | Relabel of process * relabel
      | Rec of string * process
      (** Syntax of CCS: process and agents *)

    type equations = (string * process) list
      (** D = [Xi = Qi] *)

    val opt_equations : equations -> string -> process
    val subst : string -> process -> process -> process
      (** Substitution *)

    val relabel_act : relabel -> act -> act
      (** Structural semantics: Return the list [ls : (action * process) list] which a process
        can execute in a step.*)

    val transitions : equations -> process -> (act * process) list
    val string_of_process : process -> string
  end

  module CCSK :
  sig
    (** Syntax of CCSK: CCS and Keys *)
    type process =
        Zero
      | Prefix_i of act * int * process
      | Choice of process * process
      | Parallel of process * process
      | Restriction of process * string list
      | Var of string
      | Relabel of process * relabel
      
    type equations = (string * process) list
    val opt_equations : equations -> string -> process
    val subst : string -> process -> process -> process
    val relabel_act : relabel -> act -> act
    val transitions : equations -> process -> (act * process) list
    val key : process -> int list
      (** CCS with Communication Keys *)
      
    val std : process -> bool
  end

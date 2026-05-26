open Ccsk
(* Labelled Transition Systems (LTS) *)

(* Build LTS from CCS types (?) *)
type lts_state  = process
type lts_edge   = { inp_state : int; label : act; out_state : int }
type lts = {
  states : (int * process) list;
  edges  : lts_edge list;
}

(** CCS Semantics *)
(* let lts_from_ccs (q:process) (d:equations) : lts = *)


(** CCSK Semantics *)

(* let lts_from_ccsk (q : equations) (p : process) : lts = *)
(*   {} *)

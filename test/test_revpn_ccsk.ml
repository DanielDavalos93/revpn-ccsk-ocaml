(* Net *)
open Revpn_ccsk.Net
(** Define a net *)

let pl = generate_place 4

let tr = generate_transition 4

let arcs = [
  PT ("s1", "t1", 1); PT ("s1", "t2", 1);
  TP ("t1", "s2", 1); TP ("t2", "s3", 1);
  PT ("s3", "t4", 1); PT ("s2", "t4", 1); PT ("s2", "t3", 1);
  TP ("t3", "s4", 1); TP ("t4", "s4", 1)
]

let set : transition_id list = ["a"; "b"; "tau"]

let lambda t  =
  match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | "t3" -> {t_id = t.t_id; t_label = "b"}
  | "t4" -> {t_id = t.t_id; t_label = "tau"}
  | _ -> {t_id = t.t_id; t_label = t.t_label}

let label_trans = fun x -> lambda x

let net1 = make_label_net pl tr arcs set label_trans

let init_marking = [("s1", 1)]

let mnet1 = make_marked_net net1 init_marking

(** Verifies that [t1] is enabled at [s1] *)
let en_t1 = is_enabled mnet1 "t1" (* return true *)

(* Firing *)
let fs = firing_sequence mnet1 ["t1";"t2"]

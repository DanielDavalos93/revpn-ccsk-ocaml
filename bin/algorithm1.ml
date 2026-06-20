(* 
 This file contain the implementation of the Algorithm 1 of the paper 
 "Encoding Reversible Petri nets into CCSK".
 We use the net of the Fig. 3.
 *)

open Revpn_ccsk.Net
open Revpn_ccsk.Ccsk.CCS
module Encoding = Revpn_ccsk.Encoding


(* ------------------------------------------------------------------
  Building the net

  Net Figure 3

 •t1 = {s1},        t1• = {s1, s2},   lambda(t1) = a
 •t2 = {s1},        t2• = {s3},       lambda(t2) = b
 •t3 = {s2},        t3• = {s4},       lambda(t3) = b
 •t4 = {s2, s3},    t4• = {s1, s4},   lambda(t4) = tau

 ------------------------------------------------------------------ *)

let place4 = generate_place 4

let transition4 = generate_transition 4

let arcs4 =
  [
    PT ("s1", "t1", 1); TP ("t1", "s2", 1); TP ("t1", "s1", 1); (* t1 *)
    PT ("s1", "t2", 1); TP ("t2", "s3", 1);                     (* t2 *)
    PT ("s3", "t4", 1); PT ("s2", "t4", 1); TP ("t4", "s1", 1); TP ("t4", "s4", 1); (* t4 *)
    PT ("s2", "t3", 1); TP ("t3", "s4", 1);                     (* t3 *)
  ]

let set3 : transition_id list = ["a"; "b"; "tau"]

let lambda4 (t : transition) : transition = match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | "t3" -> {t_id = t.t_id; t_label = "b"}
  | "t4" -> {t_id = t.t_id; t_label = "tau"}
  | _ -> {t_id = t.t_id; t_label = t.t_label}

let label_trans4 = List.map lambda4 transition4

let net4 = make_label_net place4 transition4 arcs4 set3 label_trans4

let marking0 : marking = [("s1",1)]

let marked_net_fig3 : make_marked_net net4 marking0

let label_of_trans4 net tid = 
  match List.find_opt (fun t -> t.t_id = tid) net.label with
  | Some t -> t.t_label
  | None -> "?"

let () =
  List.iter (fun t ->
    let p_size = List.length (preset_of_transition net4 t.t_id) in
    assert (p_size <= 2);
    if p_size = 2 then assert (label_of_trans4 net4 t.t_id = "tau")
  ) transition4

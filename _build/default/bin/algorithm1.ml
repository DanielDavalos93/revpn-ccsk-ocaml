(* 
 This file contain the implementation of the Algorithm 1 of the paper 
 "Encoding Reversible Petri nets into CCSK".
 We use the net of the Fig. 3.
 *)

open Revpn_ccsk.Net
open Revpn_ccsk.Ccsk
open Revpn_ccsk.Encoding
open Revpn_ccsk.Lts

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
    PT ("s1", "t1"); TP ("t1", "s2"); (*TP ("t1", "s1"); t1 *)
    PT ("s1", "t2"); TP ("t2", "s3");                     (* t2 *)
    PT ("s3", "t4"); PT ("s2", "t4"); TP ("t4", "s1"); TP ("t4", "s4"); (* t4 *)
    PT ("s2", "t3"); TP ("t3", "s4");                     (* t3 *)
  ]

let set3 : transition_id list = ["a"; "b"; "tau"]

let lambda4 (t : transition) : transition = match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | "t3" -> {t_id = t.t_id; t_label = "b"}
  | "t4" -> {t_id = t.t_id; t_label = "tau"}
  | _ -> {t_id = t.t_id; t_label = t.t_label}

let label_trans4 = List.map lambda4 transition4

let net4 = make_label_net place4 transition4 arcs4 set3 lambda4

let marking0 : marking = ["s1"]

let marked_net1 = make_marked_net net4 marking0

let encode1 = encode marked_net1

let marking_graph1 = marking_graph marked_net1

let lts_mnet1 = lts_of_marked_net marked_net1

let str_bisim = are_bisimilar_strong encode1 lts_mnet1

let () = 
  print_endline "Algorithm 1 on the synchronising CCS net";
  print_newline ()



let () =
  if ccs_net marked_net1.net 
    then (
      print_result (process_of_marked_net marked_net1);
      print_newline ();
      print_endline "The net is a CCS net";
      print_lts_explicit "LTS(Q,D)" encode1;
      print_newline ();
      print_lts_explicit "M(N,m)" lts_mnet1;
      print_newline ();
      print_bisimilar_strong encode1 lts_mnet1;
      print_bisimilar_weak encode1 lts_mnet1;
    )
    else (
      print_endline "Is not a CCS net --> Algorithm stopped";
      print_newline ()
    )



let () = print_endline "All Algorithm 1 checks passed"

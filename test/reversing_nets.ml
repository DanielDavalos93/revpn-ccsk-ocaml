open Revpn_ccsk.Enriching
open Revpn_ccsk.Net

let key_ex  = List.map (fun i -> 
    "st" ^ string_of_int i |> make_place) (1--4)

let pl_ex : place list = (generate_place 4) @ key_ex

let arc_rev = [
  PT ("s1", "t1");
  TP ("t1", "st1"); PT ("st1", "u1"); TP ("u1", "s1");
  PT ("s1", "t2"); TP ("t2", "st2"); PT ("st2", "u2");
  TP ("u2", "s1");
  TP ("t1", "s2"); TP ("t2", "s3");
  PT ("s3", "t4"); PT ("s3", "u2");
  PT ("s2", "t4"); PT ("s2", "u1");
  PT ("s2", "t3"); TP ("t4", "st4"); PT ("st4","u4");
  TP ("t4", "s4"); TP ("t3", "st3"); PT ("st3", "u3");
  TP ("t3", "s4"); TP ("u3", "s2");
  PT ("s4", "u4"); PT ("s4", "u3");
  TP ("u4", "s3"); TP ("u4", "s2");
]

let rev_tr : transition list = [
  {t_id = "u1"; t_label = "a"}; {t_id = "u2"; t_label = "b"};
  {t_id = "u3"; t_label = "b"}; {t_id = "u4"; t_label = "tau"}
]

let rlambda = fun t ->
  match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | "t3" -> {t_id = t.t_id; t_label = "b"}
  | "t4" -> {t_id = t.t_id; t_label = "tau"}
  | _ -> t 


let net_ex = make_label_net pl_ex (tr @ rev_tr) arc_rev set rlambda

let knet_ex : keyPairNet = 
  {net = net_ex; 
  key = List.map (fun x -> x.p_id) key3}


let rnet : reversing_net = {
  net = net_ex;
  key = List.map (fun x -> x.p_id) key3;
  rev_trans = rev_tr;
  }

let is_rev = is_reversible_lab_net rnet

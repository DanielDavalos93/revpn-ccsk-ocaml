open Revpn_ccsk.Enriching
open Revpn_ccsk.Net

let key_ex : place list = List.map (fun i -> 
    "st" ^ string_of_int i |> make_place) (1--4)

let pl_ex : place list = (generate_place 4) @ key_ex

let arcs_ex = [
  PT ("s1", "t1"); TP ("t1", "s1");
  TP ("t1", "st1"); 
  PT ("s1", "t2"); TP ("t2", "st2");
  TP ("t1", "s2"); TP ("t2", "s3");
  PT ("s3", "t4"); PT ("s2", "t4");
  PT ("s2", "t3"); TP ("t4", "st4");
  TP ("t4", "s1"); TP ("t3", "st3");
  TP ("t3", "s4"); TP ("t4", "s4")
]

let net_ex = make_label_net pl_ex tr arcs_ex set label_trans

let knet_ex : keyPairNet = 
  {net = net3; 
  key = List.map (fun x -> x.p_id) key3}

let rev_trans_ex : transition list = [
  {t_id = "t1"; t_label = "a"}; {t_id = "t2"; t_label = "b"};
  {t_id = "t3"; t_label = "b"}; {t_id = "t4"; t_label = "tau"}
]

let reversing_net_ex : reversing_net = {
  net = net_ex;
  key = knet_ex;
  rev_trans = rev_trans_ex;
  }

let () = is_reversible_lab_net reversing_net_ex

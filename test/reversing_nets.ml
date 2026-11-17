(* open Revpn_ccsk.Enriching *)
(* open Revpn_ccsk.Net *)
(* open Revpn_ccsk.Token *)

(* Token and system net *)

let m0 : markingTok = make_marking [(s1, [w0; w0])]

let m0' : markingTok = make_marking [(s1, [w0])]
let m1' : markingTok = make_marking [(s2, [Token.Tok (net1.label_map t1, [w0], 1)]); (s3, [Token.Tok (net1.label_map t2, [w0], 1)])]

(* Fig. 4 *)
let ns1 : net_system = {
  net = net1; 
  m = m0
}

(* Firing m0' [t1> m1 *)
let m1 = ((fire_tk ns1 m0' t1) |> unopt_net_system).m
let show_m1 = marking_to_bindings m1
 
(* Firing m0' [t2> m2 *)
let m2 = ((fire_tk ns1 m0' t2) |> unopt_net_system).m
let show_m2 = marking_to_bindings m2 

(* Firing sequence m0 [t1;t2;t3> m3*)
let m3 = ((firing_sequence_tk ns1 [m0'; m0'; m1'] [t1; t2; t3]) |> unopt_net_system).m
let show_m3 = marking_to_bindings m3

(* Firing sequence m0 [t1;t2;t4> m4*)
let m4 = ((firing_sequence_tk ns1 [m0'; m0'; m1'] [t1; t2; t4]) |> unopt_net_system).m
let show_m4 = marking_to_bindings m4

(* ---- Key labelled net example ---- *)

(* key places *)
let st1 = make_place "st1"

let st2 = make_place "st2"

let st3 = make_place "st3"

let st4 = make_place "st4"

let arcs_k : arc list = [TP (t1, st1); TP (t2, st2); TP (t3, st3); TP (t4, st4)] @ arcs1

let net_key = make_label_net [s1; s2; s3; s4; st1; st2; st3; st4] [t1; t2; t3; t4] arcs_k set label_trans 

let knet1 = {
  net = net_key;
  key = [st1; st2; st3; st4]
}

(* Reversing net - example *)

let u1 = make_transition "u1"

let u2 = make_transition "u2"

let u3 = make_transition "u3"

let u4 = make_transition "u4"

let lambda_r = function t ->
  match t with
  | x when List.mem x [t1; u1] -> "a"
  | x when List.mem x [t2; t3; u2; u3] -> "b"
  | x when List.mem x [t4; u4] -> "tau"
  | _ -> ""

let arcs_r : arc list = [
  PT (s1, t1); TP (t1, s1);
  PT (s1, t2); TP (t1, s2); 
  TP (t2, s3); PT (s3, t4); 
  PT (s2, t4); PT (s2, t3);
  TP (t3, s4); TP (t4, s4);
  PT (st1, u1); PT (s2, u1); TP (u1, s1);
  PT (st2, u2); PT (s3, u2); TP (u2, s1); 
  PT (st3, u3); PT (s4, u3); TP (u3, s2);
  PT (st4, u4); PT (s4, u4); TP (u4, s2); TP (u4, s3)]

let net_key_r = make_label_net [s1; s2; s3; s4; st1; st2; st3; st4] [t1; t2; t3; t4; u1; u2; u3; u4] arcs_r set (fun x -> lambda_r x) 

let knet_r = {
  net = net_key_r;
  key = [st1; st2; st3; st4]
}

let rnet1 = {
  key_net = knet_r;
  rev_trans = [u1; u2; u3; u4]
}

let () =
  assert (is_enabled_tk ns1 t1 == true);
  assert (is_enabled_tk ns1 t3 == false);
  assert (is_key_net knet1 == true);
  assert (is_key_net knet_r == false);

  assert (List.length knet1.net.transitions == List.length knet1.key);

(* let pl_ex : place list = (generate_place 4) @ key_ex *)
(**)
(* let arc_rev = [ *)
(*   PT ("s1", "t1"); *)
(*   TP ("t1", "st1"); PT ("st1", "u1"); TP ("u1", "s1"); *)
(*   PT ("s1", "t2"); TP ("t2", "st2"); PT ("st2", "u2"); *)
(*   TP ("u2", "s1"); *)
(*   TP ("t1", "s2"); TP ("t2", "s3"); *)
(*   PT ("s3", "t4"); PT ("s3", "u2"); *)
(*   PT ("s2", "t4"); PT ("s2", "u1"); *)
(*   PT ("s2", "t3"); TP ("t4", "st4"); PT ("st4","u4"); *)
(*   TP ("t4", "s4"); TP ("t3", "st3"); PT ("st3", "u3"); *)
(*   TP ("t3", "s4"); TP ("u3", "s2"); *)
(*   PT ("s4", "u4"); PT ("s4", "u3"); *)
(*   TP ("u4", "s3"); TP ("u4", "s2"); *)
(* ] *)
(**)
(* let rev_tr : transition list = [ *)
(*   {t_id = "u1"; t_label = "a"}; {t_id = "u2"; t_label = "b"}; *)
(*   {t_id = "u3"; t_label = "b"}; {t_id = "u4"; t_label = "tau"} *)
(* ] *)
(**)
(* let rlambda = fun t -> *)
(*   match t.t_id with *)
(*   | "t1" -> {t_id = t.t_id; t_label = "a"} *)
(*   | "t2" -> {t_id = t.t_id; t_label = "b"} *)
(*   | "t3" -> {t_id = t.t_id; t_label = "b"} *)
(*   | "t4" -> {t_id = t.t_id; t_label = "tau"} *)
(*   | _ -> t  *)
(**)
(**)
(* let net_ex = make_label_net pl_ex (tr @ rev_tr) arc_rev set rlambda *)
(**)
(* let knet_ex : keyPairNet =  *)
(*   {net = net_ex;  *)
(*   key = List.map (fun x -> x.p_id) key3} *)
(**)
(**)
(* let rnet : reversing_net = { *)
(*   net = net_ex; *)
(*   key = List.map (fun x -> x.p_id) key3; *)
(*   rev_trans = rev_tr; *)
(*   } *)
(**)
(* let is_rev = is_reversible_lab_net rnet *)

(* let () = *)
(*   belongs w2 (Sum (Token w1, Sum (Token w2, Token w0))) = true *)

(* 
 This file contain the implementation of the Algorithm 1 of the paper 
 "Encoding Reversible Petri nets into CCSK".
 We use the net of the Fig. 3.
 *)

open Revpn_ccsk.Net
open Revpn_ccsk.Ccsk
open Revpn_ccsk.Encoding


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

let net4 = make_label_net place4 transition4 arcs4 set3 lambda4

let marking0 : marking = [("s1",1)]

let marked_net1 = make_marked_net net4 marking0

let label_of_trans4 net tid = 
  match List.find_opt (fun t -> (net.label_map t).t_id = tid) net.transitions with
  | Some t -> t.t_label
  | None -> "?"

let () =
  List.iter (fun t ->
    let p_size = List.length (preset_of_transition net4 t.t_id) in
    assert (p_size <= 2);
    (* if p_size = 2 then assert (label_of_trans4 net4 t.t_id = "tau") *)
  ) transition4

let encode (m : marked_net) =
  let net = m.net in
  let d0 = init_place_equations net in
  let d1 = encode_simple_transitions net d0 in
  let d2, fresh_actions = encode_sync_transitions net d1 in
  let q = assemble_marking net m |> restrict_all fresh_actions in
  { process = q; equations = d2 }
let result1 = encode marked_net1

let () = 
  print_endline "Algorithm 1 on the synchronising CCS net";
  print_result result1;
  print_newline ()

let () =
  assert (List.length result1.equations = List.length place4);
  List.iter(fun (_, p) ->
    let rec no_yt = function
      | CCS.Zero -> true
      | CCS.Var x -> not (String.length x > 0 && x.[0] = 'Y')
      | CCS.Prefix (_, q) -> no_yt q
      | CCS.Choice (q1, q2) | CCS.Parallel (q1, q2) -> no_yt q1 && no_yt q2
      | CCS.Restriction (q, _) -> no_yt q
      | CCS.Rec (_, q) -> no_yt q
      | _ -> false
    in
    assert (no_yt p)
  ) result1.equations


let () =
  match result1.process with
  | CCS.Restriction (CCS.Var x, [a]) ->
      assert (x = var_of_place "s1");
      assert (a = action_of_sync "t4")
  | CCS.Var x ->
      assert (x = var_of_place "s1")
  | _ -> assert false

let () = print_endline "All Algorithm 1 checks passed"

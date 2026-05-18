
  (***********************************************)


type state = string (* type for states *)

type direction = Fwd | Bwd (* if it corresponds to a forward or backward direction*)

type 'a action = 'a * direction (* type for labels *)

type 'a transition_label = state * 'a * state (* type for transitions *)

type 'a transition = 'a transition_label list (** transition relation type for Labelled Transitions Systems *)

module LTS = struct 
  
  let mk_label q n : state list = List.map (fun x -> q ^ string_of_int x) ( 0-- (n-1) ) (* the number of states generes automatically the set of states *) 

  let mk_actions a ls : 'a action list =
    zip (mk_label a (List.length ls)) ls

    (* the number of states generes automatically the set of states *) 
  let mk_transition ls : 'a transition = ls

  (* let mk_labels ls : label list = *)

  let empty : 'a transition = []

  let create transition_list = transition_list

  let compare_states (s1 : state) (s2 : state) : bool = s1 == s2

  let compare_actions (l1 : 'a action) (l2 : 'a action) : bool = 
    let (a1, d1) = l1 in 
    let (a2, d2) = l2 in 
    a1 == a2 && d1 == d2

  (* Split the list in [(x,Fwd) | x <- label] U [(y,Bwd) | y <- label] *)
  let disj_Fwd_Bwd xs = append_disj (function (_, (_, x), _) -> x == Fwd) xs 

  let extract_states (tr : (label * ('a * direction) * label) list) = set_of_list (List.fold_left (@) [] (List.map (fun (x,(_,_),z) -> [x;z]) tr))

  let extract_actions (tr : (label * ('a * direction) * label) list) = set_of_list (List.fold_left (@) [] (List.map (fun (_,(a,_),_) -> [a]) tr))


  (* Strong forward and reverse bisimulation between two LTS's (Q1,A1,->1) and (Q2,A2,->2) *)
  let cond_bisimulationFR (lts1 : 'a transition) (lts2 : 'a transition) (r : state list) (q1, q2) =
    let q1 = extract_states lts1 in           (* Q1 *)
    let q2 = extract_states lts2 in           (* Q2 *)
    let q = bin_prod q1 q2 in                 (* Q = Q1 x Q2 *)
    let partition_q1 = disj_Fwd_Bwd lts1 in   (* ->1 = ->1^f U ->1^b : is the disjoint union between forward and backward for ->1 *)
    let partition_q2 = disj_Fwd_Bwd lts2 in   (* ->2 = ->2^f U ->2^b : is the disjoint union between forward and backward for ->2 *)
    let rel1f = fst partition_q1 in          (* [(q,q') | q ->1^f q'] *)
    let rel1b = snd partition_q1 in          (* [(q,q') | q ->1^b q'] *)
    let rel2f = fst partition_q2 in          (* [(q,q') | q ->2^f q'] *)
    let rel2b = snd partition_q2 in          (* [(q,q') | q ->2^b q'] *)
    let cond1 = List.for_all ( not ((fun x -> List.mem (q1, x)) rel1f) || List.exists (fun y -> List.mem (q2,y) rel2f && List.mem (x,y) r)) q
    let cond2 = List.for_all ( not ((fun x -> List.mem (q1, x)) rel1b) || List.exists (fun y -> List.mem (q2,y) rel2b && List.mem (x,y) r)) q
    let cond3 = List.for_all ( not ((fun x -> List.mem (q2, x)) rel2f) || List.exists (fun y -> List.mem (q1,y) rel1f && List.mem (x,y) r)) q
    let cond4 = List.for_all ( not ((fun x -> List.mem (q2, x)) rel2b) || List.exists (fun y -> List.mem (q1,y) rel1b && List.mem (x,y) r)) q
    [cond1; cond2; cond3; cond4]

  let bisimulationFR (lts1 : 'a transition) (lts2 : 'a transition) (r : state list) =
    let fq = cond_bisimulationFR lts1 lts2 r
    List.for_all (fun x -> List.mem x r && (!! (fq x) 0) && (!! (fq x) 1) && (!! (fq x) 2) && (!! (fq x) 3)) r

(*   let strong_bisimulationFR (lts1 : 'a transition) (lts2 : 'a transition) (r : state list) = *)
(*     let fq = cond_bisimulationFR lts1 lts2 r *)
(*     List.for_all (fun x -> List.mem x r && (!! (fq x) 0) && (!! (fq x) 2)) r *)
(* end *)
(**)
(* let lts1 = [("q0",(1,Fwd),"q1"); ("q0",(2,Fwd),"q2"); ("q1",(1,Bwd),"q0")] *)
(* let lts2 = [("r0",(1,Bwd),"r1"); ("r1",(1,Bwd),"r0")] *)


(* type 'a state = Q of 'a  *)
(* type 'a action = A of 'a *)
(* type ('a, 'b) relation_trans = ('a state * 'b action * 'a state) list *)
(* type direction = F | B *)
(**)
(* type ('a, 'b) lts = 'a state list * 'b action list * ('a, 'b) relation_trans *)
(* type lts_FR = int state list * (int * direction) action list * (int, direction) relation_trans *)
(**)
(*   (* Ejemplo *) *)
(* let state1 : int state list = [Q 1; Q 2] *)
(**)
(* let action1 : (int * direction) action list = [A (1, F); A (1,B)] *)
(**)
(* let rel1 : (int, int * direction) relation_trans = [(Q 1, A (1,F), Q 2); (Q 2, A (1,B), Q 1)] *)


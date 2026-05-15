
type state = string (* type for states *)


type direction = Fwd | Bwd (* if it corresponds to a forward or backward direction*)

type label = string

type action = label * direction (* type for labels *)

type transition_label = state * action * state (* type for transitions *)

type lts = transition_label list (** type for Labelled Transitions Systems *)

module StringMap = Map.Make(String)

module LTS = struct 
  
  let mk_states ~q:"q" n : state list = List.map (fun x -> "q" ^ string_of_int x) ( 0--n ) (* the number of states generes automatically the set of states *) 
  (**)
  (* let mk_labels ls : label list = *)

  let empty : lts = []

  let new_lts transition_list = transition_list

  (* let compare_states (s1 : state) () *)
end




(* type 'a state = Q of 'a  *)
(* type 'a action = A of 'a *)
(* type ('a, 'b) relation_trans = ('a state * 'b action * 'a state) list *)
(* type direction = F | B *)
(**)
(* type ('a, 'b) lts = 'a state list * 'b action list * ('a, 'b) relation_trans *)
(* type lts_FR = int state list * (int * direction) action list * (int, direction) relation_trans *)
(**)
(* (* Append disjoint union *) *)
(* let append_disj f xs = (List.filter f xs, List.filter (function x -> f x |> not) xs) *)
(**)
(* let disj_forward_reverse xs = append_disj (function (_, A (_, x), _) -> x == F) xs *)
(**)
(*   (* Ejemplo *) *)
(* let state1 : int state list = [Q 1; Q 2] *)
(**)
(* let action1 : (int * direction) action list = [A (1, F); A (1,B)] *)
(**)
(* let rel1 : (int, int * direction) relation_trans = [(Q 1, A (1,F), Q 2); (Q 2, A (1,B), Q 1)] *)


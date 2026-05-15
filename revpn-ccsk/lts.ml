(** Utils *)
(** range from i to j *)

let rec (--) i j = if i > j then [] else i :: i + 1 -- j
  (* let rec aux n acc = *)
  (*     if n < i then acc else aux (n-1) (n :: acc) *)
  (*   in aux j [] ;; *)

let (|>>) x f = 
  match x with
  | None -> None
  | Some y -> f y

let rec zip ls ts = 
  match ls, ts with
  | [], _ -> []
  | _, [] -> []
  |x :: xs, y :: ys -> (x,y) :: (zip xs ys)

(* Append disjoint union *)
let append_disj f xs = List.filter f xs, List.filter (function x -> f x |> not) xs

(* Get the n-th element of a list *)
let rec ( !! ) xs n = 
  match xs, n with
    | [], _ -> raise (Failure "get_nth")
    | _, n when (n > List.length xs) -> raise (Invalid_argument "get_nth")
    | x ::_, 0 -> x
    | x :: xs, n -> !! xs (n-1)

let init : 'a list -> 'a list = fun xs -> List.rev xs |> List.tl |> List.rev

let last : 'a list -> 'a = fun xs -> !! xs (List.length xs - 1)

(** Concat lists *)
(* let rec ( ++ ) xs ys = *)
(*   match xs with  *)
(*   | [] -> ys  *)
(*   | xs -> (init xs) ++ ((last xs) :: ys) *)


(* Lists to set *)

let rec set_of_list xs = match xs with 
  | [] -> []
  | x :: xs -> if List.mem x xs then set_of_list xs else x :: set_of_list xs


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

  (* Strong forward and reverse bisimulation between two LTS's *)
  (* let rec bisimulationFR (lts1 : transition) (lts2 : transition) : bool = *)
  (*   let  *)

end




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


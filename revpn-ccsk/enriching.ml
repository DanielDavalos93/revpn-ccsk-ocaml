open Net
open Lts
open Util

type token = 
  | Tok_empty
  | Tok of (label * token list * int)

let rec size (tk : token) : int =
  match tk with
  | Tok_empty -> 0
  | Tok (_, tok, _) -> 
      List.fold_left (max) 0 (List.map (size) tok) + 1

let (+.) (t1: token) (t2: token) : token list =
  [t1; t2]

let kt (w : token) =
  match w with
  | Tok_empty -> 0
  | Tok (_,_,j) -> j

let lab_tok (w : token) =
  match w with
  | Tok_empty -> ""
  | Tok (a,_,_) -> a

let toks (w : token) =
  match w with
  | Tok_empty -> []
  | Tok (_,w,_) -> w

let (-.) (t: token) (sub_tok: token) : token =
  match t with
  | Tok_empty -> Tok_empty
  | Tok (a, ts, i) -> Tok (a, remove_one_token ts sub_tok, i)


let w0 = Tok_empty                        (* size w0 -> 0 *)

let w1 = Tok ("a", [Tok_empty], 1)        (* size w1 -> 1 *)

let w2 = Tok ("b", [Tok_empty], 1)        (* size w2 -> 1 *)

let w3 = Tok ("a", [Tok_empty;
          Tok ("b", [Tok_empty], 1)], 2)  (* size w3 -> 2 *)

let w4 = Tok ("tau", [w3], 3)  (* size w4 -> 2 *)

type token_net = {
  net : labelled_net;
  key : token;
}

let make_token_net (net : labelled_net) (tok : token) : token_net = {
  net = net; key = tok;
}

let immediate_transitions (ln : labelled_net) : (transition_id * transition_id) list =
  let plSet = get_place ln in
  let trSet = get_transition ln in
  let binProd = bin_prod trSet trSet in
  List.filter (fun pair ->
    (List.exists (fun x ->
      List.mem (TP (fst pair, x)) ln.arcs && List.mem (PT (x, snd pair)) ln.arcs
    ) plSet)
  ) binProd


let rec m_flat (net : labelled_net) (marking : marking) : token =
  let lbl pid =
    match List.find_opt (fun p -> p.p_id = pid) net.places with
    | Some _ ->
        pid
    | None   ->
        match List.find_opt (fun t -> t.t_id = pid) net.transitions with
        | Some t -> (net.label_map t).t_label
        | None   -> pid
  in
  match marking with
  | []      -> Tok_empty
  | [p]     -> Tok (lbl p, [Tok_empty], 1)
  | p :: ps ->
      let child = m_flat net ps in
      Tok (lbl p, [m_flat net [p]; child], size child + 1)


let is_enabled_tk (tn : token_net) (tid : transition_id) : bool =
  let inputs = input_arcs tn.net tid in
  match tn.key with
  | Tok_empty -> false
  | Tok (a, t, _) -> 
      (* let lamb_t = (tn.net.label_map {t_id = tid; t_label = ""}).t_label in *)
      List.mem a inputs

let next_id (tk : token) : int = size tk + 1

let fire_tk (tn : token_net) (tid : transition_id) : token_net option =
  if not (is_enabled_tk tn tid) then None
  else
    let outputs = output_arcs tn.net tid in
    let w       = tn.key in
    let base_id = next_id w in
    let new_token =
      match outputs with
      | []  ->
          Tok_empty
      | [q] ->
          Tok (q, [w], base_id)
      | qs  ->
          let children =
            List.mapi (fun i q -> Tok (q, [w], base_id + i)) qs
          in
          (* the new token is tagged with the transition id *)
          Tok ((tn.net.label_map {t_id = tid; t_label = ""}).t_id, children, base_id)
    in
    Some { tn with key = new_token }

let unfire_tk (tn : token_net) (tid : transition_id) : token_net option =
  match tn.key with
  | Tok_empty -> None
  | Tok (label, children, _) ->
      let outputs = output_arcs tn.net tid in
      if label <> tid && not (List.mem label outputs) then None
      else
        match children with
        | [w] ->
            Some { tn with key = w }
        | _ ->
           let inputs = input_arcs tn.net tid in
            begin match List.find_opt
              (fun w -> match w with
                | Tok (a,_,_) -> List.mem a inputs
                | Tok_empty   -> false)
              children
            with
            | Some w -> Some { tn with key = w }
            | None   -> None
            end

let rec firing_sequence_tk (tn : token_net) (ts : transition_id list) : token_net option =
  match ts with
  | []      -> Some tn
  | t :: ts ->
      match fire_tk tn t with
      | None     -> None
      | Some tn' -> firing_sequence_tk tn' ts

type keyPairNet = {
  net : labelled_net;
  key : place_id list;
  }

let is_key_net (kn : keyPairNet) : bool =
    List.for_all (fun t ->
      let post_t = output_arcs kn.net t in
      List.length (intersect post_t kn.key) = 1
    ) (get_transition kn.net) &&
    List.for_all (fun s ->
      let pre_s = List.map (fun x -> x.t_id) (preset_of_place kn.net s) in
      let post_s = List.map (fun x -> x.t_id) (postset_of_place kn.net s) in
      List.length pre_s = 1 && List.length post_s = 0
    ) kn.key

let key3 : place list = List.map (fun i -> 
    "st" ^ string_of_int i |> make_place) (1--4)

let pl3 : place list = (generate_place 4) @ key3

let arcs3 = [
  PT ("s1", "t1"); TP ("t1", "s1");
  TP ("t1", "st1"); 
  PT ("s1", "t2"); TP ("t2", "st2");
  TP ("t1", "s2"); TP ("t2", "s3");
  PT ("s3", "t4"); PT ("s2", "t4");
  PT ("s2", "t3"); TP ("t4", "st4");
  TP ("t4", "s1"); TP ("t3", "st3");
  TP ("t3", "s4"); TP ("t4", "s4")
]

let net3 = make_label_net pl3 tr arcs3 set label_trans

let knet3 : keyPairNet = 
  {net = net3; 
  key = List.map (fun x -> x.p_id) key3}

let keypl (kn : keyPairNet) (t : transition_id) =
  let sk = kn.key in
  List.filter (fun x -> 
    (List.map (fun y -> y.t_id) (preset_of_place kn.net x)) = [t]) sk

type reversing_net = {
  net : labelled_net;
  key : place_id list;
  rev_trans : transition list;
}

let forwardTransition (rn : reversing_net) : transition list =
  setminus rn.net.transitions rn.rev_trans

let forwardNet (rn : reversing_net) : keyPairNet = 
  let fwdT = forwardTransition rn in
  let fwd_arcs = List.filter (fun x ->
          (List.mem (pi x Fst) (List.map (fun x -> x.t_id) fwdT)) ||
          (List.mem (pi x Snd) (List.map (fun x -> x.t_id) fwdT)) 
          ) rn.net.arcs in
  {
    net = {
      places = rn.net.places;
      transitions = fwdT;
      arcs = fwd_arcs;
      set = rn.net.set;
      label_map = rn.net.label_map;
      };
    key = rn.key;
    } 

let prop_reverse_transition (rn : reversing_net) (u : transition) = 
  fun t -> 
      (preset_of_transition rn.net u.t_id = 
        postset_of_transition rn.net t.t_id) &&
      (postset_of_transition rn.net u.t_id = 
        preset_of_transition rn.net t.t_id) &&
      ((rn.net.label_map t).t_label = (rn.net.label_map u).t_label)

let is_reversible_lab_net (rn : reversing_net) : bool =
  let fwdT = forwardTransition rn in
  is_key_net (forwardNet rn) &&
  is_subset rn.rev_trans rn.net.transitions &&
  List.for_all (fun u ->
    List.exists (fun t ->
    exists_unique fwdT (prop_reverse_transition rn u) t
    ) fwdT
  ) rn.rev_trans


(** Enabling and Firing a Reversing Transition.*)


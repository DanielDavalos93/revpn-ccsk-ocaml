open Util
open List

type place_id = string
type transition_id = string

type place = {
  p_id: place_id;
}

type transition = {
  t_id: transition_id;
  t_label: string
}

(** [arc] is the immediate relation between places and transitions.
 Here the type [int] plays the rol of the number of tokens. *)
type arc = 
  | PT of (place_id * transition_id * int) 
  | TP of (transition_id * place_id * int)

type marking = (place_id * int) list

type labelled_net = {
  places  : place list;
  transitions : transition list;
  arcs : arc list;
  set : string list;
  label_map : transition -> transition;
}

let make_label_net (places : place list) (transitions : transition list) (arcs : arc list) 
    (set : string list) (label_map : transition -> transition) : labelled_net =
  { places; transitions; arcs; set; label_map}

let make_place id : place =
  { p_id = id }

let generate_place n =
  List.map (fun x -> "s" ^ string_of_int x |> make_place) (1--n)

let make_transition id : transition =
  { t_id = id; t_label = "" }

let generate_transition n  =
  List.map (fun x -> "t" ^ string_of_int x |> make_transition) (1--n)

let make_label f t =
  List.map (f) t

type marked_net = {
  net : labelled_net;
  marking : marking;
}

let empty_transition : transition = {
    t_id = "";
    t_label = "";
  }

let empty_marked_net : marked_net =
  let empty_label_net = make_label_net [] [] [] [] (fun x -> empty_transition) in
  {
    net = empty_label_net;
    marking = [];
  }

let make_marked_net net marking : marked_net =
  {net; marking}

let get_transition (n : labelled_net)  =
  List.map (fun x -> x.t_id) n.transitions

let get_place (n : labelled_net)  =
  List.map (fun x -> x.p_id) n.places

let tokens net p =
  match List.assoc_opt p net.marking with
  | Some n -> n
  | None   -> 0
 
let set_tokens (marking:marking) (p:transition_id) (n:int) =
  let rest = List.filter (fun (k, _) -> k <> p) marking in
  if n = 0 then rest else (p, n) :: rest

let input_arcs net tid =
  List.filter_map (function
    | PT (pid, t, w) when t = tid -> Some (pid, w)
    | _ -> None
  ) net.arcs
 
let output_arcs net tid =
  List.filter_map (function
    | TP (t, pid, w) when t = tid -> Some (pid, w)
    | _ -> None
  ) net.arcs

(* ---- Preset and Postset -------- *)
 
let preset_of_transition net tid =
  List.filter_map (function
    | PT (pid, t, _) when t = tid ->
        List.find_opt (fun p -> p.p_id = pid) net.places
    | _ -> None
  ) net.arcs
 
let postset_of_transition net tid =
  List.filter_map (function
    | TP (t, pid, _) when t = tid ->
        List.find_opt (fun p -> p.p_id = pid) net.places
    | _ -> None
  ) net.arcs
 
let preset_of_place net pid =
  List.filter_map (function
    | TP (tid, p, _) when p = pid ->
        List.find_opt (fun t -> t.t_id = tid) net.transitions
    | _ -> None
  ) net.arcs
 
let postset_of_place net pid =
  List.filter_map (function
    | PT (p, tid, _) when p = pid ->
        List.find_opt (fun t -> t.t_id = tid) net.transitions
    | _ -> None
  ) net.arcs
 
let is_enabled (mn : marked_net) (tid : transition_id) =
  List.for_all
    (fun (pid, w) -> tokens mn pid >= w)
    (input_arcs mn.net tid)
 
(** Fire a transition if it is enabled; return the net with
    the new marking on [Some] of [None] if it isn't enabled.*)
let fire (net : marked_net) (tid :transition_id) : marked_net option =
  if not (is_enabled net tid) then None (*Error "Transition is not enabled"*)
  else
    let m1 =
      List.fold_left
        (fun m (pid, w) -> set_tokens m pid (List.assoc pid m - w))
        net.marking
        (input_arcs net.net tid)
    in
    let m2 =
      List.fold_left
        (fun m (pid, w) ->
          let cur = match List.assoc_opt pid m with Some n -> n | None -> 0 in
          set_tokens m pid (cur + w))
        m1
        (output_arcs net.net tid)
    in
    Some { net with marking = m2 }

(* Auxiliar function: [a option] -> [a] *)
let un_opt (x : marked_net option) : marked_net =
  match x with
  | None -> empty_marked_net
  | Some ls -> ls

let rec firing_sequence (mn : marked_net) (s : transition_id list) : marked_net option =
  match s with
  | [] -> Some mn
  | [t] -> fire mn t
  | t :: ts ->
      let m1 = (fire mn t |> un_opt).marking in
      let net1 = {
        net = mn.net;
        marking = m1
      } in
      firing_sequence net1 ts

(** List of enabled transitions *)
let enabled_transitions net =
  List.filter_map (fun t ->
    if is_enabled net t.t_id then Some t else None
  ) net.net.transitions

let find_reachable (mn : marked_net) (m : marking) =
  let ls = all_comb (List.map (fun x -> x.t_id) mn.net.transitions) in
  find_opt (fun x ->
    ((firing_sequence mn x |> un_opt).marking = m)) ls

(** [is_reachable M m] return true if there is a sequence of
  transitions [s = t1;...;tn] such that the firing sequence 
  from the initial marking [M.marking] with the sequence [s]
  produces the marking [m].
*)
let is_reachable (mn : marked_net) (m : marking) : bool =
  not (find_reachable mn m = None)

(* let reachable_markings (mn : marked_net) =
  let all_places = get_place mn.net in
  let marks = bin_prod all_places [1] in
  let all_marks = all_places mn.net |> all_comb in
   *)

(** All combinations of transition ids (subsets, no repetition).
    For n transitions produces 2^n sublists. *)
let all_comb_transitions (mn : marked_net) : transition_id list list =
  let tids = List.map (fun t -> t.t_id) mn.net.transitions in
  all_comb tids

(** [reachable_markings mn] returns the set of all markings [m] such that
    there exists a sequence [ts] (subset of transition ids, no repetition)
    where [firing_sequence mn ts = Some mn'] and [mn'.marking = m].

    Strategy: enumerate all subsets of transitions via [all_comb],
    fire each as a sequence, collect distinct successful markings. *)
let reachable_markings (mn : marked_net) : marking list =
  let normalize m =
    m |> List.filter (fun (_, n) -> n > 0)
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let marking_equal a b = normalize a = normalize b in
  (* Collect all markings reached by firing any valid subsequence *)
  let candidates : marking list =
    all_comb_transitions mn
    |> List.filter_map (fun ts ->
        match firing_sequence mn ts with
        | Some mn' -> Some mn'.marking
        | None     -> None) in
  let all = mn.marking :: candidates in
  (* Deduplicate *)
  List.fold_left (fun acc m ->
    if List.exists (marking_equal m) acc then acc
    else m :: acc
  ) [] all
  |> List.rev


  type reachable_entry = {
  re_marking  : marking;
  re_witness  : transition_id list;  (* firing_sequence mn ts = Some {marking; ...} *)
}

let reachable_markings_with_witness (mn : marked_net) : reachable_entry list =
  let normalize m =
    m |> List.filter (fun (_, n) -> n > 0)
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let marking_equal a b = normalize a = normalize b in

  let initial_entry = { re_marking = mn.marking; re_witness = [] } in

  let candidates : reachable_entry list =
    all_comb_transitions mn
    |> List.filter_map (fun ts ->
        match firing_sequence mn ts with
        | Some mn' -> Some { re_marking = mn'.marking; re_witness = ts }
        | None     -> None)
  in

  List.fold_left (fun acc entry ->
    if List.exists (fun e -> marking_equal e.re_marking entry.re_marking) acc
    then acc
    else acc @ [entry]
  ) [initial_entry] candidates
   
(** ----- Marking graph ------------------- *)

(** A labelled edge in the marking graph:
    m' -[a]-> m''  where  mn.net.label t = a  and  fire {mn | marking=m'} t = Some {marking=m''} *)
(* type mg_edge = { *)
(*   mg_src   : marking;          (* m'  *) *)
(*   mg_label : string;           (* a = (mn.net.label t).t_label *) *)
(*   mg_dst   : marking;          (* m'' *) *)
(* } *)
(**)
(* type marking_graph = { *)
(*   mg_states  : marking list;    (* R  : reachable markings        *) *)
(*   mg_sigma   : string list;           (* Σ  : mn.net.set                *) *)
(*   mg_edges   : mg_edge list;    (* -> : labelled transition rel.  *) *)
(*   mg_initial : marking;         (* M0 : initial marking           *) *)
(* } *)

(** Normalize a marking to a canonical form for equality checks *)
let normalize (m : marking) : marking =
  m |> List.filter (fun (_, n) -> n > 0)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let marking_equal (a : marking) (b : marking) : bool =
  normalize a = normalize b

(** [marking_graph mn] constructs the marking graph
      (reachable_markings mn,  mn.net.set,  ->,  mn.marking)
 *)
(* let marking_graph (mn : marked_net) : lts = *)
(*   let states = List.map (fun x -> x) reachable_markings mn in *)
(*   let sigma  = mn.net.set in *)
(*   let edges  = *)
(*     List.concat_map (fun m' -> *)
(*       List.filter_map (fun t -> *)
(*         let mn' = { mn with marking = m' } in *)
(*         match fire mn' t.t_id with *)
(*         | None      -> None *)
(*         | Some mn'' -> *)
(*             Some { mg_src   = m'; *)
(*                    mg_label = (mn.net.label_map t).t_label;  (* : string list *) *)
(*                    mg_dst   = mn''.marking } *)
(*       ) mn.net.transitions *)
(*     ) states *)
(*   in *)
(*   { states  = states; *)
(*     trans   = edges; *)
(*   } *)

(* ------- Pretty-print ----------------- *)
 
let print_preset_postset net =
  print_endline "Preset and Postset of transitions";
  List.iter (fun t ->
    let pre  = preset_of_transition  net t.t_id |> List.map (fun p -> p.p_id) in
    let post = postset_of_transition net t.t_id |> List.map (fun p -> p.p_id) in
    Printf.printf "  •%s = { %s }\n"  t.t_id (String.concat ", " pre);
    Printf.printf "   %s• = { %s }\n" t.t_id (String.concat ", " post)
  ) net.transitions;
  print_endline "Preset and Postset of places";
  List.iter (fun p ->
    let pre  = preset_of_place  net p.p_id |> List.map (fun t -> t.t_id) in
    let post = postset_of_place net p.p_id |> List.map (fun t -> t.t_id) in
    Printf.printf "  •%s = { %s }\n"  p.p_id (String.concat ", " pre);
    Printf.printf "   %s• = { %s }\n" p.p_id (String.concat ", " post)
  ) net.places
 
let print_marking net =
  print_endline "Current marking:";
  List.iter (fun p ->
    Printf.printf "  %-15s : %d token(s)\n" p.p_id (tokens net p.p_id)
  ) net.net.places
 
let print_enabled net =
  let ts = enabled_transitions net in
  if ts = [] then
    print_endline "  (without enabled transitions)"
  else
    List.iter (fun t ->
      Printf.printf "  [%s] %s\n" t.t_id t.t_id
    ) ts
 

(** example *)

let pl = generate_place 4

let tr = generate_transition 4

let arcs = [
  PT ("s1", "t1", 1); TP ("t1", "s1", 1); PT ("s1", "t2", 1);
  TP ("t1", "s2", 1); TP ("t2", "s3", 1);
  PT ("s3", "t4", 1); PT ("s2", "t4", 1); PT ("s2", "t3", 1);
  TP ("t3", "s4", 1); TP ("t4", "s4", 1)
]

let set : transition_id list = ["a"; "b"; "tau"]

let lambda t  =
  match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | "t3" -> {t_id = t.t_id; t_label = "b"}
  | "t4" -> {t_id = t.t_id; t_label = "tau"}
  | _ -> {t_id = t.t_id; t_label = t.t_label}

let label_trans = fun x -> lambda x

let net1 = make_label_net pl tr arcs set label_trans

let init_marking = [("s1", 1)]

let mnet1 = make_marked_net net1 init_marking

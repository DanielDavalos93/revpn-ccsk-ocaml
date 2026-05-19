
(* Multisets *)
type 'a multiset = ('a * int) list

(* let supp (f : 'a multiset) = fun x -> if f x > 0 then 1 else 0 *)

(* Operations for multisets *)
(* let fun_mult (f : int -> int -> int) (m1:string multiset) (m2:string multiset) : string multiset = *)
(*   let sortedm1 = sort_increasing_pair_left m1 in *)
(*   let sortedm2 = sort_increasing_pair_left m2 in *)
(*   let z = zip sortedm1 sortedm2 in *)
(*   List.map (fun ((x1,x2), (y1,y2)) -> (x1, f x2 y2)) z *)
(**)
(* let sum_mul = fun_mult (+) *)
(**)
(* let diff_mul = fun_mult (fun x y -> max (x-y) 0) *)

type place_id = string
type transition_id = string

type place = {
  p_id: place_id;
  p_name: string;
}

type transition = {
  t_id: transition_id;
  t_name: string;
}

type arc = PT of (place_id * transition_id * int) | TP of (transition_id * place_id * int)

type marking = (place_id * int) list

type labelled_net = {
  places  : place list;
  transitions : transition list;
  arcs : arc list;
  set : string list;
  label : transition list -> string list;
}

let make_label_net places transitions arcs set label : labelled_net =
  { places; transitions; arcs; set; label }

let make_place id name : place =
  { p_id = id; p_name = name }
 
let make_transition id name : transition =
  { t_id = id; t_name = name }

type marked_net = {
  net : labelled_net;
  marking : marking;
}

let tokens net p =
  match List.assoc_opt p net.marking with
  | Some n -> n
  | None   -> 0
 
let set_tokens marking p n =
  let rest = List.filter (fun (k, _) -> k <> p) marking in
  if n = 0 then rest else (p, n) :: rest

(** Arcos de entrada (place → trans) para una transición. *)
let input_arcs net tid =
  List.filter_map (function
    | PT (pid, t, w) when t = tid -> Some (pid, w)
    | _ -> None
  ) net.arcs
 
(** Arcos de salida (trans → place) para una transición. *)
let output_arcs net tid =
  List.filter_map (function
    | TP (t, pid, w) when t = tid -> Some (pid, w)
    | _ -> None
  ) net.arcs

(* ---- Preset y Postset -------- *)
 
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
 
let is_enabled net tid =
  List.for_all
    (fun (pid, w) -> tokens net pid >= w)
    (input_arcs net tid)
 
(** Fire a transition if it is enabled; return the net with
    the new marking on [Some] of [None] if it isn't enabled.*)
let fire net tid =
  if not (is_enabled net tid) then None
  else
    let m1 =
      List.fold_left
        (fun m (pid, w) -> set_tokens m pid (List.assoc pid m - w))
        net.marking
        (input_arcs net tid)
    in
    let m2 =
      List.fold_left
        (fun m (pid, w) ->
          let cur = match List.assoc_opt pid m with Some n -> n | None -> 0 in
          set_tokens m pid (cur + w))
        m1
        (output_arcs net tid)
    in
    Some { net with marking = m2 }
 
(** List of enabled transitions *)
let enabled_transitions net =
  List.filter_map (fun t ->
    if is_enabled net t.t_id then Some t else None
  ) net.transitions

(* ------- Pretty-print ----------------- *)
 
let print_preset_postset net =
  print_endline "Preset and Postset of transitions";
  List.iter (fun t ->
    let pre  = preset_of_transition  net t.t_id |> List.map (fun p -> p.p_name) in
    let post = postset_of_transition net t.t_id |> List.map (fun p -> p.p_name) in
    Printf.printf "  •%s = { %s }\n"  t.t_name (String.concat ", " pre);
    Printf.printf "   %s• = { %s }\n" t.t_name (String.concat ", " post)
  ) net.transitions;
  print_endline "Preset and Postset of places";
  List.iter (fun p ->
    let pre  = preset_of_place  net p.p_id |> List.map (fun t -> t.t_name) in
    let post = postset_of_place net p.p_id |> List.map (fun t -> t.t_name) in
    Printf.printf "  •%s = { %s }\n"  p.p_name (String.concat ", " pre);
    Printf.printf "   %s• = { %s }\n" p.p_name (String.concat ", " post)
  ) net.places
 
let print_marking net =
  print_endline "Current marking:";
  List.iter (fun p ->
    Printf.printf "  %-15s : %d token(s)\n" p.p_name (tokens net p.p_id)
  ) net.places
 
let print_enabled net =
  let ts = enabled_transitions net in
  if ts = [] then
    print_endline "  (without enabled transitions)"
  else
    List.iter (fun t ->
      Printf.printf "  [%s] %s\n" t.t_id t.t_name
    ) ts
 

(* module Net = struct *)
(**)
(*   let places name : place list = name *)
(**)
(*   let transitions name : transition list = name *)
(**)
(*   let flow name : immed_rel list = name *)
(**)
(*   let labelling_map lam  = fun x -> lam x *)
(**)
(*   let mk_state q n : place list = List.map (fun x -> q ^ string_of_int x) ( 0--(n-1) ) *)
(**)
(*   let mk_trans t n : transition list = List.map (fun x -> t ^ string_of_int x) ( 0--(n-1) ) *)
(**)
(**)
(*   let preset (t : transition) = List.filter (fun p -> List.mem (PT (p,t)) flow) places *)
(**)
(*   let postset (t : transition) = List.filter (fun p -> List.mem (t,p) flow) places *)
(* end *)

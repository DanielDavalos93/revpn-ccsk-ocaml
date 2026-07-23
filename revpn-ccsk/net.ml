open Util

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
  | PT of (place_id * transition_id) 
  | TP of (transition_id * place_id)

type marking = place_id list

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

let tokens (mn : marked_net) (p : place_id) =
  List.filter (fun x -> p = x) mn.marking |> List.length
 
let input_arcs net tid : marking =
  List.filter_map (function
    | PT (pid, t) when t = tid -> Some pid
    | _ -> None
  ) net.arcs
 
let output_arcs net tid : marking =
  List.filter_map (function
    | TP (t, pid) when t = tid -> Some pid
    | _ -> None
  ) net.arcs

(* ---- Preset and Postset -------- *)
 
let preset_of_transition net tid =
  List.filter_map (function
    | PT (pid, t) when t = tid ->
        List.find_opt (fun p -> p.p_id = pid) net.places
    | _ -> None
  ) net.arcs
 
let postset_of_transition net tid =
  List.filter_map (function
    | TP (t, pid) when t = tid ->
        List.find_opt (fun p -> p.p_id = pid) net.places
    | _ -> None
  ) net.arcs
 
let preset_of_place net pid =
  List.filter_map (function
    | TP (tid, p) when p = pid ->
        List.find_opt (fun t -> t.t_id = tid) net.transitions
    | _ -> None
  ) net.arcs
 
let postset_of_place net pid =
  List.filter_map (function
    | PT (p, tid) when p = pid ->
        List.find_opt (fun t -> t.t_id = tid) net.transitions
    | _ -> None
  ) net.arcs
 
let is_enabled (mn : marked_net) (tid : transition_id) =
  List.for_all
    (fun pid -> tokens mn pid >= 1)
    (input_arcs mn.net tid)
 
let remove_one_token m pid =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> if x = pid then acc @ xs else aux (x::acc) xs
  in
  aux [] m

let setminus m1 m2 =
  List.fold_left (fun acc pid -> remove_one_token acc pid) m1 m2

(** Fire a transition if it is enabled; return the net with
    the new marking on [Some] of [None] if it isn't enabled.*)
let fire (net : marked_net) (tid :transition_id) : marked_net option =
  if not (is_enabled net tid) then None (*Error "Transition is not enabled"*)
  else
    let m1 = input_arcs net.net tid in
    let m2 = output_arcs net.net tid in
    let m = net.marking in
    Some { net with marking = (setminus m m1) @ m2 }

(* Auxiliar function: [a option] -> [a] *)
let un_opt (x : marked_net option) : marked_net =
  match x with
  | None -> empty_marked_net
  | Some ls -> ls

let rec firing_sequence (mn : marked_net) (s : transition_id list) : marked_net option =
  match s with
  | [] -> Some mn
  (* | [t] -> fire mn t *)
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
  

(* let pair_fir (mn : marked_net) (t : transition_id) : marking * transition_id * marking = *)
(*   let m1 = mn.marking in *)
(*   let m2 = (fire mn t |> un_opt).marking in *)
(*   (m1, t, m2) *)

(** Canonical key for a marking: sorted list of place_ids. *)
let marking_key (m : marking) : transition_id =
  m |> List.sort String.compare |> String.concat ";"

(** [marking_graph mn] returns all reachable edges (m, t, m') from [mn.marking]
    by BFS, visiting each marking at most once. *)
let marking_graph (mn : marked_net) : (marking * transition_id * marking) list =
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 124 in
  let queue   : marking Queue.t           = Queue.create () in
  let edges   : (marking * transition_id * marking) list ref = ref [] in
  let enqueue m =
    let k = marking_key m in
    if not (Hashtbl.mem visited k) then begin
      Hashtbl.add visited k ();
      Queue.push m queue
    end
  in
  enqueue mn.marking;
  while not (Queue.is_empty queue) do
    let m    = Queue.pop queue in
    let cur  = { mn with marking = m } in
    let en = enabled_transitions cur in
    Printf.printf "Estado: %s, transiciones habilitadas: %d\n" 
      (marking_key m) (List.length en);
    List.iter (fun t ->
      match fire cur t.t_id with
      | None     -> ()
      | Some mn' ->
          edges := (m, (cur.net.label_map t).t_label, mn'.marking) :: !edges;
          enqueue mn'.marking
    ) (enabled_transitions cur);
  done;
  List.rev !edges

let reachable_markings (mn : marked_net) =
  let init = mn.marking in
  [init] @ List.map (fun (_,_,x) -> x) (marking_graph mn)

let ccs_net (ln : labelled_net) =
  List.for_all (fun x ->
    let len_pre = List.length (preset_of_transition ln x.t_id) in
    let lam_t = (ln.label_map x).t_label in
    len_pre <= 2 && ( not (len_pre = 2) || lam_t = "tau")
  ) ln.transitions

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

(** ----------- Examples ---------------- *)

(** example1 *)

let pl : place list = generate_place 4

let tr : transition list = generate_transition 4

let arcs = [
  PT ("s1", "t1"); (*TP ("t1", "s1"); *)
  PT ("s1", "t2");
  TP ("t1", "s2"); TP ("t2", "s3");
  PT ("s3", "t4"); PT ("s2", "t4"); 
  PT ("s2", "t3"); TP ("t4", "s1");
  TP ("t3", "s4"); TP ("t4", "s4")
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

let init_marking = ["s1"]

let mnet1 = make_marked_net net1 init_marking

(** example 2 *)

let pl2 = generate_place 2
let tr2 = generate_transition 2
let arcs2 = [
  PT ("s1","t1");
  PT ("s1","t2");
  TP ("t1","s2");
  TP ("t2","s2")
]
let init2 = ["s1"]

let set2 : transition_id list = ["a"; "b"]

let lambda2 t  =
  match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | _ -> {t_id = t.t_id; t_label = t.t_label}

let label_trans2 = fun x -> lambda2 x

let net2 = make_label_net pl2 tr2 arcs2 set2 label_trans2

let init2 = ["s1"]

let mnet2 = make_marked_net net2 init2


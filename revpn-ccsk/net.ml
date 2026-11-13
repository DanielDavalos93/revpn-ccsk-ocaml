open Util
open Lts

module Place      = Make_id ()
module Transition = Make_id ()

type place      = Place.t
type transition = Transition.t

type arc = 
  | PT of place * transition 
  | TP of transition * place

type marking = place list

type labelled_net = {
  places  : place list;
  transitions : transition list;
  arcs : arc list;
  set : label list;
  label_map : transition -> label;
}

let make_label_net (places : place list) (transitions : transition list) (arcs : arc list) 
    (set : label list) (label_map : transition -> label) : labelled_net =
  { places; transitions; arcs; set; label_map }

let make_place (s : string) : place = Place.of_string s

let generate_place n =
  List.map (fun x -> "s" ^ string_of_int x |> make_place) (1--n)

let make_transition (s : string) : transition = Transition.of_string s

let generate_transition n  =
  List.map (fun x -> "t" ^ string_of_int x |> make_transition) (1--n)

let make_label_list (ls : string list) : label list = ls

let make_label (s : string) : label = s

type marked_net = {
  net : labelled_net;
  marking : marking;
}

let generate_marking (ls : string list) : marking =
  List.map Place.of_string ls

let empty_transition : transition = make_transition "" 

let empty_marked_net : marked_net =
  let empty_label_net = make_label_net [] [] [] [] (fun _ ->
    (Transition.to_string empty_transition)) in
  {
    net = empty_label_net;
    marking = [];
  }

type int_projection =
  | Fst
  | Snd

let pi_t (r : arc) (i : int_projection) =
  match r, i with
  | TP (t,_), Fst -> t
  | PT (_,t), Snd -> t
  | _, _ -> empty_transition

let pi_p (r : arc) (i : int_projection) =
  match r, i with
  | TP (_,p), Snd -> p
  | PT (p,_), Fst -> p
  | _, _ -> Place.of_string "" 


let make_marked_net net marking : marked_net =
  {net; marking}

let get_transition (n : labelled_net) = n.transitions

let get_place (n : labelled_net) : marking = n.places

let tokens (mn : marked_net) (p : place) =
  List.filter (fun x -> p = x) mn.marking |> List.length
 
let input_arcs n tid  =
  List.filter_map (function
    | PT (pid, t) when t = tid -> Some pid
    | _ -> None
  ) n.arcs
 
let output_arcs n tid  =
  List.filter_map (function
    | TP (t, pid) when t = tid -> Some pid
    | _ -> None
  ) n.arcs

(* ---- Preset and Postset -------- *)
 
let preset_of_transition net tid : place list =
  List.filter (fun x -> List.mem (PT (x, tid)) (net.arcs)) (get_place net)
 
let postset_of_transition net tid : place list =
  List.filter (fun x -> (List.mem (TP (tid, x)) (net.arcs))) (get_place net)

let preset_of_place net pid =
  List.filter_map (function
    | TP (tid, p) when p = pid ->
        List.find_opt (fun t -> t = tid) net.transitions
    | _ -> None
  ) net.arcs
 
let postset_of_place net pid =
  List.filter_map (function
    | PT (p, tid) when p = pid ->
        List.find_opt (fun t -> t = tid) net.transitions
    | _ -> None
  ) net.arcs
 
let initial_marking (n : labelled_net) : place list =
  let pl = get_place n in
  List.filter (fun x -> preset_of_place n x = []) pl

let final_marking (n : labelled_net) : place list =
  let pl = get_place n in
  List.filter (fun x -> postset_of_place n x = []) pl
  
let is_enabled (mn : marked_net) (tid : transition) =
  List.for_all (fun pid -> tokens mn pid >= 1)
    (input_arcs mn.net tid)

let fire (net : marked_net) (tid :transition) : marked_net option =
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

let rec firing_sequence (mn : marked_net) (s : transition list) : marked_net option =
  match s with
  | [] -> Some mn
  | t :: ts ->
      let m1 = (fire mn t |> un_opt).marking in
      let net1 = {
        net = mn.net;
        marking = m1
      } in
      firing_sequence net1 ts


let enabled_transitions net =
    List.filter_map (fun t ->
      if is_enabled net t then Some t else None
    ) net.net.transitions

let pair_fir (mn : marked_net) (t : transition) : marking * transition * marking =
  let m1 = mn.marking in
  let m2 = (fire mn t |> un_opt).marking in
  (m1, t, m2)

let pairs_back (mn : marked_net) (tid : transition) : (place * transition) list =
  let preset_tid = preset_of_transition mn.net tid in
  (List.map (fun s -> (s, tid)) preset_tid)

let marking_key (m : marking) = 
  m |> List.map (Place.to_string) |> String.concat ";"

let normalize_marking (m : marking) =
  m |> List.map Place.to_string |> List.sort String.compare 

let marking_graph (mn : marked_net) : (marking * label * marking) list =
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let queue   : marking Queue.t          = Queue.create () in
  let edges   : (marking * label * marking) list ref = ref [] in
  let enqueue m =
    let normalize = List.map Place.of_string (normalize_marking m) in
    let k = marking_key normalize in
    if not (Hashtbl.mem visited k) then begin
      Hashtbl.add visited k ();
      Queue.push normalize queue
    end
  in
  let init = normalize_marking mn.marking |> List.map Place.of_string in
  enqueue init;
  while not (Queue.is_empty queue)  do
    let m    = Queue.pop queue in
    let mn  = { mn with marking = m} in
    let enabled_tr = enabled_transitions mn in
    Printf.printf "Marking: %s, enabled transitions: %d\n" 
      (marking_key m) (List.length enabled_tr);
    List.iter (fun t ->
      match fire mn t with
      | None     -> ()
      | Some mn' ->
          let m' = List.map Place.of_string (normalize_marking mn'.marking) in
          let lab = mn.net.label_map t in
          edges := (m, lab, m') :: !edges;
          enqueue m';
    ) enabled_tr;
  done;
  List.rev !edges

let reachable_markings (mn : marked_net) : marking list =
  let init = mn.marking in
  [init] @ List.map (fun (_,_,x) -> x) (marking_graph mn)

let ccs_net (ln : labelled_net) : bool =
  List.for_all (fun x ->
    let len_pre = List.length (preset_of_transition ln x) in
    let lam_t = ln.label_map x in
    len_pre <= 2 && ( not (len_pre = 2) || lam_t = "tau")
  ) ln.transitions

let is_safe (mn : marked_net) : bool =
  let visited = Hashtbl.create 1024 in
  let queue = Queue.create () in
  let safe = ref true in
  let queue_marking m =
    let key = marking_key (normalize_marking m |> List.map (Place.of_string)) in
    if not (Hashtbl.mem visited key) then 
      begin
        Hashtbl.add visited key ();
        Queue.push (normalize_marking m) queue
      end
  in
  queue_marking mn.marking;

  while not (Queue.is_empty queue) && !safe do
    let m = Queue.pop queue in
    let counts = Hashtbl.create 16 in
    List.iter (fun pid ->
      let c = try Hashtbl.find counts pid with Not_found -> 0 in
      Hashtbl.replace counts pid (c + 1)
    ) m;
    Hashtbl.iter (fun _ cnt -> if cnt > 1 then safe := false) counts;
    if !safe then
      let cur = { mn with marking = m |> List.map (Place.of_string) } in
      List.iter (fun t ->
        match fire cur t with
        | None -> ()
        | Some mn' ->
            let m' = normalize_marking mn'.marking |> List.map (Place.of_string) in
            queue_marking m'
      ) (enabled_transitions cur)
  done;
  !safe


(** ------- Pretty-print ----------------- *)

let print_preset_postset net =
  print_endline "Preset and Postset of transitions";
  List.iter (fun t ->
    let pre  = preset_of_transition  net t |> List.map (Place.to_string) in
    let post = postset_of_transition net t |> List.map (Place.to_string) in
    Printf.printf "  •%s = { %s }\n"  (Transition.to_string t) (String.concat ", " pre);
    Printf.printf "   %s• = { %s }\n" (Transition.to_string t) (String.concat ", " post)
  ) net.transitions;
  print_endline "Preset and Postset of places";
  List.iter (fun p ->
    let pre  = preset_of_place  net p |> List.map (Transition.to_string) in
    let post = postset_of_place net p |> List.map (Transition.to_string) in
    Printf.printf "  •%s = { %s }\n"  (Place.to_string p) (String.concat ", " pre);
    Printf.printf "   %s• = { %s }\n" (Place.to_string p) (String.concat ", " post)
  ) net.places

let print_marking net =
  print_endline "Current marking:";
  List.iter (fun p ->
    Printf.printf "  %-15s : %d token(s)\n" p (tokens net (Place.of_string p))
  ) (net.net.places |> List.map (Place.to_string))

let print_enabled net =
  let ts = enabled_transitions net |> List.map (Transition.to_string) in
  if ts = [] then
    print_endline "  (without enabled transitions)"
  else
    List.iter (fun t ->
      Printf.printf "  [%s] %s\n" t t
    ) ts

(** ----------- Examples ---------------- *)

(** example1 *)

let s1 = make_place "s1"
let s2 = make_place "s2"
let s3 = make_place "s3"
let s4 = make_place "s4"

let t1 = make_transition "t1"
let t2 = make_transition "t2"
let t3 = make_transition "t3"
let t4 = make_transition "t4"

let arcs1 = [
  PT (s1, t1); TP (t1, s1);
  PT (s1, t2);
  TP (t1, s2); TP (t2, s3);
  PT (s3, t4); PT (s2, t4); 
  PT (s2, t3); TP (t4, s1);
  TP (t3, s4); TP (t4, s4)
]

let set = make_label_list ["a"; "b"; "tau"]

let lambda1 : transition -> label = function t ->
  match t with
  | t1 -> "a"
  | t2 -> "b"
  | t3 -> "b"
  | t4 -> "tau"

let label_trans = fun x -> lambda1 x

let net1 = make_label_net [s1; s2; s3; s4] [t1; t2; t3; t4] arcs1 set label_trans

let init_marking = generate_marking ["s1"]

let mnet1 = make_marked_net net1 init_marking

(** example 2 *)
(* using the construction for places and transition in example 1 *)
let arcs2 = [
  PT (s1, t1); TP (t1, s1);
  PT (s1, t2);
  TP (t2, s2); ]

let lambda2 : transition -> label  = function t ->
  match t with
  | t1 -> "a"
  | t2 -> "b"

let label_trans2 = fun x -> lambda2 x

let net2 = make_label_net [s1; s2] [t1; t2] arcs2 ["a";"b"] label_trans

let init_marking2 = generate_marking ["s1"]

let mnet2 = make_marked_net net2 init_marking2



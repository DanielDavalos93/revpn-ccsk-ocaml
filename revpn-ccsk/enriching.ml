open Net
open Lts
open Util

(** {b Token}

   [token] is the set of tokens defined inductively as:
  - [Tok_empty]: {m (\cdot, \langle\!\langle \rangle\!\rangle, \cdot)}
  - [Tok (a, ls, i)]: if [ls] has [token] type, {m (a, \langle\!\langle w_1,\dots,w_n\rangle\!\rangle, i)} with {m a} an action, {m w_1,\dots,w_n} are tokens and {m i\in\mathbb N} an identificator number.
  *)
type token = 
  | Tok_empty
  | Tok of (label * token list * int)

let rec size (tk : token) : int =
  match tk with
  | Tok_empty -> 0
  | Tok (_, tok, _) -> 
      List.fold_left (max) 0 (List.map (size) tok) + 1

(** {b Sum of tokens.} If {m t_1 = (a_1,w_1,i_1)} and 
    {m t_2 = (a_1, w_2, i_2)} then {m t_1\oplus t_2 = 
      (a_1,w_1,i_1);(a_2,w_2,i_2)} is the concatenation of tokens.
*)
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


(** ---- Example ---- *)

(** {m w_0 = (\cdot, \langle\!\langle \rangle\!\rangle)} *)
let w0 = Tok_empty                        (* size w0 -> 0 *)

(** {m w_1 = (a, (\cdot, \langle\!\langle \rangle\!\rangle), 1)} *)
let w1 = Tok ("a", [Tok_empty], 1)        (* size w1 -> 1 *)

(** {m w_2 = (b, (\cdot, \langle\!\langle \rangle\!\rangle), 1)} *)
let w2 = Tok ("b", [Tok_empty], 1)        (* size w2 -> 1 *)

(** {m w_3 = (a, \langle\!\langle (\cdot \langle\!\langle \rangle\!\rangle), 
    (b, (\cdot, \langle\!\langle \rangle\!\rangle), 1)\rangle\!\rangle, 2)} *)
let w3 = Tok ("a", [Tok_empty;
          Tok ("b", [Tok_empty], 1)], 2)  (* size w3 -> 2 *)

(** {m w_4 = (\tau, \langle\!\langle(a, \langle\!\langle (\cdot \langle\!\langle \rangle\!\rangle), 
    (b, (\cdot, \langle\!\langle \rangle\!\rangle), 1)\rangle\!\rangle, 2)\rangle\!\rangle, 3)} *)
let w4 = Tok ("tau", [w3], 3)  (* size w4 -> 2 *)

(* --------------------- *)

(** To take into account the new notion of tokens, we have to reformulate
    the notions for semantics. [token_net] is the type for nets where the 
    markings have type [token].
 *)

type token_net = {
  net : labelled_net;
  marking : token;
}

let make_token_net (net : labelled_net) (tok_marking : token) : token_net = {
  net = net; marking = tok_marking;
}

(** [m_flat marking] converts a marking into a flat token whose children
    are one [Tok(p, [Tok_empty], i)] per place, numbered left to right.

      m_flat []           = Tok_empty
      m_flat [p1;…;pn]   = Tok ("·", [Tok(p1,[Tok_empty],1);
                                        Tok(p2,[Tok_empty],2);
                                        …
                                        Tok(pn,[Tok_empty],n)], n+1)  *)
(* let rec m_flat (ln : labelled_net) (tid : transition_id) : token = *)
(*   let label_a = (ln.label_map {t_id = tid; t_label = ""}).t_label in *)
(*   let pres_t = input_arcs ln tid in *)
(*   match pres_t with *)
(*   | []  -> Tok_empty *)
(*   | [p] -> Tok (label_a, [Tok_empty], encode_string label_a) *)
(*   | ps  -> *)
(*       let children = *)
(*         List.mapi (fun i p -> Tok (label_a, [Tok_empty], i)) ps in *)
(*       let top_id = encode_string label_a in *)
(*       Tok (label_a, children, top_id) *)

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

(** {b Immediate dependency.} 
    Two transitions {m t_1, t_2} are in {i immediate dependency} if 
    there is a place {m a \in P} such that 
    {m (t_1, a) \in R \wedge (a, t_2) \in R}.
*)
let immediate_transitions (ln : labelled_net) : (transition_id * transition_id) list =
  let plSet = get_place ln in
  let trSet = get_transition ln in
  let binProd = bin_prod trSet trSet in
  List.filter (fun pair ->
    (List.exists (fun x ->
      List.mem (TP (fst pair, x)) ln.arcs && List.mem (PT (x, snd pair)) ln.arcs
    ) plSet)
  ) binProd
  
(* let marking_tok (ln : labelled_net) (m : marking) : token list = *)
(*   ln.places |> List.mapi (fun i p -> (i+1, p)) |> *)
(*   List.filter_map (fun (i, p) -> *)
(**)
(*     if List.mem p.p_id m then Some (Tok (p.p_id, [Tok_empty], i)) *)
(*     else None *)
(*   ) *)

(** [is_enabled_tk tn tid] checks if transition [tid] is enabled
    in the token net [tn].

    A transition [tid] with input places [p1,…,pk] is enabled iff
    the current marking token has label in {p1,…,pk}, i.e., the
    token currently sits in one of the input places. *)
let is_enabled_tk (tn : token_net) (tid : transition_id) : bool =
  let inputs = input_arcs tn.net tid in
  match tn.marking with
  | Tok_empty -> false
  | Tok (a, t, _) -> 
      (* let lamb_t = (tn.net.label_map {t_id = tid; t_label = ""}).t_label in *)
      List.mem a inputs

(** [next_id tk] returns a fresh identifier = size of the token + 1. *)
let next_id (tk : token) : int = size tk + 1

(** [fire_tk tn tid] fires transition [tid] if enabled.

    Firing [t] with:
      - current token  w = Tok (p, history, i)  in input place [p]
      - output places  [q1, …, qk]

    Produces one new token per output place, each wrapping [w]:
      {m w'_j = \mathsf{Tok} (q_j, \mathtt{w}, \mathtt{next_id w}}
    Since the marking is a single token, firing [t] with one output
    place produces [Tok (q, [w], i+1)].
    For transitions with multiple output places (like [t4] in Figure 3)
    we produce a combined token whose children are one token per output:
      {m w' = \mathsf{Tok} (tid, [\mathsf{Tok} (q_1,\mathtt{w},i+1); Tok(q_2,\mathtt{w},i+2)], i+1)}

    Returns [None] if [tid] is not enabled. *)
let fire_tk (tn : token_net) (tid : transition_id) : token_net option =
  if not (is_enabled_tk tn tid) then None
  else
    let outputs = output_arcs tn.net tid in
    let w       = tn.marking in
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
    Some { tn with marking = new_token }

(** [unfire_tk tn tid] reverses the firing of [tid].
    If the current token is [Tok (tid, [w], _)], returns [w].
    Returns [None] if the current token was not produced by [tid]. *)
let unfire_tk (tn : token_net) (tid : transition_id) : token_net option =
  match tn.marking with
  | Tok_empty -> None
  | Tok (label, children, _) ->
      let outputs = output_arcs tn.net tid in
      if label <> tid && not (List.mem label outputs) then None
      else
        match children with
        | [w] ->
            Some { tn with marking = w }
        | _ ->
           let inputs = input_arcs tn.net tid in
            begin match List.find_opt
              (fun w -> match w with
                | Tok (a,_,_) -> List.mem a inputs
                | Tok_empty   -> false)
              children
            with
            | Some w -> Some { tn with marking = w }
            | None   -> None
            end

(** [firing_sequence_tk tn ts] fires all transitions in [ts] in order.
    Returns [Some tn'] if every step succeeds, [None] at first failure. *)
let rec firing_sequence_tk (tn : token_net) (ts : transition_id list) : token_net option =
  match ts with
  | []      -> Some tn
  | t :: ts ->
      match fire_tk tn t with
      | None     -> None
      | Some tn' -> firing_sequence_tk tn' ts

(** {b Key Labelled Net}

  A net {m K = (N, S_k)} is called {b key} labelled net (or key net) if 
    for every {m t \in T}, {m |t^{\bullet} \cap S_k| = 1}, and for all 
  {m s \in S_k}, such that {m |s^{\bullet}}|=0 \wedge |^{\bullet} s|=1.}
*)
let key_net (tn : marked_net) : bool =
    List.for_all (fun t ->
      let post_t = output_arcs tn.net t in
      List.length (intersect post_t tn.marking) = 1
    ) (get_transition tn.net) &&
    List.for_all (fun s ->
      let pre_s = List.map (fun x -> x.t_id) (preset_of_place tn.net s) in
      let post_s = List.map (fun x -> x.t_id) (postset_of_place tn.net s) in
      List.length pre_s = 1 && List.length post_s = 0
    ) tn.marking

(** Example
 Using [net1] from [Net.ml] we define a token net with marking *)
(* let tn1 = *)

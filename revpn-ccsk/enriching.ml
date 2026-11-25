open Net
open Util
open Token

type net_system = {
  net : labelled_net;
  m : markingTok;
}
(** A {i net system} is a pair {m (N, m)} with {m N} labelled net
  and the marking {m m} is a mapping associating to each place a multiset 
  of tokens, i.e. {m m : S \to \mu\textsf{Tok}}. *)

let make_net_system (net : labelled_net) (m : markingTok) : net_system = {
  net = net; 
  m = m;
}

let immediate_transitions (ln : labelled_net) : (transition * transition) list =
  let plSet = get_place ln in
  let trSet = get_transition ln in
  let binProd = bin_prod trSet trSet in
  List.filter (fun pair ->
    (List.exists (fun x ->
      List.mem (TP (fst pair, x)) ln.arcs && List.mem (PT (x, snd pair)) ln.arcs
    ) plSet)
  ) binProd
  (** {b Immediate dependency.} 
      Two transitions {m t_1, t_2} are in {i immediate dependency} if 
      there is a place {m a \in P} such that 
      {m (t_1, a) \in R \wedge (a, t_2) \in R}.
  *)


(* Enabling and Firing for tokens *)
let is_enabled_tk (ns : net_system) (tid : transition) : bool =
  let inputs = input_arcs ns.net tid in
  List.for_all (fun p -> tokens_in ns.m p > 0) inputs
  (** [is_enabled_tk tn tid] checks if transition [tid] is enabled
    in the token net [tn].

    A transition [tid] with input places [p1,…,pk] is enabled iff
    the current marking token has label in [p1,…,pk], i.e., the
    token currently sits in one of the input places. *)

let next_id (w : Token.t) : int = Token.size w + 1
  (** [next_id tk] returns a fresh [identifier = size of the token + 1]. *)

let unopt_net_system (mso : net_system option) : net_system =
  match mso with
  | None -> 
    let empty_label_net = make_label_net [] [] [] [] (fun x -> "") in
    {net = empty_label_net; m = empty_marking}
  | Some m -> m

let fire_tk (ns : net_system) (m : markingTok) (tid : transition) : net_system option =
  let pre  = input_arcs  ns.net tid in            (* •t *)
  let post = output_arcs ns.net tid in            (* t• *)
  let mem p l = List.exists (fun q -> Place.compare p q = 0) l in
  (* w_{s_i}: the token of m used in s_i ∈ •t (exactly one) *)
  let token_in p =
    match Multiset.to_list (marking_of m p) with
    | [ w ] -> Some w
    | _ -> None in
  let used = List.map (fun p -> (p, token_in p)) pre in
  let w_of s =                                    (* Some w_s  iff  s ∈ •t *)
    match List.find_opt (fun (p, _) -> Place.compare p s = 0) used with
    | Some (_, w) -> w
    | None -> None in
    (* w = (λ(t), <<w_s1,…,w_sn>>, i) *)
    let ws = List.filter_map snd used in
    let i  = List.fold_left (fun acc w -> max acc (next_id w)) 1 ws in
    let w  = Token.Tok (ns.net.label_map tid, ws, i) in
    (* m'(s) for the places that change: •t ∪ t• *)
    let affected = pre @ List.filter (fun s -> not (mem s pre)) post in
    let step acc s =
      Option.bind acc (fun mm ->
        let ms = marking_of ns.m s in
        let ms' =
          match w_of s, mem s post with
          | Some w_s, true  ->                    (* s ∈ •t ∩ t• : m(s) − w_s + w *)
              Option.map (Multiset.add w) (Multiset.remove w_s ms)
          | Some w_s, false ->                    (* s ∈ •t \ t• : m(s) − w_s *)
              Multiset.remove w_s ms
          | None, true ->                         (* s ∈ t• \ •t : m(s) + w *)
              Some (Multiset.add w ms)
          | None, false ->                        (* otherwise    : m(s) *)
              Some ms in
        Option.map (fun r -> set_place s r mm) ms') in
    Option.map (fun m' -> { ns with m = m' })
      (List.fold_left step (Some ns.m) affected)
(** [fire_tk ns m tid] fires [tid] consuming the tokens [m ⊆ ns.m].

    With [w_s] the token of [m] in [s ∈ •t] and
    {m w = (\lambda(t), \langle\!\langle w_{s_1},\dots,w_{s_n}\rangle\!\rangle, i)}:
    {math m'(s) = \begin{cases}
      m(s) - w_s + w & s \in {}^\bullet t \cap t^\bullet \\
      m(s) - w_s     & s \in {}^\bullet t \setminus t^\bullet \\
      m(s) + w       & s \in t^\bullet \setminus {}^\bullet t \\
      m(s)           & \text{otherwise}
    \end{cases}}
    Returns [None] if [m] is not a valid choice of tokens for [t]. *)


let rec firing_sequence_tk (ns : net_system) (ms : markingTok list) (ts : transition list) : net_system option =
  match ms, ts with
  | [], []      -> Some ns
  | m :: ms, t :: [] -> fire_tk ns m t
  | m :: ms, t :: ts ->
      if List.length ms == List.length ts then
        match fire_tk ns m t with
        | None     -> None
        | Some ns' -> firing_sequence_tk ns' ms ts
      else
        None
  | _, _    -> None
  (** [firing_sequence_tk tn ts] fires all transitions in [ts] in order.
    Returns [Some tn'] if every step succeeds, [None] at first failure. *)

let is_initial (n : net_system) (m : markingTok) : bool =
  List.for_all (fun s -> 
    is_subset ((marking_of m s) |> Multiset.to_list) [Tok_empty]
    ) n.net.places


type keyPairNet = {
  net : labelled_net;
  key : place list;
  }

let is_key_net (kn : keyPairNet) : bool =
    List.for_all (fun t ->
      let post_t = output_arcs kn.net t in
      List.length (intersect post_t kn.key) = 1
    ) (get_transition kn.net) &&
    List.for_all (fun s ->
      let pre_s = List.map (fun x -> x) (preset_of_place kn.net s) in
      let post_s = List.map (fun x -> x) (postset_of_place kn.net s) in
      List.length pre_s = 1 && List.length post_s = 0
    ) kn.key
(** {b Key Labelled Net.} A net {m K = (N, S_k)} is called {b key labelled net} 
  (or key net) if for every {m t \in T}, {m |t^{\bullet} \cap S_k| = 1}, and for all 
  {m s \in S_k}, such that {m |s^{\bullet}|=0 \wedge |{}^{\bullet} s|=1.}
*)

let keypl (kn : keyPairNet) (t : transition) : place list =
  let sk = kn.key in
  List.filter (fun x -> 
    (List.map (fun y -> y) (preset_of_place kn.net x)) = [t]) sk
(** Let [kn = (N, S_k)] be a key labelled net and let [t] be a transition.
    Then with {m s^t} we denote the {b key place} {m s^t \in S_k} such 
    that {m {}^{\bullet} s^t = \{t\}}. The key place associated to a 
    transition [t] is denoted with {m \mathsf{keypl}(t)}. 
    So, [keypl kn t] returns the key place associated to [t]. *)

let is_key_initial (kn : keyPairNet) (m : markingTok) : bool =
  let ns = make_net_system kn.net m in
  is_initial ns m && 
  List.for_all (fun s -> 
    (marking_of m s) = Multiset.empty
    ) kn.key
    (** We say that {m m} is {b key initial} in a key net iff (a) {m \forall s \in S, [\![ m(s) ]\!] \subseteq \{(\cdot,\langle\!\langle \rangle\!\rangle, \cdot)\}} and (b) {m \forall s \in S_k, m(s) = 0}. *)

(* --- Reversing nets --- *)
module ReversingNet = struct
(** Formally a {i reversible labelled net} is a pair {m R=(N,S_k,U)}, where
    {m \mathsf{K} = (\langle S, T, F, \lambda, \mathsf{A}\rangle, S_k)} is 
    a key net, and {m U} is the set of reversing transitions with 
    {m U \subseteq T}.
    We define [reversing_net] as a pair [{key_net = kn; rev_trans = rt}] 
    being [kn] the key net {m \mathsf{K}} and [rt] the reversing transitions
    set {m U}.
*)
  type reversing_net = {
    key_net : keyPairNet;
    rev_trans : transition list;
  }

  let forwardTransition (rn : reversing_net) : transition list =
    setminus rn.key_net.net.transitions rn.rev_trans

  (** [forwardKeyNet rn]: for every a reversible net [rn] equals to 
    {m (N, S_k, U)} with {m N = \langle S, T, F, \lambda, \mathsf{A}\rangle} 
    returns the forward key net {m (\langle S, T, F_f, \lambda\rangle, S_k)} 
    where {m F_f} suprimes all the arcs {m (s, u)} and {m (u, s)} with 
    {m u \in U} over {m F}. *)
  let forwardKeyNet (rn : reversing_net) : keyPairNet = 
    let fwdT = forwardTransition rn in
    let fwd_arcs = List.filter (fun x ->
          (List.mem (pi_t x Fst) (List.map (fun x -> x) fwdT)) ||
          (List.mem (pi_t x Snd) (List.map (fun x -> x) fwdT)) 
          ) rn.key_net.net.arcs in
    {
    net = {
      places = rn.key_net.net.places;
      transitions = fwdT;
      arcs = fwd_arcs;
      set = rn.key_net.net.set;
      label_map = rn.key_net.net.label_map;
      };
      key = rn.key_net.key;
    }

  (** Let [rn] be a reversing net {m R =(\langle S, T, F, \lambda, \mathsf{A}\rangle, S_k, U)}. 
    For each forward transition [t] in [T] and backward transition [u] in [U], 
    [prop_reverse_transition rn u t] is [true] if 
    {math ({}^{\bullet}u=t^{\bullet}) \wedge (u^{\bullet}={}^{\bullet}t) \wedge (\lambda (t) = \lambda (u)).} *)
  let prop_reverse_transition (rn : reversing_net) : transition -> transition -> bool =
    let lnet = rn.key_net.net in
    fun u ->
      fun t -> 
        (preset_of_transition lnet u = postset_of_transition lnet t) &&
        (postset_of_transition lnet u = preset_of_transition lnet t) &&
        (lnet.label_map t = lnet.label_map u)

  (** [is_reversible_labelled_net] take a reversing net {m R =(\langle S, T, F, \lambda, \mathsf{A}\rangle, S_k, U)} as [rn] and returns true 
      if it satisfies the following statements:
        + [rn.key_net.net] has type [labelled_net];
        + If we write {m (\langle S, T \setminus U, F \setminus (\{(s, u) | s \in S, u \in U \} \cup \{(u, s) | s \in S, u \in U \}), \lambda, \mathsf{A}\rangle, S_k)} as [kn'], then [is_key_net kn'] is [true]; and
        + {m U \subseteq T} and {m \forall} [u]{m \in U, \exists!} [t]{m \in T\setminus U}, [prop_reverse_transition rn u t].
 *)
  let is_reversible_labelled_net (rn : reversing_net) : bool =
    let fwdT = forwardTransition rn in
    is_key_net (forwardKeyNet rn) &&
    is_subset rn.rev_trans rn.key_net.net.transitions &&
    List.for_all (fun u ->
      List.exists (fun t ->
      exists_unique fwdT (prop_reverse_transition rn u) t
      ) fwdT
    ) rn.rev_trans

  (** When [rn : reversing_net] satisfies the [is_reversible_labelled_net] def.,
      then [reverse_transition rn t] returns the transition [Some t'], such that 
     {math ({}^{\bullet}t=t'^{\bullet}) \wedge (t^{\bullet}={}^{\bullet}t') \wedge (\lambda (t) = \lambda (t')).}

     Of course, this condition is allways possible because of the third reversible 
     condition. In otherwise, [reverse_transition rn t] returns [None].
     In this function, don't mind if [t] is forward or backward transition.
  *)
  let reverse_transition (rn : reversing_net) (t : transition) =
    let fwdT = forwardTransition rn in
    let bwdT = rn.rev_trans in
    if is_reversible_labelled_net rn then
      if List.mem t (rn.rev_trans) then
        (List.filter (fun t' -> prop_reverse_transition rn t t') fwdT)
      else
        (List.filter (fun t' -> prop_reverse_transition rn t t') bwdT)
    else
      []
    

(** Let {m R = (\langle S, T, F, \lambda, A\rangle, S_k, U)} be a reversible labelled net 
    and let {m u \in U} be a reversing transition, and {m t \in T \setminus U} the 
    corresponding forward transition. Then [u] is {b enabled at the marking} [m] whenever 
    {math 
    \begin{align*}
    &\forall s \in {}^{\bullet}u,\\
    & \quad \exists w \in m(s), \mathsf{label}(w) = \lambda(t) \wedge \\
    & \qquad \forall w'\in \lfloor w \rfloor,\\
    & \quad \qquad \exists s' \in u^{\bullet},\\
    & \qquad \qquad \exists t' ∈ {}^{\bullet}s, 
        w′ \in m(\mathsf{keypl}(t)) \vee w′ = (\cdot, \langle\!\langle \rangle\!\rangle, ·)
    \end{align*}
    }
*)
  let is_enabled (rn : reversing_net) (m : markingTok) (u : transition) =
    let rt = !! (reverse_transition rn u) 0 in
    let net = rn.key_net.net in
    List.mem u rn.rev_trans && 
    List.for_all (fun s ->
      List.exists (fun w -> label w = net.label_map rt &&
      let ws' = List.map (fun ws -> Multiset.of_list [ws]) (toks w) in
        List.for_all (fun w' ->
          List.exists (fun s' ->
            let m_keypl_t = fun t' -> List.map (fun p -> marking_of m p) (keypl rn.key_net t') in
            List.exists (fun t' -> List.mem w' (m_keypl_t t') || w' = Multiset.of_list [Tok_empty]
            ) (preset_of_place net s')
          ) (preset_of_transition net u)
        ) ws'
      ) (marking_of m s |> Multiset.to_list)
    ) (preset_of_transition net u)

end

type token = Tok_empty | Tok of (string * token list * int)
(**
  Let {m N = \langle S, T, F, \lambda, \mathsf{A} \rangle} be a labelled net.
   [token] is the type of tokens defined inductively as:
     {math \cfrac{}{\mathsf{Tok\_empty : token}} \qquad\qquad 
     \cfrac{a\in\mathsf{A} \qquad ls\mathsf{ : [token]} \qquad i \in \mathbb N}{\mathsf{Tok}(a, ls, i)\mathsf{ : token}}}

  - [Tok_empty]: {m (\cdot, \langle\!\langle \rangle\!\rangle, \cdot)}
  - [Tok (a, ls, i)]: if [ls] has [token] type, 
    {m (a, \langle\!\langle w_1,\dots,w_n\rangle\!\rangle, i)} with {m a} 
    an action, {m w_1,\dots,w_n} are tokens and {m i\in\mathbb N} an identificator number.
  *)

val size : token -> int
    (** The {i size} of a token is defined as follows: 
    {m \mathsf{size}((\cdot,\langle\!\langle \rangle\!\rangle,\cdot))=0} and
    {m \mathsf{size}((a,\langle\!\langle w_1,\dots, w_n \rangle\!\rangle,j))=
        \max\{\mathsf{size(w_i) \mid 1\le i \le n}\} + 1}.

    *)

val ( +. ) : token -> token -> token list
    (** {b Sum of tokens.} If {m t_1 = (a_1,w_1,i_1)} and 
        {m t_2 = (a_1, w_2, i_2)} then {m t_1+_{\bullet} t_2 = 
        (a_1,w_1,i_1);(a_2,w_2,i_2)} is the concatenation of tokens.
    *)

val kt : token -> int
(** Given a token [w = (a, w', i)] we define: [kt w = i], [lab_tok w = a]
and [toks w = w']. *)

val lab_tok : token -> string
val toks : token -> token list
val ( -. ) : token -> token -> token
(** For a token {m w = (a,\langle\!\langle w_1,\dots,w_{j-1},w_j,w_{j+1}\rangle\!\rangle, i)}, 
    we define {m w -_{\bullet} w_j} as
   {math (a,\langle\!\langle w_1,\dots,w_{j-1},w_{j+1}\rangle\!\rangle, i)}. *)

val w0 : token
  (** ---- Example ---- *)

  (** {m w_0 = (\cdot, \langle\!\langle \rangle\!\rangle, \cdot)} *)

val w1 : token
  (** {m w_1 = (a, (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 1)} *)

val w2 : token
  (** {m w_2 = (b, \langle\!\langle (\cdot, \langle\!\langle \rangle\!\rangle, \cdot)\rangle\!\rangle, 1)} *)

val w3 : token
  (** {m w_3 = (a, \langle\!\langle (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 
    (b, (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 1)\rangle\!\rangle, 2)} *)

val w4 : token
  (** {m w_4 = (\tau, \langle\!\langle(a, \langle\!\langle (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 
    (b, \langle\!\langle(\cdot, \langle\!\langle \rangle\!\rangle, \cdot)\rangle\!\rangle, 1)\rangle\!\rangle, 2)\rangle\!\rangle, 3)} *)

type token_net = { net : Net.labelled_net; key : token; }
(** --------------------- *)

  (** To take into account the new notion of tokens, we have to reformulate
    the notions for semantics. [token_net] is the type for nets where the 
    markings have type [token].
  *)

val make_token_net : Net.labelled_net -> token -> token_net
val immediate_transitions : Net.labelled_net -> (string * string) list
  (** {b Immediate dependency.} 
      Two transitions [t_1, t_2] are in {i immediate dependency} if 
      there is a place {m a \in P} such that 
      {m (t_1, a) \in R \wedge (a, t_2) \in R}.
  *)

val m_flat : Net.labelled_net -> Net.marking -> token
(** [m_flat marking] converts a marking into a flat token whose children
    are one [Tok(p, [Tok_empty], i)] per place, numbered left to right.

      [m_flat []           = Tok_empty]
      
      [m_flat [p1;…;pn]   = Tok ("·", [Tok(p1,[Tok_empty],1);]
                                        [Tok(p2,[Tok_empty],2);]
                                        [...]
                                        [Tok(pn,[Tok_empty],n)], n+1)]  *)

val is_enabled_tk : token_net -> string -> bool
  (** [is_enabled_tk tn tid] checks if transition [tid] is enabled
    in the token net [tn].

    A transition [tid] with input places [p1,…,pk] is enabled iff
    the current marking token has label in {p1,…,pk}, i.e., the
    token currently sits in one of the input places. *)

val next_id : token -> int
  (** [next_id tk] returns a fresh [identifier = size of the token + 1]. *)

val fire_tk : token_net -> string -> token_net option
  (** [fire_tk tn tid] fires transition [tid] if enabled.

    Firing [t] with:
      - current token  w = Tok (p, history, i)  in input place [p]
      - output places  [q1, …, qk]

    Produces one new token per output place, each wrapping [w]:
      {m w'_j = \mathsf{Tok} (q_j, \mathsf{w}, \mathsf{next_id w})}
    Since the marking is a single token, firing [t] with one output
    place produces [Tok (q, [w], i+1)].
    For transitions with multiple output places (like [t4] in Figure 3)
    we produce a combined token whose children are one token per output:
      {m w' = \mathsf{Tok} (tid, [\mathsf{Tok} (q_1,\mathsf{w},i+1); \mathsf{Tok}(q_2,\mathsf{w},i+2)], i+1)}

    Returns [None] if [tid] is not enabled. *)

val unfire_tk : token_net -> string -> token_net option
  (** [unfire_tk tn tid] reverses the firing of [tid].
    If the current token is [Tok (tid, [w], i)], returns [Some w].
    Returns [None] if the current token was not produced by [tid]. *)

val firing_sequence_tk : token_net -> string list -> token_net option
  (** [firing_sequence_tk tn ts] fires all transitions in [ts] in order.
    Returns [Some tn'] if every step succeeds, [None] at first failure. *)

type keyPairNet = { net : Net.labelled_net; key : string list; }
(** {b Key Labelled Net.} A net {m K = (N, S_k)} is called {b key labelled net} 
  (or key net) if for every {m t \in T}, {m |t^{\bullet} \cap S_k| = 1}, and for all 
  {m s \in S_k}, such that {m |s^{\bullet}|=0 \wedge |{}^{\bullet} s|=1.}
*)

val is_key_net : keyPairNet -> bool

val key3 : Net.place list
(** Example
 Using [net1] from [Net.ml] we define a token net with marking *)

val pl3 : Net.place list
val arcs3 : Net.arc list
val net3 : Net.labelled_net
val knet3 : keyPairNet
val keypl : keyPairNet -> string -> string list
  (** {b Key place.} Given a key labelled net {m \mathsf{K} = (N, S_k)} and
      a transition {m t\in T}, the {i key place} [keypl k t] return the place
      {m s^t\in S_k} such that {m {}^{\bullet}s^t = \{t\}.}
  *)

type reversing_net = {
  net : Net.labelled_net;
  key : string list;
  rev_trans : Net.transition list;
}
(** {1 Reversible labelled net} 
  
  A {b reversible labelled net} with resersing transitions {m U\subseteq T}
  (we denote this as [rev_trans] in the constructor of the type 
  [reversing_net]) and key places {m S_k \subseteq S} is the tripe 
  {m R = (\langle S, T, F, \lambda, A\rangle, S_k, U)}.
*)

val forwardTransition : reversing_net -> Net.transition list
  (** [forwardNet] take a [reversing_net] and return a [keyPairNet] removing
      their backward transitions and all their arcs containing reversing 
      transitions.
   *)

val forwardNet : reversing_net -> keyPairNet
val prop_reverse_transition :
  reversing_net -> Net.transition -> Net.transition -> bool
val is_reversible_lab_net : reversing_net -> bool

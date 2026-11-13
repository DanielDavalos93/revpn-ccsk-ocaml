open Lts
open Net

module Token = struct
  type t =
    | Tok_empty
    | Tok of (label * t list * int)
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

  let rec size (tk : t) : int =
    match tk with
    | Tok_empty -> 0
    | Tok (_, ws, _) -> 
      List.fold_left (fun m w -> max m (size w)) 0 ws + 1
    (** The {i size} of a token is defined as follows: 
    {m \mathsf{size}((\cdot,\langle\!\langle \rangle\!\rangle,\cdot))=0} and
    {m \mathsf{size}((a,\langle\!\langle w_1,\dots, w_n \rangle\!\rangle,j))=
        \max\{\mathsf{size(w_i) \mid 1\le i \le n}\} + 1}.

    *)

  let compare : t -> t -> int = Stdlib.compare

  let equal a b = compare a b = 0
end

(* --- μTok: finite multisets of tokens --- *)
module TokMap = Map.Make (Token)

module Multiset = struct
  type t = int TokMap.t                   (* token ↦ multiplicity (> 0) *)

  let empty : t = TokMap.empty
  let is_empty = TokMap.is_empty
  let count w (s : t) = Option.value ~default:0 (TokMap.find_opt w s)

  let add ?(n = 1) w (s : t) : t =
    if n <= 0 then s else TokMap.add w (count w s + n) s

  let remove ?(n = 1) w (s : t) : t option =
    let c = count w s in
    if c < n then None
    else if c = n then Some (TokMap.remove w s)
    else Some (TokMap.add w (c - n) s)

  let union (a : t) (b : t) : t =                       (* a ⊎ b *)
    TokMap.union (fun _ i j -> Some (i + j)) a b

  let diff (a : t) (b : t) : t option =                 (* a ⊖ b, if b ⊆ a *)
    TokMap.fold (fun w n acc -> Option.bind acc (remove ~n w)) b (Some a)

  let subset (a : t) (b : t) = TokMap.for_all (fun w n -> n <= count w b) a
  let cardinal (s : t) = TokMap.fold (fun _ n acc -> n + acc) s 0
  let equal = TokMap.equal Int.equal
  let compare = TokMap.compare Int.compare

  let singleton w = add w empty
  let of_list l = List.fold_left (fun s w -> add w s) empty l
  let to_list (s : t) =
    TokMap.fold (fun w n acc -> List.init n (fun _ -> w) @ acc) s []
end

(* ---------- m : S -> μTok ---------- *)
module PMap = Map.Make (Place)             (* keys: place_id *)

type markingTok = Multiset.t PMap.t            
(** replaces transition_id list *)

let empty_marking : markingTok = PMap.empty

(** m(p), total: absent places are mapped to ∅ *)
let marking_of (m : markingTok) (p : place) : Multiset.t =
  Option.value ~default:Multiset.empty (PMap.find_opt p m)

(** canonical form: never store p ↦ ∅ *)
let set_place p ms (m : markingTok) : markingTok =
  if Multiset.is_empty ms then PMap.remove p m else PMap.add p ms m

let add_token p w (m : markingTok) : markingTok =
  set_place p (Multiset.add w (marking_of m p)) m

let unopt_tk (mop : markingTok option) : markingTok =
  match mop with
  | None -> empty_marking
  | Some m -> m

let remove_token p w (m : markingTok) : markingTok option =
  Option.map (fun ms -> set_place p ms m)
    (Multiset.remove w (marking_of m p))

let make_marking (l : (place * Token.t list) list) : markingTok =
  List.fold_left
    (fun m (p, ws) -> List.fold_left (fun m w -> add_token p w m) m ws)
    empty_marking l

let marking_equal   : markingTok -> markingTok -> bool = PMap.equal Multiset.equal
let marking_compare : markingTok -> markingTok -> int  = PMap.compare Multiset.compare

(* m1 u m2 : disjoint multiset sum *)
(* let marking_union (a : markingTok) (b : markingTok) : markingTok = *)
(*   List.fold_left *)
(*     (fun m (p, ms) -> set_place p (Multiset.union (marking_of m p) ms) m) a b *)

(** m1 ⊖ m2 : defined iff m2 ⊆ m1 *)
(* let marking_diff (a : markingTok) (b : markingTok) : markingTok option = *)
(*   List.fold_left *)
(*     (fun acc (p, ms) -> *)
(*       Option.bind acc (fun m -> *)
(*         Option.map (fun r -> set_place p r m) *)
(*           (Multiset.diff (marking_of m p) ms))) *)
(*     (Some a) b *)

(* let marking_subset (a : markingTok) (b : markingTok) : bool = *)
(*   List.for_all (fun (p, ms) -> Multiset.subset ms (marking_of b p)) a *)

(** number of tokens in a place *)
let tokens_in (m : markingTok) p = Multiset.cardinal (marking_of m p)

open Token

let kt (w : t) : int =
  match w with
  | Tok_empty -> 0
  | Tok (_,_,j) -> j

let lab_tok (w : t) : label =
  match w with
  | Tok_empty -> ""
  | Tok (a,_,_) -> a

let toks (w : t) : t list =
  match w with
  | Tok_empty -> []
  | Tok (_,w,_) -> w


let w0 = Tok_empty                 (* size w0 -> 0 *)

let w1 = Tok ("a", [w0], 1)        (* size w1 -> 1 *)

let w2 = Tok ("b", [w0], 1)        (* size w2 -> 1 *)

let w3 = Tok ("a", [w0; Tok ("b", [w0], 1)], 2)  (* size w3 -> 2 *)

let w4 = Tok ("tau", [w3], 3)             (* size w4 -> 2 *)

  (** ---- Example ---- *)

  (** {m w_0 = (\cdot, \langle\!\langle \rangle\!\rangle, \cdot)} *)

  (** {m w_1 = (a, (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 1)} *)

  (** {m w_2 = (b, \langle\!\langle (\cdot, \langle\!\langle \rangle\!\rangle, \cdot)\rangle\!\rangle, 1)} *)

  (** {m w_3 = (a, \langle\!\langle (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 
    (b, (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 1)\rangle\!\rangle, 2)} *)

  (** {m w_4 = (\tau, \langle\!\langle(a, \langle\!\langle (\cdot, \langle\!\langle \rangle\!\rangle, \cdot), 
    (b, \langle\!\langle(\cdot, \langle\!\langle \rangle\!\rangle, \cdot)\rangle\!\rangle, 1)\rangle\!\rangle, 2)\rangle\!\rangle, 3)} *)

(* Example for use multiset for tokens *)
let mul1 = Multiset.of_list [w0; w0]
let mul2 = Multiset.of_list [w1]

(** This is the basic step of firing, from p to q. *)
let move p q w m =
  Option.map (add_token q w) (remove_token p w m) |> unopt_tk

(** [move "p2" "p3" w1 m1]  : Some {p ↦ {|w0, w0|}, q ↦ {| w1 |} } 

    [move "p2" "p3" w2 m1]  : None: w2 is not in p
*)
let marking_to_list (m : markingTok) =
  PMap.bindings m
  |> List.map (fun (p, ms) -> (p, Multiset.to_list ms))

let marking_to_bindings (m : markingTok) =
  PMap.bindings m
  |> List.map (fun (p, ms) -> (p, TokMap.bindings ms))

let show = Option.map marking_to_list

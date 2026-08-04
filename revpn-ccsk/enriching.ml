(* open Net *)

(** [token] is the set of tokens defined inductively as:
  - [Tok_empty]: {m (\cdot, \llangle \rrangle, \cdot)}
  - [Tok (a, ls, i)]: if [ls] has [token] type, {m (a, \llangle w_1,\dots,w_n\rrangle, i)} with {m a} an action, {m w_1,\dots,w_n} are tokens and {m i\in\mathbb N} an identificator number.
  *)
type token = 
  | Tok_empty
  | Tok of (label * token list * int)

let rec size (tk : token) : int =
  match tk with
  | Tok_empty -> 0
  | Tok (_, tok, _) -> 
      List.fold_left (max) 0 (List.map (size) tok) + 1


(** ---- Example 2 ---- *)

let w0 = Tok_empty                        (* size w0 -> 0 *)
let w1 = Tok ("a", [Tok_empty], 1)        (* size w1 -> 1 *)
let w2 = Tok ("b", [Tok_empty], 1)        (* size w2 -> 1 *)
let w3 = Tok ("a", [Tok_empty;
          Tok ("b", [Tok_empty], 1)], 2)  (* size w3 -> 2 *)
let w4 = Tok ("tau", [Tok ("a", [Tok_empty], 1); 
          Tok ("b", [Tok_empty], 2)], 3)  (* size w4 -> 2 *)

(* --------------------- *)

(** {b Key Labelled Net}*)


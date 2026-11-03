type token =
  | Tok_empty
  | Tok of (label * token list * int)

let rec size (tk : token) : int =
  match tk with
  | Tok_empty -> 0
  | Tok (_, ws, _) -> 
      List.fold_left (fun m w -> max m (size w)) 0 ws + 1

let compare : token -> token -> int = Stdlib.compare

let equal a b = compare a b = 0

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

let w3 = Tok ("a", [Tok_empty; Tok ("b", [Tok_empty], 1)], 2)  (* size w3 -> 2 *)

let w4 = Tok ("tau", [w3], 3)             (* size w4 -> 2 *)




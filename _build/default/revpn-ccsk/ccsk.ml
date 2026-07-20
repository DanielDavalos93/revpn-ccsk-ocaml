type act =
  | Input of string 
  | Output of string 
  | Silent

let co_action = function
  | Input a   -> Output a
  | Output a  -> Input a
  | Silent    -> Silent

(** We can tested that: [co_action(co_action a) = a], for [a] in [act_t]*)

let notation_act = function
  | Input a   -> a
  | Output a  -> "!" ^ a
  | Silent    -> "tau"

type relabel = string -> string

(** Syntax of CCS: process and agents *)
module CCS = struct

  (** Syntax of CCS: process and agents *)

type process =
  | Zero                          (* 0 *)
  | Prefix of act * process       (* alpha.Q *)
  | Choice of process * process   (* P + P' *)
  | Parallel of process * process  (* P | Q *)
  | Restriction of process * string list (* P \ {x1,x2,..} *)
  | Var of string                 (* X *)
  | Relabel of process * relabel
  | Rec of string * process       (* rec X.P *)

(* D = [Xi = Qi] *)
type equations = (string * process) list

let opt_equations (e : equations) (x : string) : process =
  match List.assoc_opt x e with
  | Some p -> p
  | None -> failwith ("Variable of process invalid: " ^ x)


(** Substitution *)

let rec subst (x : string) (s : process) (p : process) : process =
  match p with
  | Zero        -> Zero
  | Prefix (a, p1) -> Prefix (a, subst x s p1)
  | Choice (p1, p2) -> Choice (subst x s p1, subst x s p2)
  | Parallel (p1, p2) -> Parallel (subst x s p1, subst x s p2)
  | Restriction (p1, l) -> Restriction (subst x s p1, l)
  | Var y -> if y = x then s else Var y
  | Relabel (p1, f) -> Relabel (subst x s p1, f)
  | Rec (y, p1) -> if y = x then Rec (x, p1) else Rec (x, subst x s p1)
  


(** Structural semantics: Return the list [ls : (action * process) list] which a process
can execute in a step.*)

let relabel_act (f : relabel) (a : act) : act =
  match a with
  | Input x -> Input (f x)
  | Output x -> Output (f x)
  | Silent -> Silent

let rec transitions (e : equations) (p : process) : (act * process) list =
  match p with
  | Zero -> []
  | Prefix (a, p1) -> [(a,p1)]
  | Choice (p1, p2) -> transitions e p1 @ transitions e p2
  | Parallel (p1, p2) ->
      let transition1 = transitions e p1 in
      let transition2 = transitions e p2 in
      let left = List.map (fun (a, p1') -> (a, Parallel (p1', p2))) transition1 in
      let right = List.map (fun (a, p2') -> (a, Parallel (p1,p2'))) transition2 in
      let sync =
        List.concat_map (fun (a,p1') ->
          List.concat_map (fun (b, p2') ->
            if co_action a = b && a<> Silent then [(Silent, Parallel (p1', p2'))] else []
          ) transition1
        ) transition2
      in
      left @ right @ sync
  | Restriction (p1, rest) ->
      let is_restricted ac =
        match ac with
        | Input a | Output a -> List.mem a rest
        | Silent -> false
      in
      List.filter_map (fun (a, p1') ->
        if is_restricted a then None else Some (a, Restriction (p1', rest))
      ) (transitions e p1)
  | Relabel (p1, f) ->
      List.map (fun (a, p1') ->
        (relabel_act f a, Relabel (p1', f))
      ) (transitions e p1)
  | Var x -> transitions e (opt_equations e x)
  | Rec (x, body) ->
      let unfolded = subst x (Rec (x, body)) body in
      transitions e unfolded

let rec string_of_process = function
  | Zero             -> "0"
  | Prefix (a, Zero) -> notation_act a
  | Prefix (a, p)    -> notation_act a ^ "." ^ string_of_process p
  | Choice (p, q)    ->
      "(" ^ string_of_process p ^ " + " ^ string_of_process q ^ ")"
  | Parallel (p, q)       ->
      "(" ^ string_of_process p ^ " | " ^ string_of_process q ^ ")"
  | Restriction (p, l)  ->
      string_of_process p ^ " \\ {" ^ String.concat "," l ^ "}"
  | Relabel (p, _)   ->
      string_of_process p ^ "[f]"
  | Var x            -> x
  | Rec (x, p)       ->
      "rec " ^ x ^ ". " ^ string_of_process p

end

(** Syntax of CCSK: CCS and Keys *)

module CCSK = struct

type process =
  | Zero                          (* 0 *)
  | Prefix_i of act * int * process (* alpha[i].Q *)
  | Choice of process * process   (* P + P' *)
  | Parallel of process * process  (* P | Q *)
  | Restriction of process * string list (* (va)Q *)
  | Var of string                 (* X *)
  | Relabel of process * relabel

(* D = [Xi = Qi] *)
type equations = (string * process) list

let opt_equations (e : equations) (x : string) : process =
  match List.assoc_opt x e with
  | Some p -> p
  | None -> failwith ("Variable of process invalid: " ^ x)


(** Substitution *)

let rec subst (x : string) (s : process) (p : process) : process =
  match p with
  | Zero        -> Zero
  | Prefix_i (a, i, p1) -> Prefix_i (a, i, subst x s p1)
  | Choice (p1, p2) -> Choice (subst x s p1, subst x s p2)
  | Parallel (p1, p2) -> Parallel (subst x s p1, subst x s p2)
  | Restriction (p1, l) -> Restriction (subst x s p1, l)
  | Var y -> if y = x then s else Var y
  | Relabel (p1, f) -> Relabel (subst x s p1, f)


let relabel_act (f : relabel) (a : act) : act =
  match a with
  | Input x -> Input (f x)
  | Output x -> Output (f x)
  | Silent -> Silent

let rec transitions (e : equations) (p : process) : (act * process) list =
  match p with
  | Zero -> []
  | Prefix_i (a, _, p1) -> [(a, p1)]
  | Choice (p1, p2) -> transitions e p1 @ transitions e p2
  | Parallel (p1, p2) ->
      let transition1 = transitions e p1 in
      let transition2 = transitions e p2 in
      let left = List.map (fun (a, p1') -> (a, Parallel (p1', p2))) transition1 in
      let right = List.map (fun (a, p2') -> (a, Parallel (p1,p2'))) transition2 in
      let sync =
        List.concat_map (fun (a,p1') ->
          List.concat_map (fun (b, p2') ->
            if co_action a = b && a<> Silent then [(Silent, Parallel (p1', p2'))] else []
          ) transition1
        ) transition2
      in
      left @ right @ sync
  | Restriction (p1, rest) ->
      let is_restricted ac =
        match ac with
        | Input a | Output a -> List.mem a rest
        | Silent -> false
      in
      List.filter_map (fun (a, p1') ->
        if is_restricted a then None else Some (a, Restriction (p1', rest))
      ) (transitions e p1)
  | Relabel (p1, f) ->
      List.map (fun (a, p1') ->
        (relabel_act f a, Relabel (p1', f))
      ) (transitions e p1)
  | Var x -> transitions e (opt_equations e x)


(** CCS with Communication Keys *)

let rec key (p : process)  =
  match p with
  | Zero -> []
  | Var _ -> []
  | Prefix_i (_, i, q) -> [i] @ key q
  | Restriction (q, _) -> key q
  | Choice (q1, q2) -> key q1 @ key q2
  | Parallel (q1, q2) -> key q1 @ key q2
  | Relabel (q, _) -> key q

let std (q : process) : bool =
  key q == []

end

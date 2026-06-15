(* open Ccsk *)
(* Labelled Transition Systems (LTS) *)

(* Build LTS from CCS types (?) *)
type label = string

(** State. For practicy we use [[n] = 0..n-1] for states instead other type like [string]. 
The current state [init] should be less or equal than [n_states].
*)

type state = {
  n_states : int;
  init : int;
}

type transition = (int * label * int) list

type lts = {
    states : state;                     (* States s*)
    trans : transition    (* Transition relation *)
  }

module LTS = struct

  let make : state -> transition -> lts = fun s t -> {
    states = s;
    trans = t;
  }

  let succ (p : lts) (n : int) (a : label) : int list =
    List.filter_map (fun (s1, lbl, s2) ->
      if s1 = n && lbl = a then
        Some s2
      else 
        None) p.trans

  let pred (p : lts) (n : int) (a : label) : int list =
    List.filter_map (fun (s1, lbl, s2) ->
      if s2 = n && lbl = a then
        Some s1
      else 
        None) p.trans

  let all_labels (lts : lts) : label list =
    List.sort_uniq String.compare
      (List.map (fun (_, l, _) -> l) lts.trans)
  
  let postset (p : lts) (s : int list) (a : label) : int list =
    List.sort_uniq compare
      (List.concat_map (fun s -> succ p s a) s)

end

(** ---------------------------------------------------------------
   NAIVE ALGORITHM
   ---------------------------------------------------------------

     [R₀ = S × S]

     [Rₖ₊₁ = {! (s,t) ∈ Rₖ |
               ∀a. ∀s'. s --a--> s'  →  ∃t'. t --a--> t' ∧ (s',t') ∈ Rₖ
                    ∧
               ∀a. ∀t'. t --a--> t'  →  ∃s'. s --a--> s' ∧ (s',t') ∈ Rₖ }]
 
   Stops when Rₖ₊₁ = Rₖ (find a fix point).

   --------------------------------------------------------------- *)
 

(** Set of pairs as a ordered list *)

module PairSet = Set.Make(struct
  type t = int * int
  let compare = compare
end)
 

let refine_step (lts : lts) (labels : label list) (r : PairSet.t) : PairSet.t =
  PairSet.filter (fun (s, t) ->
    List.for_all (fun a ->
      (* ∀ s' : s --a--> s'  →  ∃ t' : t --a--> t' ∧ (s',t') ∈ R *)
      let ok_left =
        List.for_all (fun s' ->
          List.exists (fun t' -> PairSet.mem (s', t') r)
            (LTS.succ lts t a)
        ) (LTS.succ lts s a)
      in
      (* ∀ t' : t --a--> t'  →  ∃ s' : s --a--> s' ∧ (s',t') ∈ R *)
      let ok_right =
        List.for_all (fun t' ->
          List.exists (fun s' -> PairSet.mem (s', t') r)
            (LTS.succ lts s a)
        ) (LTS.succ lts t a)
      in
      ok_left && ok_right
    ) labels
  ) r
 
(** The complete algorithm. Iteration till the fix point *)

let bisim_naive (lts : lts) : PairSet.t =
  let labels = LTS.all_labels lts in
  let n      = lts.states.n_states in
  let r0 = List.fold_left (fun acc s -> (* r0 = [(i,j) | i,j ∈ [n-1]] *)
    List.fold_left (fun acc t -> PairSet.add (s,t) acc) acc
      (List.init n (fun i -> i))
  ) PairSet.empty (List.init n (fun i -> i)) in
  let rec iterate r =
    let r' = refine_step lts labels r in
    if PairSet.equal r r' then r else iterate r' in
  iterate r0
 
  (** [pre_a(B)]: states with some succ. [a] in the block [B]. *)

let pre_a (lts : lts) (block_of : int array) (b_id : int) (a : label) : int list =
  List.filter_map (fun (src, lbl, dst) ->
    if lbl = a && block_of.(dst) = b_id then Some src else None
  ) lts.trans
  |> List.sort_uniq compare
 
(* Splits the block `c_id` if their states are in `splitter_set` *)

let split_block (block_of : int array) (n : int) (c_id : int) 
                (splitter_set : int list) (next_id : int ref) : bool =
  let in_splitter = Hashtbl.create 16 in
  List.iter (fun s -> Hashtbl.add in_splitter s true) splitter_set;
  let inside  = ref [] in
  let outside = ref [] in
  for s = 0 to n - 1 do
    if block_of.(s) = c_id then
      if Hashtbl.mem in_splitter s
      then inside  := s :: !inside
      else outside := s :: !outside
  done;
  if !inside = [] || !outside = [] then false else 
    begin
      let new_id = !next_id in
      incr next_id;
      List.iter (fun s -> block_of.(s) <- new_id) !inside;
      ignore new_id;
      true
    end
 
(* Result of the algorithm *)
type partition_result = {
  block_of   : int array;   (* block_of.(s) *)
  n_blocks   : int;   
  blocks     : int list array; 
}
 
let bisim_partition (lts : lts) : partition_result =
  let n      = lts.states.n_states in
  let labels = LTS.all_labels lts in
  let block_of  = Array.make n 0 in
  let next_id   = ref 1 in
  let changed   = ref true in
  while !changed do
    changed := false;
    let current_n_blocks = !next_id in
    List.iter (fun a ->
      for b_id = 0 to current_n_blocks - 1 do
        let pre = pre_a lts block_of b_id a in
        if pre <> [] then begin
          let snap = !next_id in
          for c_id = 0 to snap - 1 do
            if split_block block_of n c_id pre next_id then
              changed := true
          done
        end
      done
    ) labels
  done;
  let nb = !next_id in
  let blocks = Array.make nb [] in
  for s = 0 to n - 1 do
    blocks.(block_of.(s)) <- s :: blocks.(block_of.(s))
  done;
  { block_of; n_blocks = nb; blocks }
 
(** ---------------------------------------------------------------
   WEAK BISIMULATION
   ---------------------------------------------------------------

  [s ≈ t] if there is [R] shuch that every [(s,t) ∈ R] satisfies:
       - if [s --a--> s'  (a ≠ τ)] then [∃ t'. t ==a==> t' y (s',t') ∈ R]
             where  [==a==>] means  [τ* · a · τ*]
       - si [s --τ--> s'] then [∃ t'. t ==ε==> t' y (s',t') ∈ R]
             where  [==ε==>] means  [τ*]
       (y simétricamente para t)
 
    Implementation for closure of [τ] by [BFS/DFS], then fix point.

   --------------------------------------------------------------- *)
 
let tau_closure (lts : lts) (s : int) : int list =
  let visited = Hashtbl.create 8 in
  let queue   = Queue.create () in
  Queue.push s queue;
  Hashtbl.add visited s true;
  while not (Queue.is_empty queue) do
    let curr = Queue.pop queue in
    List.iter (fun t ->
      if not (Hashtbl.mem visited t) then begin
        Hashtbl.add visited t true;
        Queue.push t queue
      end
    ) (LTS.succ lts curr "tau")
  done;
  Hashtbl.fold (fun k _ acc -> k :: acc) visited []
 
(* Weak LTS.succ: (tau-star to tau-star) *)
let weak_LTS_succ (lts : lts) (s : int) (a : label) : int list =
  let pre_tau = tau_closure lts s in
  let after_a = List.concat_map (fun s' -> LTS.succ lts s' a) pre_tau in
  let after_a_tau = List.concat_map (tau_closure lts) after_a in
  List.sort_uniq compare after_a_tau
 
(** Fix point for weak bisimulation *)

let bisim_weak (lts : lts) : PairSet.t =
  let visible_labels =
    List.filter (fun l -> l <> "tau") (LTS.all_labels lts) in
  let n  = lts.states.n_states in
  let r0 = List.fold_left (fun acc s ->
    List.fold_left (fun acc t -> PairSet.add (s,t) acc) acc
      (List.init n (fun i -> i))
  ) PairSet.empty (List.init n (fun i -> i))
  in
  let refine_weak r =
    PairSet.filter (fun (s, t) ->
      let ok_visible =
        List.for_all (fun a ->
          List.for_all (fun s' ->
            List.exists (fun t' -> PairSet.mem (s',t') r)
              (weak_LTS_succ lts t a)
          ) (weak_LTS_succ lts s a)
          &&
          List.for_all (fun t' ->
            List.exists (fun s' -> PairSet.mem (s',t') r)
              (weak_LTS_succ lts s a)
          ) (weak_LTS_succ lts t a)
        ) visible_labels in
      let ok_tau =
        List.for_all (fun s' ->
          List.exists (fun t' -> PairSet.mem (s',t') r)
            (tau_closure lts t)
        ) (tau_closure lts s)
        &&
        List.for_all (fun t' ->
          List.exists (fun s' -> PairSet.mem (s',t') r)
            (tau_closure lts s)
        ) (tau_closure lts t) in
      ok_visible && ok_tau
    ) r in
  let rec iterate r =
    let r' = refine_weak r in
    if PairSet.equal r r' then r else iterate r'
  in
  iterate r0
 
(** ---------------------------------------------------------------
   MINIMIZATION
   ---------------------------------------------------------------

   Given a LTS and a partition, returns the minimal LTS 
   --------------------------------------------------------------- *)
 
let minimize_lts (lts : lts) (pr : partition_result) : lts =
  let q_trans =
    List.map (fun (s, a, t) ->
      (pr.block_of.(s), a, pr.block_of.(t))
    ) lts.trans
    |> List.sort_uniq compare
  in
  let state_min : state = {
    n_states = pr.n_blocks;
    init = pr.block_of.(lts.states.init)
  }
  in
  { states = state_min;
    trans    = q_trans;
  }
 
(** ---------------------------------------------------------------
   PRETTY-PRINTERS
   --------------------------------------------------------------- *)
 
let print_partition (pr : partition_result) =
  Printf.printf "  Partición (%d bloques):\n" pr.n_blocks;
  Array.iteri (fun b states ->
    let sorted = List.sort compare states in
    Printf.printf "    B%d = { %s }\n" b
      (String.concat ", " (List.map string_of_int sorted))
  ) pr.blocks
 
let print_lts_explicit (name : string) (lts : lts) =
  Printf.printf "  LTS '%s': %d estados, %d transiciones, inicial=%d\n"
    name lts.states.n_states (List.length lts.trans) lts.states.n_states;
  List.iter (fun (s, a, t) ->
    Printf.printf "    %d --[%s]--> %d\n" s a t
  ) lts.trans
 
let print_bisim_relation (r : PairSet.t) (n : int) =
  Printf.printf "  Bisimulación (pares s~t, s<n=%d):\n" n;
  PairSet.iter (fun (s, t) ->
    if s <= t then Printf.printf "    %d ~ %d\n" s t
  ) r

 
  (* let lts_from_ccsk (q : equations) (p : process) : lts = *)
  (*   {} *)

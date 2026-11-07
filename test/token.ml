open Revpn_ccsk.Token

let s1 = Multiset.of_list [e; e; w1]       (* {| e, e, w1 |} *)
let s2 = Multiset.of_list [e; w2]          (* {| e, w2 |}    *)

Multiset.count e s1                        (* 2 *)
Multiset.cardinal s1                       (* 3 *)

(* sum *)
Multiset.union s1 s2                       (* {| e,e,e, w1, w2 |} *)

(* difference *)
Multiset.diff s1 s2                        (* None: w2 ∉ s1 *)
Multiset.diff (Multiset.union s1 s2) s2    (* Some s1 *)

(* remove one copy *)
Multiset.remove w1 s1                      (* Some {| e,e |} *)
Multiset.remove w2 s1                      (* None *)

(* inclusion *)
Multiset.subset (Multiset.of_list [e; w1]) s1   (* true  *)
Multiset.subset s2 s1                           (* false *)

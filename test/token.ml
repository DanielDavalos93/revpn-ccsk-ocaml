(* open Revpn_ccsk.Token *)

let e = Token.Tok_empty

let ms1 = Multiset.of_list [e; e]            (* {| e, e, w1 |} *)
let ms2 = Multiset.of_list [e;e; w2]         (* {| e, w2 |}    *)

(* Markings *)
let m0 = make_marking [ (s1, [e; e]) ]

let m1 = make_marking [ (s1, [e; e]);  (s2, [w1]) ]

let m2 = make_marking [ (s1, [e]);     (s2, [w1; w2]); (s3, [w2]) ]

let () =
  assert (Multiset.count e ms1 == 2);
  assert (Multiset.cardinal ms1 == 2);
  assert (TokMap.bindings ms1 = [(e, 2)]);
  assert (TokMap.cardinal ms1 == 1);
  assert (tokens_in m0 s1 == 2);
  (* sum *)
  assert (Multiset.to_list (Multiset.union ms1 ms2) = [Token.Tok ("b", [e], 1); e; e; e; e]);                       (* {| e,e,e, w1, w2 |} *)
  assert (Multiset.diff ms1 ms2 = None);  (* None: w2 ∉ s1 *)
  assert (Multiset.diff (Multiset.union ms1 ms2) ms2 = Some ms1);
(* Multiset.remove w1 s1                      (* Some {| e,e |} *) *)
(* Multiset.remove w2 s1                      (* None *) *)
  assert (marking_subset m0 m1 == true);
  assert (Multiset.subset (Multiset.of_list [e; e]) ms1 = true);
  assert (Multiset.subset ms2 ms1 = false);

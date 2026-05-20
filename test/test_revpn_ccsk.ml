(* Net *)

(* Define a net *)

let places = generate_place 4

let transitions = generate_transition 4

let arcs = [
  PT ("s1", "t1", 1); PT ("s1", "t2", 1);
  TP ("t1", "s2", 1); TP ("t2", "s3", 1);
  PT ("s3", "t4", 1); PT ("s2", "t4", 1); PT ("s2", "t3", 1);
  TP ("t3", "s4", 1); TP ("t4", "s4", 1)
]

let set : transition_id list = ["a"; "b"; "tau"]

let lambda t  =
  match t.t_id with
  | "t1" -> {t_id = t.t_id; t_label = "a"}
  | "t2" -> {t_id = t.t_id; t_label = "b"}
  | "t3" -> {t_id = t.t_id; t_label = "b"}
  | "t4" -> {t_id = t.t_id; t_label = "tau"}
  | _ -> {t_id = t.t_id; t_label = t.t_label}

let label = List.map (fun x -> lambda x) transitions 

let init_marking = [("s1", 1)]

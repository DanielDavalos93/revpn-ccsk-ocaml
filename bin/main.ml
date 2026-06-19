open Revpn_ccsk.Lts

(*
  
  To verify if two LTS's are bisimilars, we put them 
  together and we apply the bisimulation.

  Test A — Trivial bisimulation
  ─────────────────────────────────
  LTS1:  0 --a--> 1 --b--> 2
  LTS2:  0 --a--> 1 --b--> 2

*)

let lts_trivial : lts = {
  states = {
    n_states = 3;
    init = 0
  };
  trans = [(0,"a",1); (1,"b",2)] (* P and Q = a.b.0 *)
}

(*
  Test B — Choice vs Parallel
  ─────────────────────────────────
  P = a.0 + b.0        Q = (a.0 | b.0) \ {}
  In CCS: P and Q are not bisimilars because in Q it can
  be do first 'a' and then 'b', while in P only one of 
  two, no both.

  LTS:
    P:  0 --a--> 1
        0 --b--> 2
    Q:  3 --a--> 4
        3 --b--> 5
        4 --b--> 6
        5 --a--> 7
*)

let lts_choice_vs_par : lts = {
  states = {
    n_states = 8;
    init  = 0;
  };
  trans    = [
    (* P = a.0 + b.0 *)
    (0, "a", 1); (0, "b", 2);
    (* Q = a.b.0 | b.a.0  *)
    (3, "a", 4); (3, "b", 5);
    (4, "b", 6); (5, "a", 7);
  ]
}

(*
  Test C — Strong bisimulation with τ
  ──────────────────────────────────────
  P = τ.a.0         Q = a.0
  Are NOT strongly bisimilars (P first do τ).
  Are weakly bisimilars.

  (0,3):
    P: 0 --tau--> 1 --a--> 2
    Q: 3 --a-->  2          (they shared the state 2 = 0)
*)
let lts_tau_example : lts = {
  states = {
    n_states = 4;
    init  = 0;
  };
  trans    = [
    (0, "tau", 1); (1, "a", 2);   (* P = τ.a.0 *)
    (3, "a",   2);                (* Q = a.0   *)
  ]
}

(*
  Test D — Semaphore with two cicle
  ──────────────────────────────────────────────────
  S1 = green.red.S1     (length of cycle = 2)
  S2 = green.red.green.rojo.S2  (length = 4, but bisimilar to S1)

  S1: 0 --[green]--> 1 --[red]--> 0
  S2: 2 --[green]--> 3 --[red]--> 4 --[green]--> 5 --[red]--> 2

  bisimilares: 2 ~ 0, 3 ~ 1, 4 ~ 0, 5 ~ 1.
*)
let lts_semaphores : lts = {
  states = {
    n_states = 6;
    init  = 0;
  };
  trans    = [
    (* S1: cicle 2 *)
    (0, "green", 1); (1, "red", 0);
    (* S2: cicle 4 *)
    (2, "green", 3); (3, "red", 4);
    (4, "green", 5); (5, "red", 2);
  ]
}


(* ---------------------------------------------------------------
   § MAIN
   --------------------------------------------------------------- *)

let check_bisim_pair (lts : lts) (r : PairSet.t)
                     (s : int) (t : int) (msg : string) =
  let result = PairSet.mem (s, t) r in
  Printf.printf "  %s:  %d ~ %d  ?  %b\n" msg s t result

let () =
  print_endline "\n╔═══════════════════════════════════════════════════════════╗";
  print_endline   "║   BISIMULATION OVER FINITE LTS'S — Classic algorithms     ║";
  print_endline   "╚═══════════════════════════════════════════════════════════╝";
  (* ── TEST A: Trivial ── *)
  print_endline "\n▶ Test A: P = Q = a.b.0";
  print_lts_explicit "lts_trivial" lts_trivial;

  print_endline "\n  [Alg.1 Naive] Calculing strong bisimulation...";
  let r_a = bisim_naive lts_trivial in
  check_bisim_pair lts_trivial r_a 0 0
    "P(0) ~ Q(0)? ";
  check_bisim_pair lts_trivial r_a 1 1
    "P(1) ~ Q(1)? ";

  print_endline "\n  [Alg.2 Partition] Calculing partition...";
  let pr_a = bisim_partition lts_trivial in
  print_partition pr_a;

  (* ── TEST B: Choice vs Parallel ── *)
  print_endline "\n▶ Test B: P = a.0+b.0  vs  Q = (a | b)";
  print_lts_explicit "choice_vs_par" lts_choice_vs_par;

  print_endline "\n  [Alg.1 Naive] Calculaning strong bisimulation...";
  let r_b = bisim_naive lts_choice_vs_par in
  check_bisim_pair lts_choice_vs_par r_b 0 3
    "P(0) ~ Q(3)?";
  check_bisim_pair lts_choice_vs_par r_b 0 0
    "P(0) ~ P(0)?";

  print_endline "\n  [Alg.2 Partition] Calculing partition...";
  let pr_b = bisim_partition lts_choice_vs_par in
  print_partition pr_b;

  (* ── TEST C ── *)
  print_endline "\n▶ Example C: P = τ.a.0  vs  Q = a.0";
  print_lts_explicit "tau_example" lts_tau_example;

  print_endline "\n  [Alg.1 Naive] Calculaning strong bisimulation...:";
  let r_c_strong = bisim_naive lts_tau_example in
  check_bisim_pair lts_tau_example r_c_strong 0 3
    "P(0) ~ Q(3)?";

  print_endline "\n  [Alg.3 weak] Weak bisimulation:";
  let r_c_weak = bisim_weak lts_tau_example in
  check_bisim_pair lts_tau_example r_c_weak 0 3
    "P(0) ≈ Q(3)?";

  (* ── TEST D: Cicle Semaphores ── *)
  print_endline "\n▶ Test D: S1 (cicle-2) vs S2 (cicle-4)";
  print_lts_explicit "semaphores" lts_semaphores;

  print_endline "\n  [Alg.2 Partition] Calculing partition:";
  let pr_d = bisim_partition lts_semaphores in
  print_partition pr_d;

  print_endline "\n  [Alg.1 Naive] Calculaning strong bisimulation:";
  let r_d = bisim_naive lts_semaphores in
  check_bisim_pair lts_semaphores r_d 0 2
    "S1_initial(0) ~ S2_initial(2)? (true)";
  check_bisim_pair lts_semaphores r_d 1 3
    "S1_red(1)   ~ S2_red(3)?   (true)";
  check_bisim_pair lts_semaphores r_d 0 3
    "S1_initial(0) ~ S2_red(3)?   (false)";

  print_endline "\n  [Minimization] LTS min. of semaphores:";
  let lts_min_d = minimize_lts lts_semaphores pr_d in
  print_lts_explicit "Semaphore minime" lts_min_d;


  (* ── RESUMEN  ── *)
  print_endline "\n▶ Equivalence classes by strong bisimulation\n";
  let print_classes name lts =
    let pr = bisim_partition lts in
    Printf.printf "  %s → %d Equivalence classes (%d states)\n"
      name pr.n_blocks lts.states.n_states
  in
  print_classes "choice_vs_par" lts_choice_vs_par;
  print_classes "tau_example"   lts_tau_example;
  print_classes "semaphores"     lts_semaphores;



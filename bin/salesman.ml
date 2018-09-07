(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * salesman.ml
 *
 * Main application
 *)

open Cmdliner


(* main entry point *)
let salesman verbose =
  if verbose then
    print_endline "Done."
  else ()


(* cmdliner options *)
let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

(* let initial_pos =
  let doc = "The initial position of the first queen" in
  Arg.(value & opt (int, int) (1, 1) & info ["inital-pos"] ~docv:"INITIAL_POS" ~doc)
*)

let cmd =
  let doc = "A evolutional algorithm to solve the traveling salesman problem, implemented in OCaml" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) sets up the initial population of routes
        and runs the evolutional algorithm.";
    `P "The results are written to STDOUT";
    `P "Reference: $(i,https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35)"]
  in
  Term.(const salesman $ verbose),
  Term.info "salesman" ~version:"0.1.0" ~doc ~man


(* start main *)
let () =
  match Term.eval cmd with
  | `Error _  -> exit 1
  | _         -> exit 0



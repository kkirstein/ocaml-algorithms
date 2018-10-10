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
open Ga.Population

(* genetic algorithm *)
let genetic_algorithm ~elite_size ~mutation_rate ~population_size ~generations first_generation =
  let initial = initial_population population_size first_generation |>
                List.map Ga.Fitness.calculate
  in
  let rec loop acc last idx =
    if idx < generations then
      let next = Ga.Population.next_generation ~elite_size ~mutation_rate last in 
      loop (next :: acc) next (idx + 1)
    else List.rev acc
  in
  loop [] initial 0

(* extract distance values *)
let get_distance generations =
  List.map (fun g -> (List.hd g).Ga.Fitness.distance) generations

let get_all_distance generations =
  List.map (fun g -> List.map (fun i -> i.Ga.Fitness.distance) g) generations

(* main entry point *)
let salesman verbose elite_size mutation_rate population_size generations =
  if verbose then
    Printf.printf "Config: population_size: %d, elite_size: %d, mutation_rate: %f, generations: %d\n"
      population_size elite_size mutation_rate generations
  else ();
  let first_generation =  List.init 25
      (fun _ -> Ga.City.create (Random.int 200) (Random.int 200))
  in
  let ga = genetic_algorithm ~elite_size ~mutation_rate ~population_size ~generations first_generation in
  let distances = get_distance ga in
  let all_distance = get_all_distance ga in
  if verbose then begin
    Printf.printf "Initial distance: %f.\n" (List.hd distances);
    Printf.printf "Final distance: %f.\n" (List.rev distances |> List.hd);
    List.iter
      (fun d -> print_endline (String.concat "," (List.map string_of_float d)))
      all_distance;
    print_endline "Done."
  end
  else ()

(* cmdliner options *)
let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)
let population =
  let doc = "The population size" in
  Arg.(value & opt int 100 & info ["p"; "population"] ~docv:"POPULATION" ~doc)
let elite_size =
  let doc = "The number of elite individuals for favioured selection" in
  Arg.(value & opt int 20 & info ["e"; "elite-size"] ~docv:"ELITE_SIZE" ~doc)
let mutation_rate =
  let doc = "A factor to control the amount of mutations per generation" in
  Arg.(value & opt float 0.01 & info ["m"; "mutation-rate"] ~docv:"MUTATION_RATE" ~doc)
let generations =
  let doc = "The number of generations to be calculated" in
  Arg.(value & opt int 20 & info ["g"; "generations"] ~docv:"GENERATIONS" ~doc)

let cmd =
  let doc = "A evolutional algorithm to solve the traveling salesman problem, implemented in OCaml" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) sets up the initial population of routes
        and runs the evolutional algorithm.";
    `P "The results are written to STDOUT";
    `P "Reference: $(i,https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35)"]
  in
  Term.(const salesman $ verbose $ elite_size $ mutation_rate $ population $ generations),
  Term.info "salesman" ~version:"0.1.0" ~doc ~man


(* start main *)
let () =
  match Term.eval cmd with
  | `Error _  -> exit 1
  | _         -> exit 0



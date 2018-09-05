(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * fitness.ml
 *
 * A module to calculate the fitness of chromosome aka route
 *)

(**
 * A route and its fitness
 *)
type t = {
  route : City.t list;
  distance : float;
  fitness : float
}

(**
 * Calculate fitness for given route
 *)
let calculate route =
  let distance = 0.0 in
  { route; distance; fitness = 1 /. distance }



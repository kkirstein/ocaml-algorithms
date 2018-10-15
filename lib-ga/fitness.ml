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
  let dist = 
    let start = List.hd route in
    let rec loop rs acc =
      match rs with
      | []      -> failwith "Empty route given!"
      | h :: [] -> acc +. (City.distance h start)
      | h :: t  -> loop t (acc +. City.distance h (List.hd t))
    in
    loop route 0.0
  in
  { route; distance = dist; fitness = 1.0 /. dist }



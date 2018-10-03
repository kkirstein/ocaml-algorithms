(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * population.ml
 *
 * A module for populations of travel routes
 *)


(**
 * create a rout by random sampling from list of cities
 *)
let create_route cities =
  let rec loop c =
    match c with
    | [] -> []
    | c -> let idx = Random.int (List.length c) in
      let sample = List.nth c idx in
      let remain = List.filter (fun x -> x <> sample) c in
      sample :: loop remain
  in
  loop cities

(**
 * generate an initial population
 *)
let initial_population size cities =
  let rec loop i =
    if i < size then (create_route cities) :: loop (i + 1)
    else []
  in
  loop 1

(**
 * generate ranking of the given population
 *)
let rank population =
  let open Fitness in
  let fitness = List.map (fun x -> Fitness.calculate x) population in
  List.sort (fun a b -> compare a.fitness b.fitness) fitness

(**
 * generate ranking of given population and return sorted index
 *)
let rank_index population =
  let open Fitness in
  let fitness = List.map (fun x -> calculate x) population in
  let rec range a b = if a < b then a :: range (a + 1) b else [] in
  let combined = List.combine fitness (range 0 (List.length population)) in
  List.sort (fun (a, _) (b, _) -> compare a.fitness b.fitness) combined

(**
 * splits the list into a parts at the given position
 *)
let split_list n l =
  let rec loop a b pos =
    if pos < n then loop ((List.hd b) :: a) (List.tl b) (pos + 1)
    else (List.rev a, b)
  in
  loop [] l 0

(**
 * calculates the cumulative sum of the given float list
*)
let cum_sum data =
  let rec loop acc d =
    match d with
    | [] -> []
    | h :: t -> let new_sum = (h +. acc) in new_sum :: loop new_sum t
  in
  loop 0.0 data

(**
 * calculates the relative cumulative sum of given float list
*)
let cum_perc data =
  let cum_sum = cum_sum data in
  let sum = List.fold_left (fun a b -> a +. b) 0.0 data in
  List.map (fun x -> 100.0 *. x /. sum) cum_sum

(**
 * create the mating pool based on the given ranked population and
 * an additional number of elite items
 *)
let mating_pool ranked_population elite_size =
  let _cum_perc = cum_perc (List.map (fun x -> x.Fitness.fitness) ranked_population)
  in
  (* TODO: select populations *)
  split_list elite_size ranked_population


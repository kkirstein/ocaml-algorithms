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
 * Input module signature for the data set a fitness has to be calculated
 *)
module type Individual = sig

  type individual

  (**
   * Calculate fitness for given individual
  *)
  val fitness : individual -> float

  (**
   * breeds a child from 2 given parents,
   * e.g., by ordered crossover
   *)
  val breed : individual -> individual -> individual

  (**
   * mutates a single individual,
   * e.g., by swapping positions in case of a list
   *)
  val mutate : individual -> individual

end


(**
 * Module to calculate, mutate and breed the fitness
 * of individuals and populations
 *)
module type S = sig

  type individual

  type t = {
    individual : individual;
    fitness : float
  }

  (**
   * ranks (sorts) the given population according to the fitness
   * of its individuals
  *)
  val rank : t list -> t list

  (**
   * creates a mating pool based on the given ranked population and
   * an additional number of elite individuals
  *)
  val mating_pool : elite_size : int -> t list -> t list

  (**
   * breed a population (= list of individuals)
  *)
  val breed : elite_size : int -> t list -> t list

  (**
   * perform mutations for given population (= list of individuals)
  *)
  val mutate : mutation_rate : float -> t list -> t list

  (**
   * generate the next generation by selection, buiding a mating pool,
   * breeding and mutation
  *)
  val next_generation : elite_size : int -> mutation_rate : float -> t list -> t list

end


(**
 * Functor to generate a fitness module for the target data
 *)
(*
module Make (D : Individual) : S with type individual = D.individual
*)
module Make (D : Individual) = struct

  type individual = D.individual

  type t = {
    individual : individual;
    fitness : float
  }


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
   * ranks (sorts) the given population according to the fitness
   * of its individuals
  *)
  let rank population =
    let open Fitness in
    (* let fitness = List.map (fun x -> Fitness.calculate x) population in *)
    List.sort (fun a b -> - compare a.fitness b.fitness) population


  (**
   * creates a mating pool based on the given ranked population and
   * an additional number of elite individuals
  *)
  let mating_pool ~elite_size ranked_population =
    let open Listx in
    let cum_perc = cum_perc (List.map (fun x -> x.Fitness.fitness) ranked_population)
    in
    let elite, others = split_list elite_size ranked_population in
    let _, others_cum_perc = split_list elite_size cum_perc in
    let pick_factor = List.map (fun _ -> Random.float 100.0) others in
    (* let _others_picked = List.combine pick_factor (List.combine others_cum_perc others) |>
                        List.filter (fun (p, (c, _)) -> p <= c) |>
                        List.map (fun (_, (_, x)) -> x)
       in *)
    let others_picked_2 = (List.map
                             (fun p -> List.filter (fun (c, _) -> p <= c) (List.combine others_cum_perc others) |> List.hd)
                             pick_factor) |> List.map (fun (_, i) -> i)
    in
    List.append elite others_picked_2 |>
    List.map (fun x -> x.Fitness.route)


  (**
   * breed a population (= list of individuals)
  *)
  let breed ~elite_size mating_pool =
    let open Listx in
    let elite = sub_list 0 (elite_size - 1) mating_pool in
    let pool = sample_list mating_pool in
    let n_all = List.length mating_pool in
    let n = n_all - elite_size in
    let children = 
      List.map (fun i -> D.breed (List.nth pool i)(List.nth pool (n_all - i - 1))) (range 0 n)    (*
    let rec loop i =
      if i < n then begin
        Printf.printf "loop: %d; " i;
        (breed (List.nth pool i) (List.nth pool (n_all - i - 1))) :: loop (i + 1)
      end
      else []
    in
    loop 0
       *)
    in
    List.append elite children



end


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



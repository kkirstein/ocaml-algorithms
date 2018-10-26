(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * fitness.mli
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
  val mutate : mutation_rate : float -> individual -> individual

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
   * calculate fitness of given population
  *)
  val fitness : individual list -> t list

  (**
   * extracts the individuals from the fitness container
  *)
  val individuals : t list -> individual list

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
  val breed : elite_size : int -> individual list -> individual list

  (**
   * perform mutations for given population (= list of individuals)
  *)
  val mutate : mutation_rate : float -> individual list -> individual list

  (**
   * generate the next generation by selection, buiding a mating pool,
   * breeding and mutation
  *)
  val next_generation : elite_size : int -> mutation_rate : float -> t list -> t list

end


(**
 * Functor to generate a fitness module for the target data
*)
module Make (D : Individual) : S with type individual = D.individual


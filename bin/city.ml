(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * city.ml
 *
 * A module for the cities to be travelled to
 *)

(**
 * struct for the coordinates
 *)
type t = {
  x : int;
  y : int
}

(**
 * Create a city with given coordinates
 *)
let create x y = { x; y }

(**
 * Calulate (cartesian) distance between 2 cities
 *)
let distance c1 c2 =
  let x_diff = (float_of_int c1.x) -. (float_of_int c2.x)
  and y_diff = (float_of_int c1.y) -. (float_of_int c2.y)
  in
  sqrt ((x_diff ** 2.0) +. (y_diff ** 2.0))


(**
 * string representation of a city
 *)
let to_string city =
  "(" ^ (string_of_int city.x) ^ "," ^ (string_of_int city.y) ^ ")"


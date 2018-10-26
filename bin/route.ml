(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * route.ml
 *
 * A module for a single route between given cities
*)

open Listx

(**
 * The basic type as individual for the GA
*)
type individual = City.t list


(**
 * create a route by random sampling from list of cities
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
  loop 0


(**
 * Calculate fitness for given route
*)
let fitness route =
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
  1.0 /. dist


(**
 * breeds a child from to given parents
 * using ordered crossover
*)
let breed parent1 parent2 =
  (* check equal length of parents *)
  assert (List.compare_lengths parent1 parent2 = 0);
  (* select "genes" from first parent *)
  let g1, g2 = Random.int (List.length parent1), Random.int (List.length parent1) in
  let start_gene, end_gene = min g1 g2, max g1 g2 in
  let crossover = sub_list start_gene end_gene parent1 in
  let rec loop c p2 i =
    if i < start_gene || i > end_gene then (match p2 with
        | []  -> []
        | hd :: tl -> if List.mem hd crossover then loop c tl i else hd :: (loop c tl (i + 1)))
    else (match c with
        | [] -> loop [] p2 (i + 1)
        | hd :: tl -> hd :: (loop tl p2 (i + 1)))
  in
  loop crossover parent2 0


(**
 * mutate a single individual by swapping to positions
*)
let mutate ~mutation_rate individual =
  let len = List.length individual in
  let swap_pred = List.init len (fun _ -> Random.float 1.0 < mutation_rate) in
  let swap_with = List.map (fun p -> if p then Some (Random.int len) else None) swap_pred in
  let ary = Array.of_list individual in
  for i = 0 to (len - 1) do
    match List.nth swap_with i with
    | Some i2 -> let orig = ary.(i) in ary.(i) <- ary.(i2); ary.(i2) <- orig
    | None    -> ()
  done;
  Array.to_list ary
(*
let mutate_fp individual mutation_rate =
  let len = List.length individual in
  let swap_pred = List.init len (fun _ -> Random.float 1.0 < mutation_rate) in
  let swap_with = List.map (fun p -> if p then Some (Random.int len) else None) swap_pred in
  let swap_with_2 = swap_with in
  let first_swap = List.map2 (fun item swap ->  match swap with
      | None -> item
      | Some idx -> List.nth individual idx)
      individual swap_with in
  List.map2 (fun item swap -> match swap with
      | None -> item
      | Some idx -> List.nth individual idx)
    first_swap swap_with_2
*)



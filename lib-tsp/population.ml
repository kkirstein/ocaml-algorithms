(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * Evolutional algorithm for the Travelling Salesman Problem
 * https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35
 *
 * population.ml
 *
 * A module for populations of travel routes
 *)

(* first some helper functions *)

(**
 * generates a list of int values with given range
 * (start inclusive, stop exlcusive)
 *)
let rec range start stop =
  if start < stop then start :: range (start + 1) stop
  else []

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
 * extracts a sublist of the given indices (inclusive).
 * Returns an empty list, if out of bounds.
 *)
let sub_list start stop l =
  let rec loop i ll =
    match ll with
    | [] -> ll
    | h :: t -> if i >= start && i <= stop then h :: (loop (i + 1) t)
      else loop (i + 1) t
  in
  loop 0 l

(**
 * takes out the first element equal to a
 *)
let take_out_first a l =
  let flag = ref false in
  List.fold_left
    (fun acc x -> if x = a && not !flag then (flag := true; acc) else x :: acc)
    [] l |>
  List.rev

(**
 * random sampling of list entries
 *)
let sample_list ?n l =
  let len = match n with
    | None    -> List.length l
    | Some n  -> n
  in
  let rec loop l i =
    if i < len then
      match l with
      | [] -> []
      | l -> let idx = Random.int (List.length l) in
        let sample = List.nth l idx in
        let remain = take_out_first sample l in
        sample :: loop remain (i + 1)
    else []
  in
  loop l 0

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


(* create, breed & mutate population *)

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
 * generate ranking of the given population
 *)
let rank population =
  let open Fitness in
  (* let fitness = List.map (fun x -> Fitness.calculate x) population in *)
  List.sort (fun a b -> compare a.fitness b.fitness) population


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
 * create the mating pool based on the given ranked population and
 * an additional number of elite items
 *)
let mating_pool ~elite_size ranked_population =
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
 * breed a complete population
 *)
let breed_population ~elite_size mating_pool =
  let elite = sub_list 0 (elite_size - 1) mating_pool in
  let pool = sample_list mating_pool in
  let n_all = List.length mating_pool in
  let n = n_all - elite_size in
  Printf.printf "breed: n_all: %d, n: %d, pool_size: %d\n" n_all n (List.length pool);
  let children = 
    List.map (fun i -> breed (List.nth pool i)(List.nth pool (n_all - i - 1))) (range 0 n)    (*
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


(**
 * perform mutations for the whole population
 *)
let mutate_population ~mutation_rate population =
  print_endline "mutate...";
  List.map (mutate ~mutation_rate) population


(**
 * generate next generation by selection, building a mating pool,
 * breeding and mutation
 *)
let next_generation ~elite_size ~mutation_rate current =
  rank current |>
  mating_pool ~elite_size |>
  breed_population ~elite_size |>
  mutate_population ~mutation_rate |>
  List.map Fitness.calculate




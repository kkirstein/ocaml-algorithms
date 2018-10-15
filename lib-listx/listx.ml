(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * listx.ml
 *
 * A module with often needed utility functions for
 * list genertaion and processing
 *)

(**
 * generates a list of int values with given range
 * (start inclusive, stop exlcusive)
 *)
let rec range start stop =
  if start < stop then start :: range (start + 1) stop
  else []

let%test _ = range 0 3 = [0; 1; 2]
let%test _ = range 2 8 = [2; 3; 4; 5; 6; 7]
let%test _ = range 2 2 = []
let%test _ = range 4 2 = []

(**
 * splits the list into a parts at the given position
 *)
let split_list n l =
  let rec loop a b pos =
    if pos < n then loop ((List.hd b) :: a) (List.tl b) (pos + 1)
    else (List.rev a, b)
  in
  loop [] l 0

let%test_module _ = (module struct
                      let ll = [1; 2; 3; 4; 5; 6; 7; 8]

                      let%test _ = split_list 0 ll = ([], ll)
                      let%test _ = split_list 1 ll = ([1], [2; 3; 4; 5; 6; 7; 8])
                      let%test _ = split_list 5 ll = ([1; 2; 3; 4; 5], [6; 7; 8])
                      let%test _ = split_list 8 ll = (ll, [])
                    end)


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

let%test_module _ = (module struct
                      let ll = [1; 2; 3; 4; 5; 6; 7; 8]

                      let%test _ = sub_list 2 6 ll = [3; 4; 5; 6; 7]
                      let%test _ = sub_list 0 7 ll = ll
                      let%test _ = sub_list 5 3 ll = []
                      let%test _ = sub_list 3 8 ll = [4; 5; 6; 7; 8]
                    end)


(**
 * takes out the first element equal to a
 *)
let take_out_first a l =
  let flag = ref false in
  List.fold_left
    (fun acc x -> if x = a && not !flag then (flag := true; acc) else x :: acc)
    [] l |>
  List.rev

let%test_module _ = (module struct
                      let ll = [1; 2; 4; 3; 2; 5; 4; 7; 2]

                      let%test _ = take_out_first 3 ll = [1; 2; 4; 2; 5; 4; 7; 2]
                      let%test _ = take_out_first 0 ll = [1; 2; 4; 3; 2; 5; 4; 7; 2]
                      let%test _ = take_out_first 2 ll = [1; 4; 3; 2; 5; 4; 7; 2]
                      let%test _ = take_out_first 4 ll = [1; 2; 3; 2; 5; 4; 7; 2]
                    end)


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


let%test_module _ = (module struct
                      let ll = [1; 2; 3; 4; 5; 6; 7; 8]

                      let%test _ = List.length (sample_list ll) = List.length ll
                      let%test _ = List.length (sample_list ~n:5 ll) = 5
                    end)



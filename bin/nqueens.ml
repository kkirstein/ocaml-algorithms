(* vim: set ft=ocaml sw=2 ts=2: *)

(* benchmark.ml
 * A set of micro-benchmarks for the OCaml
 * programming language
*)

open Cmdliner

type position = int * int
exception NoSolution

(* the backtracking algorithm *)
let check_position queens (r, c) = 
  not (List.exists ( fun q ->
      let (qr, qc) = q in
      (r = qr) || (c = qc) || (abs (qr-r) = abs (qc - c))
    )
      queens)

let backtrack n initial_pos =
  let rec search queens pos =
    let x, y = pos in Printf.printf "%d:%d\n" x y;
    match pos with
    | (r, _) when r = n                     -> List.rev queens
    | (r, c) when c < (n-1) && check_position queens pos -> search (pos :: queens) (r+1, 0)
    | (r, c) when c < (n-1)                 -> search queens (r, c+1)
    | (_, _)                                -> (
        let (lr, lc) = List.hd queens in
        search (List.tl queens) (lr, lc+1))
  in
  try
    search [initial_pos] (1, 0)
  with NoSolution -> []

(* pretty-printing of solution *)
let pretty_print queens =
  List.map (fun (r, c) -> Printf.sprintf "(%d,%d)" r c) queens
  |> String.concat "\n"
  |> print_endline

(* main entry point *)
let nqueens _n = 
  let tic = print_endline "Calculating.."; Sys.time () in
  let solution = backtrack 8 (0, 0) in
  let toc = Sys.time () in
  pretty_print solution;
  Printf.printf " done (Elapsed time: %.3fs).\n" (toc -. tic)


(* cmdliner options *)
let num_queens =
  let doc = "The number of queens to be positioned" in
  Arg.(value & opt int 8 & info ["n"; "num-queens"] ~docv:"NUM_QUEENS" ~doc)

(* let initial_pos =
  let doc = "The initial position of the first queen" in
  Arg.(value & opt (int, int) (1, 1) & info ["inital-pos"] ~docv:"INITIAL_POS" ~doc)
*)

let cmd =
  let doc = "A backtracking algorithm for the N queens problem, implemented in OCaml" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) executes a search for possible arrangements
        of n queens on a chess board in such a way that they
        don't interfere with each other.";
    `P "The results are written to STDOUT"]
  in
  Term.(const nqueens $ num_queens),
  Term.info  "nqueens" ~version:"0.1.0" ~doc ~man


(* start main *)
let () =
  match Term.eval cmd with
  | `Error _  -> exit 1
  | _         -> exit 0


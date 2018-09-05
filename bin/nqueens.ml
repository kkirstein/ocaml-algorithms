(* vim: set ft=ocaml sw=2 ts=2: *)

(* benchmark.ml
 * A set of micro-benchmarks for the OCaml
 * programming language
*)

open Cmdliner

type position = {
  row: int;
  col: int
}

(* the backtracking algorithm *)
let check_position _queens _pos = 
  false

let backtrack _count =
  []


(* main entry point *)
let nqueens _n = 
  let tic = print_endline "Calculating.."; Sys.time () in
  let _solution = backtrack 8 in
  let toc = Sys.time () in
  Printf.printf " done (Elapsed time: %.3fs).\n" (toc -. tic)


(* cmdliner options *)
let num_queens =
  let doc = "The number of queens to be positioned" in
  Arg.(value & opt int 8 & info ["n"; "num-queens"] ~docv:"NUM_QUEENS" ~doc)

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


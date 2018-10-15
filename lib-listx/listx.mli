(* vim: set ft=ocaml sw=2 ts=2: *)

(**
 * listx.mli
 *
 * A module with utility functions for
 * list genertaion and processing
 *)

(**
 * generates a list of int values with given range
 * (start inclusive, stop exlcusive)
 *)
val range : int -> int -> int list


(**
 * splits the list into a parts at the given position
 *)
val split_list : int ->  'a list -> ('a list) * ('a list)


(**
 * extracts a sublist of the given indices (inclusive).
 * Returns an empty list, if out of bounds.
 *)
val sub_list : int -> int -> 'a list -> 'a list


(**
 * takes out the first element equal to a
 *)
val take_out_first : 'a -> 'a list -> 'a list


(**
 * random sampling of list entries
 *)
val sample_list : ?n:int -> 'a list -> 'a list




(* This file contains functions for memoization and explanations in French.
   Author: SeasayDev
   Date: 14-03-2025
*)

(** Memoization function provided by the statement
*)

let memo f =
  let memory = ref [] in
  fun x ->
    match !memory |> List.assoc_opt x with
    | None ->
      let y = f x in
      memory := (x, y) :: !memory;
      y
    | Some y -> y


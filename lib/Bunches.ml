(* This class represents portions of tapes.
   @author: SeasayDev.
   @Date: 14-03-2025
*)

open Tools
open Automata

(** This type allows visualizing a part of an automaton's tape
    specified by a start index and an end index.
*)
type bunches = int * int


(** This function returns, as a list, the segment of the automaton's tape.
*)
let get_bunch_values automata (inter : bunches) : 'a list =
  let (start, end_) = inter in
  let rec aux index acc =
    if index > end_ then List.rev acc
    else
      let value = automata.ribbon index in
      aux (index + 1) (value :: acc)
  in
  aux start []



(** This function converts a value to a string.
*)
let to_string automata ((start, end_) : bunches) converter =
  let rec aux index =
    if index > end_ then []
    else
      let value = automata.ribbon index in
      let str_value = converter value in
      str_value :: aux (index + 1)
  in
  String.concat "" (aux start)



(** This function tests if a portion of the tape contains a list 
    as a factor. 
    @param automata an automaton
    @param part a portion
    @param factor the list.
*)
let has_factor automata part factor =
  let ribbon_values = get_bunch_values automata part in
  is_factor_lists factor ribbon_values


(** This function tests if a portion of the tape contains a list 
    as a subword.
    @param automata an automaton
    @param part a portion
    @param subword the list.
*)
let has_subword automata part subword =
  let ribbon_values = get_bunch_values automata part in
  is_subword_lists subword ribbon_values

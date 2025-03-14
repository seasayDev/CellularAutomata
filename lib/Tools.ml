(* This class allows building the necessary tools for conducting this project.
   @author: seasayDev.
   @Date: 14-03-2025
*)


(** This function returns a list of integers.
    @param a an integer
    @param b an integer
    @return a list of integers
*)
let rec interval a b =
  match a > b with
  | true -> []
  | false -> a :: interval (a + 1) b


(** This function returns a list of strings representing 
    the elements of the list. 
    @param lst a list 
    @return a list of strings
*)
let rec list_to_string f lst =
  match lst with
  | [] -> ""
  | s ::st -> f s^list_to_string f st



(** This function generates a list interactively through 
    a function.
    @param f a function
    @param x a value 
    @param n an integer 
    @return a list of elements.
*)
let rec compose_iter f x n =
  if n = 0 then [x]  
  else if n < 0 then []  
  else
    let fin = compose_iter f (f x) (n - 1) in
    x :: fin  


(**
    This function tests if the second list is a prefix of the first.
    @param lst1 list 1
    @param lst2 list 2
    @return true or false
*)

let rec is_prefix_lists lst1 lst2 =
  match lst1, lst2 with
  | [], _any -> true  
  | _any, [] -> false  
  | m::ms, p::ps -> m = p && is_prefix_lists ms ps  



(** This function checks if the first list is a factor 
    of the second list. 
    @param lst1 list 1
    @param lst2 list 2
    @return true or false
*)
let rec is_factor_lists lst1 lst2 =
  match lst1, lst2 with
  | [], _any -> true  
  | _any, [] -> false  
  | _any ->
    if is_prefix_lists lst1 lst2 then true
    else match lst2 with
      | _::ys -> is_factor_lists lst1 ys
      | [] -> false


(** This function checks if the first list is a sublist
    of the second list. 
    @param lst1 list 1
    @param lst2 list 2
    @return true or false
*)
let rec is_subword_lists lst1 lst2 =
  match lst1, lst2 with
  | [], _any -> true  
  | _any, [] -> false  
  | x::xs, y::ys ->
    if x = y then is_subword_lists xs ys  
    else is_subword_lists lst1 ys  



(** This function checks if a list contains duplicates 
    in any of its elements. 
    @param lst a list 
    @return true or false
*)
let is_duplicate_free lst =
  let rec aux verif lst =
    match lst with
    | [] -> true  
    | x::xs ->
      if List.mem x verif then false  
      else aux (x::verif) xs  
  in
  aux [] lst

(** This function triggers a failure in the program.*)
let faire_echouer () =
  failwith "Unexpected value"

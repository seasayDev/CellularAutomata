(* This class represents automata.
   @author: SeasayDev.
   @Date: 14-03-2025
*)

(** This type represents automata using a generic record type.
    @param ribbon the automaton's tape
    @param evol the evolution function
    @param void the empty value
*)
type 'a automata = {
  ribbon : int -> 'a;
  evol : 'a * 'a * 'a -> 'a;
  void : 'a
}


(** This function returns an automaton with the specified evolution 
    function and empty value.
    @param evol the evolution function
    @param void the empty value
*)
let create evol void = {
  ribbon = (fun _any -> void);  
  evol = evol;               
  void = void;               
}


(** This function returns the value of the automaton's tape at the specified index.
    @param automata an automaton
    @param index the index
    @return the value at the position
*)
let get_value automata index =
  automata.ribbon index


(** This function returns an automaton considering its value at the specified index.
    @param automata an automaton
    @param index the index
    @return the edited value
*)
let set_value automata index value =
  {
    ribbon = (fun x -> if x = index then value else automata.ribbon x);
    evol = automata.evol;
    void = automata.void;
  }


(** This function returns the automaton obtained by shifting its 
    position to the left by the specified amount.
    @param automata an automaton
    @param k an integer
    @return an automaton
*)
let shift automata k =
  {
    ribbon = (fun x -> automata.ribbon (x + k));  
    evol = automata.evol;
    void = automata.void;
  }



(** This function returns the automaton obtained by reversing 
    the specified tape.
*)
let mirror automata =
  {
    ribbon = (fun x -> automata.ribbon (-x));  
    evol = automata.evol;
    void = automata.void;
  }

(** This function returns the automaton obtained by replacing 
    the tape values with their transformed counterparts.
    @param fn a function
    @param automata an automaton
    @return the transformed automaton
*)
let map fn automata =
  {
    ribbon = (fun x -> fn (automata.ribbon x));  
    evol = automata.evol;
    void = fn automata.void;  
  }


(** This function is a helper function for the automaton's 
    evolution class.*)
let evol automata =
  automata.evol

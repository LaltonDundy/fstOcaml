module Transducer = struct

(*
 * This module defines our transducer
 *)


    (*
     * Upper and Lower Relations
     *)
    type TRelation = string * string  

    (*
     * relations matched with certain state
     *)
    type Transitions = (TRelation * int) list 

    (*
     * List of all possible transitions for every state 
     *)
    type TTable  = ( int * Transitions ) list 

end

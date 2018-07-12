module RRelations = struct

(*
 * Module to create regular relations using ocaml's type system
 *)

    open Regular

    type rel = 
        | Cross of reg * reg
        | Comp  of rel * rel
        | Product  of rel * rel
        | UnionRel  of rel * rel
        | StarRel  of rel 
        | Id  of rel * rel
        | Relation of string * string 
        | EmptyRel


    (*
     * Union Operator
     *)

    let ( *|* ) r1 r2 = match (r1, r2) with
        | EmptyRel , r -> r
        | r , EmptyRel -> r
        | r1', r2'     -> UnionRel ( r1', r2' )

    (*
     * Product/concatenation operator
     *)
    let ( ** ) r1 r2 = match (r1, r2) with 
        | EmptyRel , r -> EmptyRel
        | r , EmptyRel -> EmptyRel 
        | r1', r2'     -> Product ( r1', r2' )

    (* 
     * Star operator
     *)
    let rec starRel = function
        | StarRel v -> starRel v
        | v         -> StarRel v

    (*
     * Plus operator
     *)
    let plusRel r = Product ( r , starRel r )

    (*
     * Cross Product
     *)
    let ( *%* ) r1 r2 = Cross ( r1 , r2 )

    (*
     * Composition operator
     *)
    let ( *@* ) r1 r2 = Comp ( r1 , r2 )
    (*
     * Relation operator
     *)
    let ( *$* ) str1 str2 = Relation ( str1 , str2 )

end
    

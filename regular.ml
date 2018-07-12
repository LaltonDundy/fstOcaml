module Regular = struct

(* 
 * All possibilities of what our 'a regular expressions could be. 
 * Represented using ocaml's type system
 *)

    type 'a reg = Empty
        | Epsilon
        | All 
        | Str of 'a 
        | Union of 'a reg * 'a reg
        | Concat of 'a reg * 'a reg
        | Intersect of 'a reg * 'a reg
        | Complement of 'a reg
        | Star of 'a reg

    let empty = Empty
    let eps   = Epsilon
    let all   = All
    let toReg s = Str s

    (*
     * Union operator
     *)
    let (<|>) r1 r2 = match (r1,r2) with
        | Empty , r -> r
        | r , Empty -> r
        | Star All , _   -> Star All
        | _   , Star All -> Star All
        | r1' , r2' -> Union (r1', r2')

    (* 
     * Concatenation operator
     *)
    let (<>) r1 r2  = Concat (r1,r2)

    (*
     * Intersection operator
     *)
    let (<&>) r1 r2 = match (r1, r2) with
        | Empty , _ -> Empty
        | _ , Empty -> Empty
        | Star All, a -> a
        | a , Star All -> a
        | r1', r2' -> Intersect (r1', r2')

    (*
     * Kleene star
     *)
    let rec star = function
        | Star r -> star r
        | r      -> Star r

    (*
     * Plus Operator
     *)
    let plus r = Concat ( r, (star r) )

    (*
     * Negation operator
     *)
    let neg = function
        | Empty -> Star All
        | Epsilon -> plus All
        | Star All -> Empty
        | Complement a -> a
        | a -> Complement a


    (*
     * Subtraction operator
     *)
    let (<->) r1 r2 = match (r1, r2) with
        | Empty, _ -> Empty
        | a , Empty -> a
        | a , b     -> if a = b then Empty 
                       else ( a <&> (neg b))


end

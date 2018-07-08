module Regular = struct

    type reg = Empty
        | Epsilon
        | All 
        | Str of string 
        | Union of reg * reg
        | Concat of reg * reg
        | Intersect of reg * reg
        | Complement of reg
        | Star of reg

    let empty = Empty
    let eps   = Epsilon
    let all   = All
    let toReg s = Str s

    let (<|>) r1 r2 = match (r1,r2) with
        | Empty , r -> r
        | r , Empty -> r
        | Star All , _   -> Star All
        | _   , Star All -> Star All
        | r1' , r2' -> Union (r1', r2')

    let (<>) r1 r2  = Concat (r1,r2)

    let (<&>) r1 r2 = match (r1, r2) with
        | Empty , _ -> Empty
        | _ , Empty -> Empty
        | Star All, a -> a
        | a , Star All -> a
        | r1', r2' -> Intersect (r1', r2')

    let star = function
        | Star r -> Star r
        | r      -> Star r

    let plus r = Concat ( r, (star r) )

    let neg = function
        | Empty -> Star All
        | Epsilon -> plus All
        | Star All -> Empty
        | Complement a -> a
        | a -> Complement a



    let (<->) r1 r2 = match (r1, r2) with
        | Empty, _ -> Empty
        | a , Empty -> a
        | a , b     -> if a = b then Empty 
                       else ( a <&> (neg b))


end

module type AutoSig = sig

    type 'a machine
    type state
    type 'a transitions = ('a * state) list
    type 'a transitionTable = (state * 'a transitions) list
    type 'a sigma = 'a list

    val states: 'a machine -> state list
    val isFinal: 'a machine -> state -> bool
    val finals: 'a machine -> state list
    val initials: 'a machine -> state list 
    val transitionList: 'a machine -> state -> 'a transitions
    val getTTable: 'a machine -> 'a transitionTable
    val getTransitions: 'a machine -> ( state * 'a ) -> state list
    val getFirstState: 'a machine -> state
    val getLastState: 'a machine -> state
    val alphabet: 'a machine -> 'a sigma 

end

module type MachineSig = sig

    type state
    type 'a transitions = ('a * state) list
    type 'a transTable = (state * 'a transitions) list
    type 'a sigma = 'a list
    type initials = state list
    type finals = state list
    type first  = state
    type last = state
    type 'a machine = 'a transTable * initials * finals * ('a sigma) * first * last

end

module AutoMachine (M: MachineSig) : AutoSig = struct

    type 'a machine = 'a  M.machine 
    type state   = M.state
    type 'a transitions = ('a * state) list
    type 'a transitionTable = (state * 'a transitions) list
    type 'a sigma = 'a list
    
    let states = function
        | (tt, _, _, _, _, _) -> tt |> (List.map fst)
    let isFinal mch s = match mch with
        | (_, _, fs, _, _, _) -> List.mem s fs
    let  finals = function
        | (_, _, fs, _, _, _) -> fs
    let initials = function
        | (_, it, _, _, _, _) -> it 
    let transitionList mch s = match mch with
        | (tt, _, _, _, _, _) -> ( try List.assoc s tt with 
                                    | _ -> [] )
    let getTTable = function
        | (tt, _, _, _, _, _) -> tt 

    let  getTransitions mch (s,a) = 
          s |> transitionList mch
            |> List.filter (fun (b,st) -> b = a)
            |> List.map    (fun (_, st) -> st) 

    let getFirstState = function
        | (_, _, _, _, frst, _) -> frst
    let  getLastState  = function
        | (_, _, _, _, _, last) -> last

    let  alphabet = function
        | (_, _, _, alpha, _, _ ) -> alpha



end




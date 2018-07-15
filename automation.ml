module type AutoSig = sig

    type state
    type 'a transitions = ('a * state) list
    type 'a transitionTable = (state * 'a transitions) list
    type 'a sigma = 'a list
    type 'a machine = 'a transitionTable * state list * state list * ('a sigma) * state * state

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


module AutoMachine : (AutoSig with type state = int ) = struct 

    type state   =  int
    type 'a transitions = ('a * state) list
    type 'a transitionTable = (state * 'a transitions) list
    type 'a sigma = 'a list
    type 'a machine = 'a transitionTable * state list * state list * ('a sigma) * state * state
    
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

module type RunRegAuto = sig

    type 'a machine
    type 'a regular
    type 'a sigma

    val build : 'a regular -> 'a sigma -> 'a machine

end




module RegAutomation  : RunRegAuto = struct
    

    open Regular
    open AutoMachine

    type 'a sigma = 'a AutoMachine.sigma
    type 'a machine = 'a AutoMachine.machine

    type 'a regular = 'a reg
    
    let stateHolder : state ref = ref 0

    let getState = 
        let () = 
            stateHolder := !stateHolder + 1
        in
        !stateHolder


    let build (re : 'a regular)  (sigm : 'a sigma) : 'a machine = match re with

    | Empty -> 
            let s = getState in

            ( [ (s, [] ) ] ,
              [],
              [],
              sigm,
              s,
              s ) 

    | Epsilon -> 
            let s = getState in

            ( [ (s, [] ) ] ,
              [],
              [s],
              sigm,
              s,
              s )

    | Str a -> 
            let s1 = getState in
            let s2 = getState in

            ( [ (s1 , [ ( a , s2) ] ) ; ( s2 , [] ) ] , 
              [],
              [s2],
              sigm,
              s1,
              s2 )





                
    | _ -> raise (Failure "ahhhh")

end

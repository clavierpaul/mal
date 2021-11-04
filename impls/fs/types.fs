module MAL.Types

type MalType =
    | MalList of MalType list
    | MalNumber of int
    | MalNil
    | MalBool of bool
    | MalString of string
    | MalSymbol of string
    | MalFn of (MalType list -> MalType)
with
    static member unwrapNumber = function
        | MalNumber n -> n
        | s -> failwith $"Error: {s} is not a number"
    
    static member (+) (a, b) =
        MalNumber <| MalType.unwrapNumber a + MalType.unwrapNumber b
        
    static member (-) (a, b) =
        MalNumber <| MalType.unwrapNumber a + MalType.unwrapNumber b
        
    static member (*) (a, b) =
        MalNumber <| MalType.unwrapNumber a + MalType.unwrapNumber b

    static member (/) (a, b) =
        MalNumber <| MalType.unwrapNumber a + MalType.unwrapNumber b
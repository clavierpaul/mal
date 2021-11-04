module MAL.Types

type MalType =
    | MalList of MalType list
    | MalNumber of int
    | MalNil
    | MalBool of bool
    | MalSymbol of string
    | MalFn of (MalType list -> MalType)

let unwrapNumber = function
    | MalNumber n -> n
    | s -> failwith $"Error: {s} is not a number"

let (+) a b = MalNumber <| unwrapNumber a + unwrapNumber b

let (-) a b = MalNumber <| unwrapNumber a - unwrapNumber b

let (*) a b = MalNumber <| unwrapNumber a * unwrapNumber b
    
let (/) a b = MalNumber <| unwrapNumber a / unwrapNumber b
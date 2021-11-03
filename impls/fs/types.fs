module MAL.Types

type MalType =
    | MalNumber of int
    | MalSymbol of string
    | MalList of MalType list
    | MalFn of (MalType list -> MalType)

let unwrapNumber = function
    | MalNumber n -> n
    | s -> failwith $"Error: {s} is not a number"

let (+) a b = MalNumber <| unwrapNumber a + unwrapNumber b

let (-) a b = MalNumber <| unwrapNumber a - unwrapNumber b

let (*) a b = MalNumber <| unwrapNumber a * unwrapNumber b
    
let (/) a b = MalNumber <| unwrapNumber a / unwrapNumber b
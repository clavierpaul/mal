module MAL.Types

type MalType =
    | MalNumber of int
    | MalSymbol of string
    | MalList of MalType list

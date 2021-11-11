module MAL.Types

type MalType =
    | MalList of MalType list
    | MalVector of MalType list
    | MalNumber of int
    | MalNil
    | MalBool of bool
    | MalString of string
    | MalHashmap of Map<MalType, MalType>
    | MalKeyword of string
    | MalSymbol of string
    | MalFn of MalFn
    | MalMacro of MalMacro
    
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
    
and
    [<CustomEquality; CustomComparison>]
    MalFn =
        | MalFn of (MalType list -> MalType)
        
        override x.Equals _ = invalidArg "x" "Attempted to compare with a function"
        override x.GetHashCode () = invalidArg "x" "Attempted to hash a function"
        
        interface System.IComparable with
            member x.CompareTo _ = invalidArg "x" "Attempted to compare with a function"
        
and MalMacro =
    | MacroQuote of MalType
    | MacroQuasiquote of MalType
    | MacroUnquote of MalType
    | MacroSpliceUnquote of MalType
    | MacroDeref of MalType
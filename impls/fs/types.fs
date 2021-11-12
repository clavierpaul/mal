module MAL.Types

// TODO: Monads? Functors?
[<CustomEquality; CustomComparison>]
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
    | MalFn of (MalType list -> MalType)
    | MalMacro of MalMacro
    
    static member private UseComparison f a b =
        match a, b with
        | MalString  a, MalString  b -> f a b
        | MalString  a, MalKeyword b -> f a b
        | MalKeyword a, MalString  b -> f a b
        | MalKeyword a, MalKeyword b -> f a b
        | a, _ -> invalidArg "a" $"Comparison not implemented for type {a.GetType}"
        
    static member private UnwrapNumber = function
        | MalNumber n -> n
        | s -> invalidArg "s" $"{s} is not a number"
    
    static member (+) (a, b) =
        MalNumber <| MalType.UnwrapNumber a + MalType.UnwrapNumber b
        
    static member (-) (a, b) =
        MalNumber <| MalType.UnwrapNumber a + MalType.UnwrapNumber b
        
    static member (*) (a, b) =
        MalNumber <| MalType.UnwrapNumber a + MalType.UnwrapNumber b

    static member (/) (a, b) =
        MalNumber <| MalType.UnwrapNumber a + MalType.UnwrapNumber b
    
    override x.Equals yObj =
        match yObj with
        | :? MalType as y -> MalType.UseComparison (=) x y
        | _ -> false
    
    override x.GetHashCode () =
        match x with
        | MalString s -> s.GetHashCode ()
        | MalKeyword s -> s.GetHashCode ()
        | t -> invalidArg "t" $"Hash code not implemented for type {t.GetType}"
    
    interface System.IComparable with
        member x.CompareTo yObj =
            match yObj with
            | :? MalType as y -> MalType.UseComparison compare x y
            | _ -> invalidArg "yObj" "Object is not of MalType"
        
and MalMacro =
    | MacroQuote of MalType
    | MacroQuasiquote of MalType
    | MacroUnquote of MalType
    | MacroSpliceUnquote of MalType
    | MacroDeref of MalType
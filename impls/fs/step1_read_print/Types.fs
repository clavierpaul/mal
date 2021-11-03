module MAL.Types

type MalType =
    | MalNumber of int
    | MalSymbol of string
    | MalList of MalType list
    
let rec displayType t =
    match t with
    | MalNumber n -> string n
    | MalSymbol s -> s
    | MalList l ->
        let displayedList = List.map displayType l |> String.concat " "
        "(" + displayedList + ")"
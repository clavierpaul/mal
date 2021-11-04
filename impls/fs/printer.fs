module MAL.Printer

open Types

let rec pr_str = function
    | MalNumber n -> string n
    | MalSymbol s -> s
    | MalBool b -> if b then "true" else "false"
    | MalNil -> "nil"
    | MalFn f -> "<function>"
    | MalList l ->
        let displayedList = List.map pr_str l |> String.concat " "
        $"({displayedList})"
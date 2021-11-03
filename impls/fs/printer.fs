module MAL.Printer

open Types

let rec pr_str = function
    | MalNumber n -> string n
    | MalSymbol s -> s
    | MalFn f -> "<function>"
    | MalList l ->
        let displayedList = List.map pr_str l |> String.concat " "
        $"({displayedList})"
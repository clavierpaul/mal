module MAL.Printer

open Types

let rec pr_str data =
    match data with
    | MalNumber n -> string n
    | MalSymbol s -> s
    | MalList l ->
        let displayedList = List.map pr_str l |> String.concat " "
        "(" + displayedList + ")"
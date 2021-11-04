module MAL.Printer

open Types

let transformReadable str char =
    match char with
    | '\n' -> str + "\\n"
    | '\\' -> str + "\\\\"
    | '"'  -> str + "\\\""
    | c -> str + string c
    
    
let rec pr_str print_readably = function
    | MalNumber n -> string n
    | MalSymbol s -> s
    | MalBool b -> if b then "true" else "false"
    | MalNil -> "nil"
    | MalFn f -> "<function>"
    | MalString s ->
        if print_readably then
            let transformed = Seq.fold transformReadable "" s
            $"\"{transformed}\""
        else
            $"\"{s}\""
    | MalList l ->
        let displayedList = List.map (pr_str print_readably) l |> String.concat " "
        $"({displayedList})"
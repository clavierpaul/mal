module MAL.Printer

open Types

let transformReadable str char =
    match char with
    | '\n' -> str + "\\n"
    | '\\' -> str + "\\\\"
    | '"'  -> str + "\\\""
    | c    -> str + string c
    
let rec pr_str print_readably = function
    | MalNumber n     -> string n
    | MalSymbol s     -> s
    | MalBool b       -> if b then "true" else "false"
    | MalNil          -> "nil"
    | MalList l       -> printList print_readably "(" ")" l
    | MalVector v     -> printList print_readably "[" "]" v
    | MalMacro m      -> "(" + printMacro print_readably m + ")"
    | MalKeyword k    -> $":{k}"
    | MalType.MalFn _ -> "<function>" // Why does this require a qualified name??????
    | MalHashmap h    -> "{" + printHashmap print_readably h + "}"
    | MalString s ->
        if print_readably then
            let transformed = Seq.fold transformReadable "" s
            $"\"{transformed}\""
        else
            $"\"{s}\""

and printList print_readably sOpen sClose l =
    let displayedList = List.map (pr_str print_readably) l |> String.concat " "
    $"{sOpen}{displayedList}{sClose}"
    
and printHashmap print_readably h =
    let displayKeyValue (k, v) =
        (pr_str print_readably k) + " " + (pr_str print_readably v)
    h
    |> Map.toList
    |> List.map displayKeyValue
    |> String.concat " "

and printMacro print_readably = function
    | MacroQuote t         -> $"quote {pr_str print_readably t}"
    | MacroQuasiquote t    -> $"quasiquote {pr_str print_readably t}"
    | MacroUnquote t       -> $"unquote {pr_str print_readably t}" 
    | MacroSpliceUnquote t -> $"splice-unquote {pr_str print_readably t}"
    | MacroDeref t         -> $"deref {pr_str print_readably t}"


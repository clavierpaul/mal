open System
open MAL

let READ = Reader.read_str
let EVAL ast = ast
let PRINT = Printer.pr_str
let rep input = input |> READ |> EVAL |> PRINT true

let rec programLoop () =
    printf "user> "
    let input = Console.ReadLine ()
    match input with
    | null -> ()
    | s ->
        try
            printfn $"{rep s}"
        with
            | Failure msg -> error $"{msg}"
            | :? Exception as msg -> error $"{msg}"
        programLoop ()

[<EntryPoint>]
let main argv =
    programLoop ()
    0

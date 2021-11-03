open System

let READ input = input
let EVAL ast = ast
let PRINT value = value
let rep input = input |> READ |> EVAL |> PRINT

let rec programLoop () =
    printf "user> "
    let input = Console.ReadLine ()
    match input with
    | null -> ()
    | s ->
        printfn $"{rep s}"
        programLoop ()

[<EntryPoint>]
let main argv =
    programLoop ()
    0

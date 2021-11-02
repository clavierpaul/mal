open System
open MAL 

let rec programLoop () =
    printf "user> "
    let input = Console.ReadLine ()
    match input with
    | null -> ()
    | s ->
        printfn $"{Commands.rep s}"
        programLoop ()

[<EntryPoint>]
let main argv =
    programLoop ()
    0

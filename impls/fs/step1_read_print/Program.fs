open System
open MAL
open FParsec
open Reader

let rec programLoop () =
    printf "user> "
    let input = Console.ReadLine ()
    match input with
    | null -> ()
    | s ->
        printfn $"{Commands.rep s}"
        programLoop ()

let test str =
    printfn $"Success: %A{Types.displayType <| read_str str}"

[<EntryPoint>]
let main argv =
    let tests = ["123"; "123 "; "abc"; "abc "; "(123 456)"; "( (123 456 789 )"; "( + 2 (* 3 4) )"]
    tests |> List.iter test
    0
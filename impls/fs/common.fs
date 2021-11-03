[<AutoOpen>]
module MAL.Common

open System

let error msg =
    Console.ForegroundColor <- ConsoleColor.Red
    printf "Error: "
    Console.ResetColor ()
    printfn msg
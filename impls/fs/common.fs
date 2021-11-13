[<AutoOpen>]
module Mal.Common

open System

let rec pair = function
    | [] | [_] -> []
    | x1::x2::tail -> (x1, x2) :: (pair tail)

let error msg =
    Console.ForegroundColor <- ConsoleColor.Red
    printf "Error: "
    Console.ResetColor ()
    printfn msg
    
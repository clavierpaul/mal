module MAL.Commands

// Stubs
let READ input = input
let EVAL ast = ast
let PRINT value = value
let rep input = input |> READ |> EVAL |> PRINT

module MAL.Commands

let READ = Reader.read_str
let EVAL ast = ast
let PRINT = Printer.pr_str
let rep input = input |> READ |> EVAL |> PRINT

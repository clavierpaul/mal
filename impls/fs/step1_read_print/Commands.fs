module MAL.Commands

// Stubs
let read = Reader.read_str
let eval ast = ast
let print = Printer.pr_str
let rep input = input |> read |> eval |> print

open System
open Mal
open Types

// These will be more elegantly wrapped in step 3
type ReplEnv = Map<string, MalType list -> MalType>

let malFunc2From operator = function
    | [a; b] -> operator a b
    | l -> failwith $"Operator expected 2 arguments but was given {List.length l}"

let repl_env: ReplEnv =
    [ ("+", malFunc2From (+))
      ("-", malFunc2From (-))
      ("*", malFunc2From (*))
      ("/", malFunc2From (/)) ] |> Map.ofList

let READ = Reader.read_str

let rec EVAL (env: ReplEnv) = function
    | MalList    l -> eval_list env l
    | MalVector  v -> MalVector  <| List.map (EVAL env) v
    | MalHashmap h -> MalHashmap <| Map.map (fun _ -> EVAL env) h
    | ast -> ast |> eval_ast env 

and eval_list env = function
    | [] -> MalList []
    | l  ->
        let newList =
            match eval_ast env <| MalList l with
            | MalList l -> l
            | _         -> failwith "Result of list eval was not a list"

        match List.head newList with
        | MalFn fn -> fn <| List.tail newList
        | s        -> failwith $"\"{Printer.pr_str true s}\" is not a function"

and eval_ast (env: ReplEnv) ast =
    let env_lookup symbol =
        match repl_env |> Map.tryFind symbol with
        | Some f -> f
        | None -> failwith $"Unknown symbol \"{symbol}\""
        
    match ast with
    | MalSymbol s -> MalFn <| env_lookup s
    | MalList l   -> MalList <| List.map (EVAL env) l
    | ast         -> ast
    
let PRINT = Printer.pr_str

let rep input =
    match input |> READ with
    | Some program -> program |> EVAL repl_env |> PRINT true
    | None -> ""

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
        programLoop ()

[<EntryPoint>]
let main argv =
    programLoop ()
    0

open System
open MAL
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
    | MalList [] -> MalList []
    | MalList l ->
        let newList =
            match eval_ast env <| MalList l with
            | MalList l -> l
            | _ -> failwith "Result of list eval was not a list"

        match List.head newList with
        | MalFn fn -> fn <| List.tail newList
        | s -> failwith $"\"{Printer.pr_str s}\" is not a function"
        
    | ast -> ast |> eval_ast env 

and eval_ast (env: ReplEnv) ast =
    let env_lookup symbol =
        match env.TryFind symbol with
        | Some f -> f
        | None -> failwith $"Unknown symbol \"{symbol}\""
        
    match ast with
    | MalSymbol s -> MalFn <| env_lookup s
    | MalList l   -> MalList <| List.map (EVAL env) l
    | ast         -> ast
    
let PRINT = Printer.pr_str

let rep input = input |> READ |> EVAL repl_env |> PRINT

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
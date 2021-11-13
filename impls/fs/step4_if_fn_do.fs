open System
open Mal
open Types
open State
let malFunc2From operator = function
    | [a; b] -> operator a b
    | l -> failwith $"Operator expected 2 arguments but was given {List.length l}"

let repl_env: Env =
    [ ("+", MalFn <| malFunc2From (+))
      ("-", MalFn <| malFunc2From (-))
      ("*", MalFn <| malFunc2From (*))
      ("/", MalFn <| malFunc2From (/)) ]
    |> Map.ofList
    |> Env.ofMap

let READ = Reader.read_str

let rec EVAL env ast: MalType =
    match ast with
    | MalList    l -> eval_list env l
    | MalVector  v -> MalVector  <| List.map (EVAL env) v
    | MalHashmap h -> MalHashmap <| Map.map (fun _ -> EVAL env) h
    | ast -> eval_ast env ast 

and addBinding env (key, value) =
    let key = unwrapSymbol key
    let evaluated = EVAL env value
    do env |> Env.set key evaluated
    evaluated

and eval_list env l =
    match l with
    | [] -> MalList []
    | MalSymbol "def!"::tail ->
        match tail with
        | [key; value] -> addBinding env (key, value)
        | _ -> failwith "def! expected 2 arguments"
    | MalSymbol "let*"::tail ->
        match tail with
        | [bindings; expr]->
            let bindings = unwrapSeq bindings
            if List.length bindings % 2 <> 0 then
                failwith "Binding list contains an odd number of elements"
            else
                let subEnv = Env.fromOuter env
                let bindings = pair bindings
                do bindings |> List.map (addBinding subEnv) |> ignore
                EVAL subEnv expr
        | _ -> failwith "let* expected 2 arguments"
    | MalSymbol "if"::tail ->
        match tail with
        | [condition; ifTrue] ->
            match EVAL env condition with
            | MalBool false | MalNil -> MalNil
            | _ -> EVAL env ifTrue
        | [condition; ifTrue; ifFalse] ->
            match EVAL env condition with
            | MalBool false | MalNil -> EVAL env ifFalse
            | _ -> EVAL env ifTrue
        | _ -> failwith "if expected 2 or 3 arguments"
    | l ->
        let result = eval_ast env <| MalList l
        let newList = unwrapList result
        
        match List.head newList with
        | MalFn fn -> fn <| List.tail newList
        | s        -> failwith $"\"{Printer.pr_str true s}\" is not a function"
            
and eval_ast env ast =
    match ast with
    | MalSymbol s -> env |> Env.get s
    | MalList l   -> let nl = l |> List.map (EVAL env) in MalList nl
    | ast         -> ast
    
let PRINT = Printer.pr_str

let rep input =
    match input |> READ with
    | Some program -> EVAL repl_env program |> PRINT true
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
            | Failure msg ->
                error $"{msg}"
                
        programLoop ()

[<EntryPoint>]
let main _ =
    programLoop ()
    0

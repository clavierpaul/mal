open System
open MAL
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

let rec EVAL ast: State<Env, MalType> =
    state {
        match ast with
        | MalList    l -> return! eval_list l
        | MalVector  v ->
            let! v = State.mapList EVAL v
            return MalVector v
        | MalHashmap h ->
            let mapEval (k, ast) = state {
                let! v = EVAL ast
                return (k, v)
            }
            
            // This code is very efficient!
            let l = Map.toList h
            let! nl = State.mapList mapEval l
            return MalHashmap <| Map.ofList nl
        | ast -> return! eval_ast ast 
    }

and addBinding (key, value) =
    state {
        let key = unwrapSymbol key
        let! evaluated = EVAL value
        do! Env.set (key, evaluated)
        return evaluated
    }

and eval_list l =
    state {
        match l with
        | [] -> return MalList []
        | l  ->
            match l with
            | MalSymbol "def!"::tail ->
                if List.length tail < 2 then
                    return failwith "def! requires 2 arguments"
                else
                    return! addBinding (tail.[0], tail.[1])
            | MalSymbol "let*"::tail ->
                if List.length tail < 2 then
                    return failwith "let* requires 2 arguments"
                else
                    let! currentEnv = State.get
                    let bindings = unwrapList tail.[0]
                    if List.length bindings % 2 <> 0 then
                        return failwith "Binding list contains an odd number of elements"
                    else
                        do! State.put <| Env.ofEnv currentEnv
                        let bindings = pair bindings
                        let! _ = bindings |> State.mapList addBinding
                        let! result = EVAL tail.[1]
                        do! State.put currentEnv
                        return result
            | l ->
                let! result = eval_ast <| MalList l
                let newList = unwrapList result
                
                match List.head newList with
                | MalFn fn -> return fn <| List.tail newList
                | s        -> return failwith $"\"{Printer.pr_str true s}\" is not a function"
    }

and eval_ast ast =
    state {
        match ast with
        | MalSymbol s -> return! Env.get s
        | MalList l   -> let! nl = l |> State.mapList EVAL in return MalList nl
        | ast         -> return ast
    }
    
let PRINT = Printer.pr_str

let rep input env =
    match input |> READ with
    | Some program ->
        let result, newEnv = State.run env (EVAL program)
        (PRINT true result), newEnv
    | None -> "", env

let rec programLoop env =
    printf "user> "
    let input = Console.ReadLine ()
    match input with
    | null -> ()
    | s ->
        try
            let output, newEnv = rep s env
            printfn $"{output}"
            programLoop newEnv
        with
            | Failure msg ->
                error $"{msg}"
                programLoop env

[<EntryPoint>]
let main _ =
    programLoop repl_env
    0

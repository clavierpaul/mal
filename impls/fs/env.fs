namespace Mal

open Types
open State

type ReplEnv = Map<string, MalType>

type Env =
    { Outer: Env option
      mutable Data: ReplEnv }
    
module Env =
    let ofMap map = 
        { Outer = None
          Data = map }
    
    let fromOuter env = 
        { Outer = Some env
          Data = Map.empty }

    let set key value env =
        env.Data <- env.Data |> Map.add key value
    
    let rec find key env =
        match env.Data |> Map.tryFind key with
        | Some _ -> (Some env)
        | None ->
            match env.Outer with
            | Some outer -> find key outer
            | None -> None

    let rec get key env =
        match find key env with
        | Some e -> e.Data |> Map.find key
        | None -> failwith $"'{key}' not found"
    
    let init binds exprs =
        let setter env (k, v) =
            env |> set k v
        
        let paired = List.map2 (fun b e -> (b, e)) binds exprs
        let env = { Outer = None; Data = Map.empty }

        do List.iter (setter env) paired
        env
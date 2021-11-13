namespace MAL

open MAL
open Types
open Printer
open State

type ReplEnv = Map<string, MalType>

type Env =
    { outer: Env option
      data: ReplEnv }
    
module Env =
    let ofMap map = 
        { outer = None
          data = map }
    
    let ofEnv env = 
        { outer = Some env
          data = Map.empty }

    let set (key, value) =
        let set' env =
            { env with data = Map.add key value env.data }
        
        State (fun env -> (), set' env)

    let rec private find' key env =
        match env.data |> Map.tryFind key with
        | Some _ -> (Some env)
        | None ->
            match env.outer with
            | Some outer -> find' key outer
            | None -> None

    let find key = State (fun env -> (find' key env), env)

    let get key =
        let rec get' env =
            match find' key env with
            | Some e -> (e.data |> Map.find key), env
            | None -> failwith $"'{key}' not found"
        
        State get'
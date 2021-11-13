module MAL.State

type State<'state, 'result> = State of ('state -> 'result * 'state)

module State =
    let inline run state x = let (State(f)) = x in f state
    let get = State (fun s -> s, s)
    let put newState = State (fun _ -> (), newState)
    let map f s = State (fun (state: 's) ->
        let x, state = run state s
        f x, state)

    /// The state monad passes around an explicit internal state that can be
    /// updated along the way. It enables the appearance of mutability in a purely
    /// functional context by hiding away the state when used with its proper operators
    /// (in StateBuilder()). In other words, you implicitly pass around an implicit
    /// state that gets transformed along its journey through pipelined code.
    type StateBuilder() =
        member this.Zero () = State(fun s -> (), s)
        member this.Return x = State(fun s -> x, s)
        member inline this.ReturnFrom (x: State<'s, 'a>) = x
        member this.Bind (x, f) : State<'s, 'b> =
            State(fun state ->
                let (result: 'a), state = run state x
                run state (f result))
        member this.Combine (x1: State<'s, 'a>, x2: State<'s, 'b>) =
            State(fun state ->
                let _, state = run state x1
                run state x2)
        member this.Delay f: State<'s, 'a> = f ()
        member this.For (seq, f: 'a -> State<'s, 'b>) =
            seq
            |> Seq.map f
            |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))
        member this.While (f, x) =
            if f () then this.Combine (x, this.While (f, x))
            else this.Zero ()
    
    // Maps a stateful function to a list and folds the returned values into a new list
    let rec mapList f l =
        let state = StateBuilder()
        state {
            match l with
            | []         -> return []
            | head::tail ->
                let! x = f head
                let! xs = mapList f tail
                return x :: xs
        }
        
let state = State.StateBuilder()


module MAL.State

type State<'s, 'a> = State of ('s -> 'a * 's)

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
            let (result: 'a), state = State.run state x
            State.run state (f result))
    member this.Combine (x1: State<'s, 'a>, x2: State<'s, 'b>) =
        State(fun state ->
            let _, state = State.run state x1
            State.run state x2)
    member this.Delay f: State<'s, 'a> = f ()
    member this.For (seq, f: 'a -> State<'s, 'b>) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))
    member this.While (f, x) =
        if f () then this.Combine (x, this.While (f, x))
        else this.Zero ()
    member this.MapList f l =
        let state = StateBuilder ()
        let rec mapList l =
            state {
                match l with
                | []         -> return []
                | head::tail ->
                    let! x = f head
                    let! xs = mapList tail
                    return x :: xs
            }
         
        mapList l
        
let state = StateBuilder()

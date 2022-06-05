module SulFer.Engine.Actor

open System.Collections.Generic
open Akkling

type private SubSolverState = {
    board: List<List<uint option>>
    point: int * int
    posibles: uint list
}

type SolverMsg =
    | Start of board: uint option list list
    | Answer of point: (int * int) * value: uint

type private SubSolverMsg =
    | Init
    | Update of (int * int) * uint

type private SolverState =
    | Pending
    | Started of board: List<List<uint option>> * children: Map<(int * int), IActorRef<SubSolverMsg>>


let private bound i =
    let rec inner c i =
        if c = i then
            (c, c + 2)
        elif c > i then
            (c - 3, c - 1)
        else
            inner (c + 3) i
    inner 0 i

let private filterBySameRowAndColumn {board = board; point = (r, c); posibles = posibles} =
    seq {0 .. 8}
    |> Seq.fold (fun acc idx ->
        acc
        |> List.filter (fun n ->
            match board[r][idx] with
            | Some v when v = n -> false
            | None ->
                match board[idx][c] with
                | Some v when v = n -> false
                | _ -> true
            | _ -> true
        )
    ) posibles

 
let private blockBound (r, c) =
    (bound r), (bound c)

let private filterBySameBlock ((rowLower, rowUpper), (colLower, colUpper)) {board = board; point = (r, c); posibles = posibles} =
    [
        for i in rowLower .. rowUpper do
            if not(i = r) then
                for j in colLower .. colUpper do
                    if not(j = c) then
                        yield (i, j)
    ]
    |> Seq.fold (fun acc (i, j) ->
        acc
        |> List.filter (fun n ->
            match board[i][j] with
            | Some v when v = n -> false
            | _ -> true
        )
    ) posibles

let rec private subSolver (state, blockBoundOpt) (ctx: Actor<'a>) = function
    | Init ->
        let blockBound = blockBound state.point
        let s =
            [filterBySameRowAndColumn; filterBySameBlock blockBound]
            |> Seq.fold (fun s f ->
                match s with
                | { posibles = [_] } ->
                    s
                | _ -> {s with posibles = f s}
            ) state
        match s with
        | {board = _; point = point; posibles = [h]} ->
            let answer =  Answer (point = point, value = h)
            printfn "got answer %A" answer
            ctx.Sender() <! answer
            stop()
        | _ -> 
            become (subSolver (s, Some blockBound) ctx)
    | Update ((x, y), n) ->
        match (state.point, blockBoundOpt) with
        | ((r, c), (Some ((rl, ru), (cl, cu)))) when x = r || y = c || ((rl <= x && x <= ru) && (cl <= y && y <= cu)) ->
            let filtered = state.posibles |> List.filter (n |> (=) >> not)
            match filtered with
            | [h] ->
                let answer = Answer (point = (r, c), value = h)
                printfn "got answer %A" answer
                ctx.Sender() <! answer
                stop()
            | _ -> become (subSolver ({state with posibles = filtered}, blockBoundOpt) ctx)
        | _ -> ignored()


let rec private solverInner state (ctx: Actor<'a>) = fun msg ->
    match state with
    | Pending ->
        match msg with
        | Start board ->
            let mutBoard = List()
            let toSolve =
                [
                    for i in 0 .. 8 do
                        let row = List()
                        for j in 0..8 do
                            let n = board[i][j]
                            row.Add(n)
                            if Option.isNone n then
                                yield (i, j)
                        mutBoard.Add(row)
                ]
            let posibles = [1 .. 9] |> List.map uint
            let children =
                [
                    for p in toSolve do
                        let (x, y) = p
                        let child = spawn ctx $"subsolver-{x}_{y}" <| props(actorOf2 (subSolver ({board = mutBoard; point = p; posibles = posibles}, None)))
                        child <! Init
                        yield p, child
                ]
                |> Map.ofList
            become (solverInner (Started (board = mutBoard, children = children)) ctx)
        | _ -> ignored()
    | Started (board, children) ->
        match msg with
        | Answer ((x, y), value) ->
            board[x][y] <- Some value
            let c =
                children
                |> Map.filter (fun k _ -> k <> (x, y))
            if Map.isEmpty c then
                for r in board do
                    let ns = 
                        r.ToArray()
                        |> Array.map Option.get
                        |> Array.map string
                        |> String.concat " "
                    printfn "%A" ns
                stop()
            else
                c
                |> Map.values
                |> Seq.iter (fun a ->
                    a <! Update ((x, y), value)
                )
                become (solverInner (Started (board = board, children = c)) ctx)
        | _ -> ignored()

let solver ctx =
    solverInner Pending ctx

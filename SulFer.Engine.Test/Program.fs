open SulFer.Engine.Actor
open Akkling

module Program =

    [<EntryPoint>]
    let main _ =
        use system = System.create "sudoku-system" (Configuration.defaultConfig())
        let aref = spawn system "solver" (props (actorOf2 solver))
        aref <! Start ([
            [ 3;  6;  7; -1; -1;  9; -1;  5;  8];
            [-1;  2; -1;  5; -1;  1;  7; -1; -1];
            [ 5;  9; -1;  8; -1;  7; -1;  2;  6];
            [-1;  3;  9; -1; -1; -1;  2;  1;  4];
            [ 6;  4;  8;  1;  7;  2;  9; -1;  5];
            [ 1;  5;  2; -1;  9;  4; -1;  6;  7];
            [ 4;  1;  6;  9;  8;  3;  5;  7;  2];
            [ 2; -1;  3;  7;  1; -1;  6;  4;  9];
            [-1;  7;  5;  2; -1; -1; -1;  8;  1]
        ]
        |> List.map (fun l ->
            l
            |> List.map (fun i ->
                if i = -1 then
                    None
                else
                    Some (uint i)
            )
        ))
        system.WhenTerminated
        |> Async.AwaitTask
        |> Async.RunSynchronously
        0

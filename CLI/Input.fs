module private Input

open System
open Minesweeper.Utils
open Minesweeper.Game

[<TailCall>]
let rec private readRawLocation () =
    printf "Enter location (y x):"
    Console.ReadLine()
    |> _.Split(' ')
    |> Array.map int
    |> function
        | [| y; x |] -> { Y = y; X = x }
        | _ -> 
            printfn "Invalid format."
            readRawLocation ()

[<TailCall>]
let rec private readLocation validityChecker =
    match readRawLocation () with
    | location when validityChecker location -> location
    | _ ->
        printfn "Invalid location."
        readLocation validityChecker

[<TailCall>]
let rec readAction locationChecker historyExists =
    let mutable actions = Map [
        ('o', "open");
        ('m', "mark");
    ]
    if historyExists then
        actions <- Map.add 'u' "undo" actions

    actions
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> sprintf "'%c' for %s" k v)
    |> String.concat ", "
    |> printfn "Enter action (%s): "

    match Console.ReadKey().KeyChar with
    | 'o' -> Open (readLocation locationChecker)
    | 'm' -> Mark (readLocation locationChecker)
    | 'u' when historyExists -> Undo
    | _ ->
        printfn "Invalid action."
        readAction locationChecker historyExists
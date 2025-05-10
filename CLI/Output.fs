module private Output

open System
open Minesweeper.Board
open Minesweeper.Utils

let private printBoard (board: Board) =
    let { Height = height; Width = width } = board.Dimensions
    
    Seq.init width id
    |> Seq.map ((+)(int '0') >> char)
    |> Seq.toArray
    |> String
    |> printfn " |%s"
    printfn "-+%s" (String.replicate width "-")

    for y in 0..height-1 do
        board.GetRow y
        |> Seq.map (
            function
            | Closed -> '#'
            | Marked -> '+'
            | Opened 0 -> ' '
            | Opened n -> char (int '0' + n)
        )
        |> Seq.toArray
        |> String
        |> printfn "%d|%s" y

let printState board leftMinesCount =
    Console.Clear()
    printBoard board

    printfn "Mines left: %d" leftMinesCount
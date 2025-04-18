﻿open Minesweeper.Board
open Minesweeper.Utils
open System

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

type private UserAction =
    | Open of Location
    | Mark of Location

[<TailCall>]
let rec private inputAction () =
    printf "Enter y and x: "

    Console.ReadLine()
    |> _.Split(' ')
    |> function
    | [| y; x |] -> Open { Y = int y; X = int x }
    | [| y; x; "m" |] -> Mark { Y = int y; X = int x }
    | _ -> 
        printfn "Invalid input. Please enter two integers separated by a space."
        inputAction ()

[<TailCall>]
let rec gameLoop board =
    Console.Clear()
    printBoard board
    printfn "Mines left: %d" board.UnmarkedMinesCount

    if board.UnmarkedMinesCount > 0 then    
        inputAction ()
        |> function
        | Open location -> openCell board location
        | Mark location -> markCell board location
        |> gameLoop

let minesCount = 10
let dimensions = { Height = 10; Width = 10 }

try
    Board.Create dimensions minesCount
    |> gameLoop
with
| MineOpened location ->
    printfn "Game over! You opened the mine at %O" location
open Minesweeper.Board
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
        board.VisibleCells[y, *]
        |> Seq.map (
            function
            | Closed -> '#'
            | Marked -> '+'
            | Opened 0 -> ' '
            | Opened n -> char (int '0' + n)
        )
        |> Seq.toArray
        |> String
        |> printfn "%d| %s" y

let private inputLocation () =
    printf "Enter y and x: "
    let rowValues = 
        Console.ReadLine()
        |> _.Split(' ')
        |> Array.map int

    { Y = rowValues.[0]; X = rowValues.[1] }

let private playGame dimensions minesCount =
    let rec gameLoop board =
        Console.Clear()
        printBoard board

        if board.LeftoverMines <> 0 then    
            inputLocation ()
            |> openCell board
            |> gameLoop

    inputLocation ()
    |> generateValidBoard (createEmptyBoard dimensions) minesCount
    |> gameLoop

let dimensions = { Height = 10; Width = 10 }
let minesCount = 10

try
    playGame dimensions minesCount
with
| MineOpened location ->
    printfn "Game over! You opened a mine at %A" location
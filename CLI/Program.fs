open Minesweeper.Board
open Minesweeper.Minefield

let PrintBoard (board: Board) =
    let height, width = Array2D.length1 board.Tiles, Array2D.length2 board.Tiles

    for y in 0..height-1 do
        for x in 0..width-1 do
            if board.Tiles.[y, x] = Opened then
                match board.Minefield[y, x] with
                | Clear -> printf " "
                | Mine -> printf "X"
                | NearMine n -> printf "%d" n
            else
                printf "#"
        printfn ""

let board = GenerateBoard (10, 10) 10
PrintBoard board

printfn ""

let board2 = RevealBoard board (2, 2)
PrintBoard board2
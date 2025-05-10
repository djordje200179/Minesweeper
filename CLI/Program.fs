open Minesweeper.Board
open Minesweeper.Utils
open Minesweeper.Game

let minesCount = 10
let dimensions = { Height = 10; Width = 10 }

Board.Create dimensions minesCount
|> gameLoop (Output.printState, Input.readAction) []
|> function
    | Ok _ -> printf "You won! You opened all cells without hitting a mine."
    | Error (MineOpenedError location) -> printf "Game over! You opened the mine at %O." location
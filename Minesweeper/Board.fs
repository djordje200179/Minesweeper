module Minesweeper.Board

open Minesweeper.Minefield

type Cell =
    | Opened
    | Closed

type Board = {
    Tiles: Cell [,]
    Minefield: Field
}

let GenerateBoard (height: int, width: int) (mines: int): Board =
    let field = GenerateField (height, width) mines
    let tiles = Array2D.create height width Closed

    { Tiles = tiles; Minefield = field }

let RevealBoard (board: Board) (y: int, x: int): Board = 
    let rec OpenCell (tiles: Cell [, ]) (y: int, x: int) =
        let height, width = Array2D.length1 board.Tiles, Array2D.length2 board.Tiles

        if x >= 0 && x < width && y >= 0 && y < height && tiles.[y, x] = Closed then
            tiles[y, x] <- Opened

            if board.Minefield[y, x] = Clear then
                OpenCell tiles (y - 1, x)
                OpenCell tiles (y + 1, x)
                OpenCell tiles (y, x - 1)
                OpenCell tiles (y, x + 1)
                OpenCell tiles (y - 1, x - 1)
                OpenCell tiles (y - 1, x + 1)
                OpenCell tiles (y + 1, x - 1)
                OpenCell tiles (y + 1, x + 1)

    let newTiles = Array2D.copy board.Tiles
    OpenCell newTiles (y, x)

    { board with Tiles = newTiles }
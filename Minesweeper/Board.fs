module Minesweeper.Board

open System
open Utils

[<Struct>]
type UndergroundCell =
    | Mine
    | NearMine of int
    | Empty

[<Struct>]
type OvergroundCell =
    | Closed
    | Marked
    | Opened of int

let private putMines dimensions avoidPoint minesCount =
    let rng = Random()

    let rec putMine mines minesLeft =
        if minesLeft = 0 then mines
        else
            let point = { Y = rng.Next dimensions.Height; X = rng.Next dimensions.Width }
            if (List.contains point mines || point = avoidPoint) then 
                putMine mines minesLeft
            else 
                putMine (point :: mines) (minesLeft - 1)

    putMine [] minesCount

type Board = 
  { OvergroundCells: OvergroundCell array2d
    UndegroundCells: UndergroundCell array2d }
    member this.Dimensions =
        { Height = Array2D.length1 this.UndegroundCells
          Width = Array2D.length2 this.UndegroundCells }
          
    member this.LeftoverMines =
        this.UndegroundCells
        |> Seq.cast<UndergroundCell>
        |> Seq.sumBy ((=) Mine >> Convert.ToInt32)

let createEmptyBoard dimension =
    { OvergroundCells = Array2D.create dimension.Height dimension.Width Closed
      UndegroundCells = Array2D.create dimension.Height dimension.Width Empty  }

exception MineOpened of Location

let openCell board location =
    let newOvergroundCells = Array2D.copy board.OvergroundCells
    let rec openCellRec location =
        newOvergroundCells[location.Y, location.X] <- 
            match board.UndegroundCells[location.Y, location.X] with
            | Mine -> raise (MineOpened location)
            | NearMine count -> Opened count
            | Empty -> Opened 0

        if newOvergroundCells[location.Y, location.X] = Opened 0 then
            location
            |> getNeigbouringCells board.Dimensions
            |> List.filter (fun location -> newOvergroundCells[location.Y, location.X] = Closed)
            |> List.iter openCellRec

    openCellRec location

    { board with OvergroundCells = newOvergroundCells }

let generateValidBoard board minesCount avoidPoint =
    let newUndergroundCells = Array2D.copy board.UndegroundCells

    for mine in (putMines board.Dimensions avoidPoint minesCount) do
        newUndergroundCells[mine.Y, mine.X] <- Mine
    
        for location in (getNeigbouringCells board.Dimensions mine) do
            newUndergroundCells[location.Y, location.X] <- 
                match newUndergroundCells[location.Y, location.X] with
                | Mine -> Mine
                | NearMine count -> NearMine (count + 1)
                | Empty -> NearMine 1
    openCell { board with UndegroundCells = newUndergroundCells } avoidPoint
module Minesweeper.Board

open System
open Utils

[<Struct>]
type HiddenCell =
    | Mine
    | NearMine of int
    | Empty

[<Struct>]
type VisibleCell =
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
  { VisibleCells: VisibleCell array2d
    HiddenCells: HiddenCell array2d }
    member this.Dimensions =
        { Height = Array2D.length1 this.HiddenCells
          Width = Array2D.length2 this.HiddenCells }
          
    member this.LeftoverMines =
        this.HiddenCells
        |> Seq.cast<HiddenCell>
        |> Seq.sumBy ((=) Mine >> Convert.ToInt32)

let createEmptyBoard dimension =
    { VisibleCells = Array2D.create dimension.Height dimension.Width Closed
      HiddenCells = Array2D.create dimension.Height dimension.Width Empty  }

exception MineOpened of Location

let openCell board location =
    let visibleCells = Array2D.copy board.VisibleCells
    let rec openCellRec location =
        visibleCells[location.Y, location.X] <- 
            match board.HiddenCells[location.Y, location.X] with
            | Mine -> raise (MineOpened location)
            | NearMine count -> Opened count
            | Empty -> Opened 0

        if visibleCells[location.Y, location.X] = Opened 0 then
            location
            |> getNeigbouringCells board.Dimensions
            |> List.filter (fun location -> visibleCells[location.Y, location.X] = Closed)
            |> List.iter openCellRec
    openCellRec location

    { board with VisibleCells = visibleCells }

let generateValidBoard board minesCount avoidPoint =
    let hiddenCells = Array2D.copy board.HiddenCells

    for mine in (putMines board.Dimensions avoidPoint minesCount) do
        hiddenCells[mine.Y, mine.X] <- Mine
    
        for location in (getNeigbouringCells board.Dimensions mine) do
            hiddenCells[location.Y, location.X] <- 
                match hiddenCells[location.Y, location.X] with
                | Mine -> Mine
                | NearMine count -> NearMine (count + 1)
                | Empty -> NearMine 1

    openCell { board with HiddenCells = hiddenCells } avoidPoint
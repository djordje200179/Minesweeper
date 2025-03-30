module Minesweeper.Board

open System
open Utils
open System.Diagnostics

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
            let point = getRandomLocation rng dimensions
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
      HiddenCells = Array2D.create dimension.Height dimension.Width Empty }

exception MineOpened of Location

let openCell board location =
    let visibleCells = Array2D.copy board.VisibleCells
    let rec openCellRec location =
        match board.HiddenCells.GetAt location with
        | Mine -> raise (MineOpened location)
        | NearMine count -> Opened count
        | Empty -> Opened 0
        |> visibleCells.SetAt location

        if visibleCells.GetAt location = Opened 0 then
            location
            |> getNeigbouringCells board.Dimensions
            |> List.filter (visibleCells.GetAt >> (=) Closed)
            |> List.iter openCellRec
    openCellRec location

    { board with VisibleCells = visibleCells }

let generateValidBoard board minesCount avoidPoint =
    let hiddenCells = Array2D.copy board.HiddenCells

    for mineLocation in (putMines board.Dimensions avoidPoint minesCount) do
        hiddenCells.SetAt mineLocation Mine
    
        mineLocation
        |> getNeigbouringCells board.Dimensions
        |> Seq.iter (hiddenCells.Update (
            function
            | Mine -> Mine
            | NearMine count -> NearMine count
            | Empty -> NearMine 1
        ))

    openCell { board with HiddenCells = hiddenCells } avoidPoint
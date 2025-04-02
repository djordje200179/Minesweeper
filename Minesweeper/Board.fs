﻿module Minesweeper.Board

open System
open Utils
open Mines

[<Struct>]
type VisibleCell =
    | Closed
    | Marked
    | Opened of int

type Board = 
    private
        { visibleCells: VisibleCell array2d
          minefield: Minefield }
      
    static member Create dimensions minesCount =
        { visibleCells = Array2D.create dimensions.Height dimensions.Width Closed
          minefield = Uninitialized minesCount }

    member this.Dimensions =
        { Height = Array2D.length1 this.visibleCells
          Width = Array2D.length2 this.visibleCells }

    member this.UnmarkedMinesCount =
        let markedCellsCount =
            this.visibleCells
                |> Seq.cast<VisibleCell>
                |> Seq.sumBy ((=) Marked >> Convert.ToInt32)
            
        getTotalMinesCount this.minefield - markedCellsCount
          
    member this.Item with get location = this.visibleCells.GetAt location
    member this.GetRow y = this.visibleCells[y, *]

exception MineOpened of Location

let rec openCell board location =
    match board.minefield with
    | Uninitialized minesCount -> 
        let minefield = initMinefield board.Dimensions location minesCount

        openCell { board with minefield = minefield } location
    | Initialized minefield ->
        let visibleCells = Array2D.copy board.visibleCells

        let rec openCellRec location =
            match minefield.GetAt location with
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

        { board with visibleCells = visibleCells }

let markCell board location =
    let visibleCells = Array2D.copy board.visibleCells
    visibleCells.UpdateAt (
        function
        | Closed -> Marked
        | Marked -> Closed
        | Opened n -> Opened n
    ) location

    { board with visibleCells = visibleCells }

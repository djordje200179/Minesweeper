module Minesweeper.Board

open System
open Utils
open Mines

[<Struct>]
type VisibleCell =
    | Closed
    | Marked
    | Opened of int

[<Struct>]
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
            
        this.minefield.TotalMinesCount - markedCellsCount
          
    member this.Item with get location = this.visibleCells.GetFromLocation location
    member this.GetRow y = this.visibleCells[y, *]

[<Struct>]
type MineOpenedError = MineOpenedError of Location

[<TailCall>]
let rec openCell board location =
    match board.minefield with
    | Uninitialized minesCount -> 
        let minefield = initMinefield board.Dimensions location minesCount

        openCell { board with minefield = minefield } location
    | Initialized minefield ->
        let visibleCells = Array2D.copy board.visibleCells
        
        let rec openCellRec location =
            match minefield.GetFromLocation location with
            | Mine -> Error (MineOpenedError location)
            | NearMine count -> 
                visibleCells.SetAtLocation location (Opened count)
                Ok ()
            | Empty -> 
                visibleCells.SetAtLocation location (Opened 0)

                location
                |> getNeigbouringLocations board.Dimensions
                |> Seq.filter (visibleCells.GetFromLocation >> (=) Closed)
                |> Seq.map openCellRec
                |> Seq.tryFind Result.isError
                |> Option.defaultValue (Ok ())              

        openCellRec location
        |> Result.map (fun () -> { board with visibleCells = visibleCells })

let markCell board location =
    let newVisibleCells = Array2D.copy board.visibleCells

    board.visibleCells.GetFromLocation location
    |> function
        | Closed -> Marked
        | Marked -> Closed
        | other -> other
    |> newVisibleCells.SetAtLocation location

    { board with visibleCells = newVisibleCells }
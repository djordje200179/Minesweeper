module Minesweeper.Board

open System
open Utils

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

[<Struct>]
type VisibleCell =
    | Closed
    | Marked
    | Opened of int

[<Struct>]
type private HiddenCell =
    | Mine
    | NearMine of int
    | Empty

[<Struct>]
type private Minefield =
    | Initialized of minefield: HiddenCell array2d
    | Uninitialized of minesCount: int

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

    member this.MarkedCellsCount =
        this.visibleCells
        |> Seq.cast<VisibleCell>
        |> Seq.sumBy ((=) Marked >> Convert.ToInt32)

    member this.TotalMinesCount =
        match this.minefield with
        | Uninitialized minesCount -> minesCount
        | Initialized minefield -> 
            minefield
            |> Seq.cast<HiddenCell>
            |> Seq.sumBy ((=) Mine >> Convert.ToInt32)

    member this.UnmarkedMinesCount = this.TotalMinesCount - this.MarkedCellsCount
          
    member this.Item with get location = this.visibleCells.GetAt location
    member this.GetRow y = this.visibleCells[y, *]

exception MineOpened of Location

let rec openCell board location =
    match board.minefield with
    | Uninitialized minesCount -> 
        let minefield = Array2D.create board.Dimensions.Height board.Dimensions.Width Empty

        for mineLocation in (putMines board.Dimensions location minesCount) do
            minefield.SetAt mineLocation Mine
    
            mineLocation
            |> getNeigbouringCells board.Dimensions
            |> Seq.iter (minefield.UpdateAt (
                function
                | Mine -> Mine
                | NearMine count -> NearMine (count + 1)
                | Empty -> NearMine 1
            ))

        openCell { board with minefield = Initialized minefield } location
    | Initialized minefield ->
        let visibleCells = Array2D.copy board.visibleCells

        if minefield.GetAt location = Mine then
            raise (MineOpened location)

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

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
    | Initialized of field: HiddenCell array2d
    | Uninitialized of minesCount: int

type Board = 
  private // TODO: Fix ident
    { visibleCells: VisibleCell array2d
      hiddenCells: Minefield }
      
    static member Create dimensions minesCount =
        { visibleCells = Array2D.create dimensions.Height dimensions.Width Closed
          hiddenCells = Uninitialized minesCount }

    member this.Dimensions =
        { Height = Array2D.length1 this.visibleCells
          Width = Array2D.length2 this.visibleCells }

    member this.LeftoverMines =
        let minesCount =
            match this.hiddenCells with
            | Uninitialized minesCount -> minesCount
            | Initialized field -> 
                field
                |> Seq.cast<HiddenCell>
                |> Seq.sumBy ((=) Mine >> Convert.ToInt32)

        let markedMinesCount =
            this.visibleCells
            |> Seq.cast<VisibleCell>
            |> Seq.sumBy ((=) Marked >> Convert.ToInt32)

        minesCount - markedMinesCount
          
    member this.Item with get location = this.visibleCells.GetAt location
    member this.GetRow y = this.visibleCells[y, *]

exception MineOpened of Location

let rec openCell board location =
    match board.hiddenCells with
    | Uninitialized minesCount -> 
        let hiddenCells = Array2D.create board.Dimensions.Height board.Dimensions.Width Empty

        for mineLocation in (putMines board.Dimensions location minesCount) do
            hiddenCells.SetAt mineLocation Mine
    
            mineLocation
            |> getNeigbouringCells board.Dimensions
            |> Seq.iter (hiddenCells.UpdateAt (
                function
                | Mine -> Mine
                | NearMine count -> NearMine count
                | Empty -> NearMine 1
            ))

        openCell { board with hiddenCells = Initialized hiddenCells } location
    | Initialized hiddenCells ->
        let visibleCells = Array2D.copy board.visibleCells

        if hiddenCells.GetAt location = Mine then
            raise (MineOpened location)

        let rec openCellRec location =
            match hiddenCells.GetAt location with
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

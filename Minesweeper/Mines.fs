module private Minesweeper.Mines

open System
open Utils

[<Struct>]
type internal MinefieldCell =
    | Mine
    | NearMine of int
    | Empty

[<Struct>]
type internal Minefield =
    | Initialized of minefield: MinefieldCell array2d
    | Uninitialized of minesCount: int

module Minefield =
    let internal init dimensions avoidPoint minesCount =
        let rng = Random()

        let rec putMineRec mines minesLeft =
            if minesLeft = 0 then mines
            else
                let point = Location.createRandom rng dimensions
                if (List.contains point mines || point = avoidPoint) then 
                    putMineRec mines minesLeft
                else 
                    putMineRec (point :: mines) (minesLeft - 1)

        let mines = putMineRec [] minesCount
    
        let minefield = Array2D.create dimensions.Height dimensions.Width Empty
        for mineLocation in mines do
            minefield[mineLocation.Y, mineLocation.X] <- Mine
        
            for neighbouringLocation in Location.getNeighbouring dimensions mineLocation do
                minefield.GetFromLocation neighbouringLocation
                |> function
                    | Mine -> Mine
                    | NearMine count -> NearMine (count + 1)
                    | Empty -> NearMine 1
                |> minefield.SetAtLocation neighbouringLocation

        Initialized minefield

    let getTotalMinesCount = function
        | Uninitialized minesCount -> minesCount
        | Initialized minefield ->
            minefield
            |> Seq.cast<MinefieldCell>
            |> Seq.sumBy ((=) Mine >> Convert.ToInt32)
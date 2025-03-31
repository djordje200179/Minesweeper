module private Minesweeper.Mines

open System
open Utils

let internal putMines dimensions avoidPoint minesCount =
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
type internal MinefieldCell =
    | Mine
    | NearMine of int
    | Empty

[<Struct>]
type internal Minefield =
    | Initialized of minefield: MinefieldCell array2d
    | Uninitialized of minesCount: int

let internal getTotalMinesCount = function
    | Uninitialized minesCount -> minesCount
    | Initialized minefield ->
        minefield
        |> Seq.cast<MinefieldCell>
        |> Seq.sumBy ((=) Mine >> Convert.ToInt32)
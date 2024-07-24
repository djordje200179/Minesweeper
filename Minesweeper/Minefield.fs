module Minesweeper.Minefield

open System

type Cell = 
    | Mine
    | NearMine of int
    | Clear

type Field = Cell [,]

let rec PutBomb (rng: Random) (field: Field): Field =
    let height, width = field.GetLength(0), field.GetLength(1)
    let y, x = rng.Next(height), rng.Next(width)

    if field[y, x] = Mine then
        PutBomb rng field
    else
        field
        |> Array2D.mapi (fun i j cell ->
            match i, j with
            | i, j when i = y && j = x -> Mine
            | i, j when Math.Abs(i - y) <= 1 && Math.Abs(j - x) <= 1 ->
                match cell with
                | Clear -> NearMine 1
                | NearMine n -> NearMine (n + 1)
                | _ -> cell
            | _ -> cell
        )

let GenerateField (height: int, width: int) (mines: int): Field =
    let rng = Random()

    Seq.fold 
        (fun field i -> PutBomb rng field) 
        (Array2D.create height width Clear)
        (seq { 1 .. mines })

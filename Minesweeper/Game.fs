module Minesweeper.Game

open Board
open Utils

type UserAction =
    | Open of Location
    | Mark of Location
    | Undo

let rec gameLoop (stateOutput, actionInput) previousBoards board =
    let leftMinesCount = getUnmarkedMinesCount board

    stateOutput board leftMinesCount

    if leftMinesCount = 0 then Ok ()  
    else
        (Location.isValid board.Dimensions, List.isEmpty previousBoards)
        ||> actionInput
        |> function
            | Open location -> openCell board location
            | Mark location -> Ok (markCell board location)
            | Undo -> failwith "Undo not implemented yet"
        |> Result.bind (gameLoop (stateOutput, actionInput) (board::previousBoards))
module Minesweeper.Utils

[<Struct>]
type Location = 
    { Y: int; X: int }
    override this.ToString() = $"({this.Y}, {this.X})"

[<Struct>]
type Dimensions = 
    { Height: int; Width: int }
    override this.ToString() = $"({this.Height} X {this.Width})"

let isLocationValid dimensions location =
    location.Y >= 0 && location.Y < dimensions.Height && location.X >= 0 && location.X < dimensions.Width

let getNeigbouringCells dimensions location =
    ([-1; 0; 1], [-1; 0; 1])
    ||> List.allPairs
    |> List.filter (fun (dy, dx) -> dy <> 0 || dx <> 0)
    |> List.map (fun (dy, dx) -> { Y = location.Y + dy; X = location.X + dx })
    |> List.filter (isLocationValid dimensions)
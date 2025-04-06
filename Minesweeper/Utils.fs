module Minesweeper.Utils

open System

[<Struct>]
type Location = 
    { Y: int; X: int }
    override this.ToString() = $"({this.Y}, {this.X})"

[<Struct>]
type Dimensions = 
    { Height: int; Width: int }
    override this.ToString() = $"({this.Height} X {this.Width})"

let inline internal isLocationValid dimensions location =
    location.Y >= 0 && location.Y < dimensions.Height && location.X >= 0 && location.X < dimensions.Width

let inline internal getRandomLocation (rng: Random) dimensions =
    { Y = rng.Next dimensions.Height; X = rng.Next dimensions.Width }

let internal getNeigbouringLocations dimensions location =
    ([-1; 0; 1], [-1; 0; 1])
    ||> Seq.allPairs
    |> Seq.filter (fun (dy, dx) -> dy <> 0 || dx <> 0)
    |> Seq.map (fun (dy, dx) -> { Y = location.Y + dy; X = location.X + dx })
    |> Seq.filter (isLocationValid dimensions)

type internal 'T ``[,]`` with
    member this.GetFromLocation location = this[location.Y, location.X]
    member this.SetAtLocation location value = this[location.Y, location.X] <- value
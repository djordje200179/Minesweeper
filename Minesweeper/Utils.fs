module Minesweeper.Utils

open System

[<Struct>]
type Dimensions = 
    { Height: int; Width: int }
    override this.ToString() = $"({this.Height} X {this.Width})"

[<Struct>]
type Location = 
    { Y: int; X: int }
    override this.ToString() = $"({this.Y}, {this.X})"

    static member inline isValid dimensions location =
        location.Y >= 0 && location.Y < dimensions.Height && location.X >= 0 && location.X < dimensions.Width

    static member inline createRandom (rng: Random) dimensions =
        { Y = rng.Next dimensions.Height; X = rng.Next dimensions.Width }

module Location =
    let getNeighbouring dimensions location =
       ([-1; 0; 1], [-1; 0; 1])
       ||> Seq.allPairs
       |> Seq.filter (fun (dy, dx) -> dy <> 0 || dx <> 0)
       |> Seq.map (fun (dy, dx) -> { Y = location.Y + dy; X = location.X + dx })
       |> Seq.filter (Location.isValid dimensions)

type internal 'T ``[,]`` with
    member inline this.GetFromLocation location = this[location.Y, location.X]
    member inline this.SetAtLocation location value = this[location.Y, location.X] <- value
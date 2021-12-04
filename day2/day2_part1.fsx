open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines (__SOURCE_DIRECTORY__ + "/day2_input.txt")

type Position = { Position: int; Depth: int }

let commands = 
    lines 
    |> Seq.map (fun line -> 
                        let line = line.Split(" ")
                        line.[0], Int32.Parse(line.[1]))

let position =
    let move (position: Position) (command, distance) =
        match command with
        | "forward" -> { position with Position = position.Position + distance }
        | "down" -> { position with Depth = position.Depth + distance }
        | "up" -> { position with Depth = position.Depth - distance }
        | _ -> failwith "Wrong input"
    commands
    |> Seq.fold move { Position = 0; Depth = 0 }

Console.WriteLine(position)
Console.WriteLine(position.Position * position.Depth)
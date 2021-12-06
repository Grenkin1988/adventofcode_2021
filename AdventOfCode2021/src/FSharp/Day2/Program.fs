open System
let fileName = sprintf "%s/%s" __SOURCE_DIRECTORY__ "/day2_input.txt"
let lines = Helpers.readLines fileName

type Position = { Position: int; Depth: int; Aim: int }

let commands = 
    lines 
    |> Seq.map (fun line -> 
        let line = line.Split(" ")
        line.[0], line.[1] |> int)

let part1 =
    let move (position: Position) (command, distance) =
        match command with
        | "forward" -> { position with Position = position.Position + distance }
        | "down" -> { position with Depth = position.Depth + distance }
        | "up" -> { position with Depth = position.Depth - distance }
        | _ -> failwith "Wrong input"
    commands
    |> Seq.fold move { Position = 0; Depth = 0; Aim = 0 }

printfn "%A" part1
printfn "%A" (part1.Position * part1.Depth)

let part2 =
    let move (position: Position) (command, distance) =
        match command with
        | "forward" -> 
            { position with 
                Position = position.Position + distance
                Depth = position.Depth + (position.Aim * distance)
            }
        | "down" -> { position with Aim = position.Aim + distance }
        | "up" -> { position with Aim = position.Aim - distance }
        | _ -> failwith "Wrong input"
    commands
    |> Seq.fold move { Position = 0; Depth = 0; Aim = 0 }

printfn "%A" part2
printfn "%A" (part2.Position * part2.Depth)

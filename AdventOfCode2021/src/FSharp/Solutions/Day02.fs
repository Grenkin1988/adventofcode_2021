[<NUnit.Framework.TestFixture>]
module Solutions.Day02

open Solutions.Helpers
open NUnit.Framework
open System

type Position = { Position: int; Depth: int; Aim: int }

[<TestCase("day02_testinput.txt", ExpectedResult = 150)>]
[<TestCase("day02_input.txt", ExpectedResult = 1451208)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
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
    (part1.Position * part1.Depth)
    
    

[<TestCase("day02_testinput.txt", ExpectedResult = 900)>]
[<TestCase("day02_input.txt", ExpectedResult = 1620141160)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    let commands = 
        lines 
        |> Seq.map (fun line -> 
            let line = line.Split(" ")
            line.[0], line.[1] |> int)

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
    (part2.Position * part2.Depth)

[<NUnit.Framework.TestFixture>]
module Solutions.Day07

open Solutions.Helpers
open NUnit.Framework
open System

[<TestCase("day07_testinput.txt", ExpectedResult = 37)>]
[<TestCase("day07_input.txt", ExpectedResult = 333755)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    let numbers = splitToInt "," [lines.Head] |> List.head
    
    let part1 =
        numbers
        |> List.map (fun n -> n, numbers |> List.map (fun m -> abs (m - n)) |> List.sum)
        |> List.minBy snd
    snd part1

[<TestCase("day07_testinput.txt", ExpectedResult = 168)>]
[<TestCase("day07_input.txt", ExpectedResult = 94017638)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    let numbers = splitToInt "," [lines.Head] |> List.head
    let fuelTo from destination =
        let dest =  abs(from - destination)
        dest * (dest + 1) / 2
    
    let part2 =
        [numbers |> List.min .. numbers |> List.max]
        |> List.map (fun n -> n, numbers |> List.map (fun m -> fuelTo m  n) |> List.sum)
        |> List.minBy snd
    snd part2

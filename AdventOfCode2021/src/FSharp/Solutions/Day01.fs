[<NUnit.Framework.TestFixture>]
module Solutions.Day01

open Solutions.Helpers
open NUnit.Framework
open System

[<TestCase("day01_testinput.txt", ExpectedResult = 7)>]
[<TestCase("day01_input.txt", ExpectedResult = 1521)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    let numbers = linesToNymbers lines
    
    let part1 =
        numbers
        |> List.pairwise
        |> List.map (fun (i,j) -> j > i)
        |> List.sumBy System.Convert.ToInt32
    
    part1

[<TestCase("day01_testinput.txt", ExpectedResult = 5)>]
[<TestCase("day01_input.txt", ExpectedResult = 1543)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    let numbers = linesToNymbers lines

    let part2 =
        numbers
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.map (fun (i,j) -> j > i)
        |> Seq.sumBy System.Convert.ToInt32
    
    part2

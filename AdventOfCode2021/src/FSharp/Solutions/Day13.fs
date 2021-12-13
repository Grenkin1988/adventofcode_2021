[<NUnit.Framework.TestFixture>]
module Solutions.Day13

open Solutions.Helpers
open NUnit.Framework

[<RequireQualifiedAccess>]
type Fold = X of int | Y of int

module Fold =
    let getInstructions (lines: string list) =
        lines
        |> List.skipWhile (fun line -> line.StartsWith("fold") |> not)
        |> List.map (fun line -> line.Split("="))
        |> List.map (fun line -> 
            if line.[0].EndsWith("x") then
                line.[1] |> int |> Fold.X
            else
                line.[1] |> int |> Fold.Y)

[<TestCase("day13_testinput.txt", ExpectedResult = 17)>]
[<TestCase("day13_input.txt", ExpectedResult = 745)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName

    let points = 
        lines 
        |> List.takeWhile (fun l -> System.String.IsNullOrWhiteSpace(l) |> not)
        |> splitToInt ","
        |> List.map (fun (x :: y :: _) -> x,y)

    let instructions = Fold.getInstructions lines

    let maxX, maxY = points |> List.maxBy (fun (x,_) -> x) |> fst, points |> List.maxBy (fun (_,y) -> y) |> snd

    let matrix = 
        Array2D.create (maxY + 1) (maxX + 1) 0
        |> Matrix.fill points 1

    let folded = 
        instructions
        |> List.take 1
        |> List.fold (fun mat i -> 
            match i with
            | Fold.X x -> Matrix.foldX x mat
            | Fold.Y y -> Matrix.foldY y mat) matrix

    Matrix.sum folded

[<TestCase("day13_testinput.txt", "day13_test.txt")>]
[<TestCase("day13_input.txt", "day13_main.txt")>]
let ``Part 2`` fileName expected =
    let lines = readInput fileName
    let expected = readExpected expected |> String.concat "\r\n"
    
    let points = 
        lines 
        |> List.takeWhile (fun l -> System.String.IsNullOrWhiteSpace(l) |> not)
        |> splitToInt ","
        |> List.map (fun (x :: y :: _) -> x,y)

    let instructions = Fold.getInstructions lines

    let maxX, maxY = points |> List.maxBy (fun (x,_) -> x) |> fst, points |> List.maxBy (fun (_,y) -> y) |> snd

    let matrix = 
        Array2D.create (maxY + 1) (maxX + 1) 0
        |> Matrix.fill points 1

    let folded = 
        instructions
        |> List.fold (fun mat i -> 
            match i with
            | Fold.X x -> Matrix.foldX x mat
            | Fold.Y y -> Matrix.foldY y mat) matrix

    let print = Matrix.print folded

    Assert.AreEqual(expected, print)

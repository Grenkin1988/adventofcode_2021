[<NUnit.Framework.TestFixture>]
module Solutions.Day05

open Solutions.Helpers
open NUnit.Framework
open System

type Matrix = int[,]

module Matrix =
    let fill x y (matrix: Matrix) =
        matrix.[x,y] <- matrix.[x,y] + 1
        matrix

    let find (pred: (_ -> bool)) (matrix: Matrix) =
        let mutable found = 0
        Array2D.iter (fun i -> if pred(i) then found <- found + 1) matrix
        found

type Pipe = { X1: int; Y1: int; X2: int; Y2: int }

module Pipe =
    let isHorizontal { X1 = x1; X2 = x2 } = x1 = x2
    let isVertical { Y1 = y1; Y2 = y2 } = y1 = y2
    let isHorizontalOrVertical pipe = isHorizontal pipe || isVertical pipe
    let isDiagonal { X1 = x1; X2 = x2; Y1 = y1; Y2 = y2 } = abs(x1 - x2) = abs(y1 - y2)
    let isValid pipe = isHorizontal pipe || isVertical pipe || isDiagonal pipe
    let minX { X1 = x1; X2 = x2 } = min x1 x2
    let maxX { X1 = x1; X2 = x2 } = max x1 x2
    let minY { Y1 = y1; Y2 = y2 } = min y1 y2
    let maxY { Y1 = y1; Y2 = y2 } = max y1 y2
    let range x1 x2 = 
        if x1 > x2
        then [x1 .. -1 .. x2]
        else [x1 .. x2]
    let xRange { X1 = x1; X2 = x2 } = range x1 x2
    let yRange { Y1 = y1; Y2 = y2 } = range y1 y2

    let draw matrix pipe =
        let range =
            match pipe with
            | pipe when isHorizontal pipe ->
                [ minY pipe..maxY pipe ] |> List.map(fun y -> pipe.X1,y)
            | pipe when isVertical pipe ->
                [ minX pipe..maxX pipe ] |> List.map(fun x -> x,pipe.Y1)
            | _ -> failwith "NO"
        range
        |> List.fold (fun m (x,y) -> Matrix.fill x y m) matrix

    let draw' matrix pipe =
        let range =
            match pipe with
            | pipe when isHorizontal pipe ->
                pipe |> yRange |> List.map(fun y -> pipe.X1,y)
            | pipe when isVertical pipe ->
                pipe |> xRange |> List.map(fun x -> x,pipe.Y1)
            | pipe when isDiagonal pipe ->
                (xRange pipe, yRange pipe)
                ||> List.zip
            | _ -> failwith "NO"
        range
        |> List.fold (fun m (x,y) -> Matrix.fill x y m) matrix

[<TestCase("day05_testinput.txt", ExpectedResult = 5)>]
[<TestCase("day05_input.txt", ExpectedResult = 8111)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    
    let pipes =
        lines
        |> List.map (fun line -> 
            line.Split([|"->"; ","|], StringSplitOptions.TrimEntries)
            |> Array.map int
            |> (fun ar -> { X1 = ar.[0]; Y1 = ar.[1]; X2 = ar.[2]; Y2 = ar.[3] }))
    
    let pipesPart1 =
        pipes
        |> List.filter Pipe.isHorizontalOrVertical
    
    let maxX = pipesPart1 |> List.maxBy Pipe.maxX |> Pipe.maxX
    let maxY = pipesPart1 |> List.maxBy Pipe.maxY |> Pipe.maxY
    
    let matrix =
        pipesPart1 |> List.fold Pipe.draw (Array2D.zeroCreate (maxX + 1) (maxY + 1))
    
    let found = Matrix.find (fun i -> i > 1) matrix
    
    found

[<TestCase("day05_testinput.txt", ExpectedResult = 12)>]
[<TestCase("day05_input.txt", ExpectedResult = 22088)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    
    let pipes =
        lines
        |> List.map (fun line -> 
            line.Split([|"->"; ","|], StringSplitOptions.TrimEntries)
            |> Array.map int
            |> (fun ar -> { X1 = ar.[0]; Y1 = ar.[1]; X2 = ar.[2]; Y2 = ar.[3] }))

    let pipesPart2 =
        pipes
        |> List.filter Pipe.isValid
    
    let maxX = pipesPart2 |> List.maxBy Pipe.maxX |> Pipe.maxX
    let maxY = pipesPart2 |> List.maxBy Pipe.maxY |> Pipe.maxY
    
    let matrix =
        pipesPart2 |> List.fold Pipe.draw' (Array2D.zeroCreate (maxX + 1) (maxY + 1))
    
    let found = Matrix.find (fun i -> i > 1) matrix
    
    found

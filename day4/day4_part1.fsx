open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines (__SOURCE_DIRECTORY__ + "/day4_testinput.txt") |> List.ofSeq

let numbers = lines.Head.Split(",") |> Array.map int |> List.ofArray

let boards = lines.Tail |> List.filter (String.IsNullOrEmpty >> not) |> List.windowed 5

type Card = (int * bool) array





Console.WriteLine(numbers)

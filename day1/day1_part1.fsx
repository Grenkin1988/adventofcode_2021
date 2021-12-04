open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines (__SOURCE_DIRECTORY__ + "/day1_input.txt")

let numbers = lines |> Seq.map Int32.Parse

let result =
    numbers
    |> Seq.pairwise
    |> Seq.map (fun (i,j) -> j > i)
    |> Seq.sumBy System.Convert.ToInt32

Console.WriteLine(result)
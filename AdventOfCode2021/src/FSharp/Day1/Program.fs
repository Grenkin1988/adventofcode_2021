open System
let fileName = sprintf "%s/%s" __SOURCE_DIRECTORY__ "/day1_input.txt"
let lines = Helpers.readLines fileName

let numbers = Helpers.lineToNymbers lines

let part1 =
    numbers
    |> List.pairwise
    |> List.map (fun (i,j) -> j > i)
    |> List.sumBy System.Convert.ToInt32

printfn "%A" part1

let part2 =
    numbers
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.pairwise
    |> Seq.map (fun (i,j) -> j > i)
    |> Seq.sumBy System.Convert.ToInt32

printfn "%A" part2

open System
let fileName = sprintf "%s/%s" __SOURCE_DIRECTORY__ "/day7_input.txt"
let lines = Helpers.readLines fileName

let numbers = lines.Head.Split(",", StringSplitOptions.TrimEntries) |> Array.map int |> List.ofArray

let part1 =
    numbers
    |> List.map (fun n -> n, numbers |> List.map (fun m -> abs (m - n)) |> List.sum)
    |> List.minBy snd

printfn "%A" part1

let fuelTo from destination =
    let dest =  abs(from - destination)
    dest * (dest + 1) / 2

let part2 =
    [numbers |> List.min .. numbers |> List.max]
    |> List.map (fun n -> n, numbers |> List.map (fun m -> fuelTo m  n) |> List.sum)
    |> List.minBy snd

printfn "%A" part2
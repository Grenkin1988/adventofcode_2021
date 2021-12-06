open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines (__SOURCE_DIRECTORY__ + "/day6_testinput.txt") |> List.ofSeq

let initial =
    lines
    |> List.map (fun line -> 
        line.Split([| ","|], StringSplitOptions.TrimEntries)
        |> Array.map int |> List.ofArray)
    |> List.head

let newFish list = list |> List.fold (fun n c -> if c = 0 then 8 :: n else n) []

let addDay list = list |> List.map (fun n -> if n = 0 then 6 else n - 1)

let processDay list day =
    let result =
        let newFish = newFish list
        let list = addDay list
        List.append list newFish
    printfn "Day %i: %A" day result
    result

let result = 
    [1..80]
    |> List.fold processDay initial

result

printfn "%A" result
printfn "%A" result.Length

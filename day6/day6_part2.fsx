open System

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines (__SOURCE_DIRECTORY__ + "/day6_input.txt") |> List.ofSeq

let initial =
    lines
    |> List.map (fun line -> 
        line.Split([| ","|], StringSplitOptions.TrimEntries)
        |> Array.map int |> List.ofArray)
    |> List.head
    |> List.groupBy id
    |> List.map (fun (i,l) -> i, List.length l |> uint64)
    |> Map.ofList

let newFish map = 
    match map |> Map.tryFind 0 with
    | None -> None
    | Some v -> ( 8, v ) |> Some

let addDay (map: Map<int,uint64>) = 
    map 
    |> Seq.map (fun kv -> if kv.Key = 0 then 6,kv.Value else kv.Key - 1,kv.Value)
    |> Seq.groupBy fst
    |> Seq.map (fun (i,l) -> i, Seq.sumBy snd l)
    |> Map.ofSeq

let processDay map day =
    let result =
        let newFish = newFish map
        let map = addDay map
        match newFish with
        | None -> map
        | Some (k,v) -> map |> Map.add k v
    printfn "Day %i: %A" day result
    result

let result = 
    [1..256]
    |> List.fold processDay initial

result

printfn "%A" result
result |> Seq.sumBy (fun kv -> kv.Value) |> printfn "%A"

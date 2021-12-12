[<NUnit.Framework.TestFixture>]
module Solutions.Day06

open Solutions.Helpers
open NUnit.Framework
open System

let initial (lines: string list) =
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

[<TestCase("day06_testinput.txt", ExpectedResult = 5934)>]
[<TestCase("day06_input.txt", ExpectedResult = 363101)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    let initial = initial lines
    
    let result = 
        [1..80]
        |> List.fold processDay initial
    
    printfn "%A" result
    result |> Seq.sumBy (fun kv -> kv.Value)

[<TestCase("day06_testinput.txt", ExpectedResult = 26984457539L)>]
[<TestCase("day06_input.txt", ExpectedResult = 1644286074024L)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    let initial = initial lines

    let result = 
        [1..256]
        |> List.fold processDay initial
    
    printfn "%A" result
    result |> Seq.sumBy (fun kv -> kv.Value)

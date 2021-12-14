[<NUnit.Framework.TestFixture>]
module Solutions.Day14

open System
open Solutions.Helpers
open NUnit.Framework

type Template = Map<(char*char), Int64>
type Rules = Map<char*char, char>

module Polymerization =
    let cycle (rules: Rules) (template: Template) : Template = 
        template
        |> Seq.collect (fun kv ->
            let (a,b) = kv.Key
            let i = Map.find (a,b) rules
            seq { ((a,i), kv.Value); ((i,b), kv.Value) })
        |> Seq.groupBy fst
        |> Seq.map (fun (i,v) -> i, v |> Seq.map snd |> Seq.sum)
        |> Map.ofSeq

    let countItems lastLetter (template: Template) =
        template
        |> Seq.map (fun kv -> fst kv.Key, kv.Value)
        |> Seq.groupBy fst
        |> Seq.map (fun (i,v) -> i, v |> Seq.map snd |> Seq.sum)
        |> Map.ofSeq
        |> Map.change lastLetter (function Some v -> Some (v + 1L) | None -> None)


[<TestCase("day14_testinput.txt", ExpectedResult = 1588)>]
[<TestCase("day14_input.txt", ExpectedResult = 2740)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    
    let initialLine = lines.Head.ToCharArray()

    let initial = 
        initialLine
        |> Array.pairwise
        |> Array.groupBy id
        |> Array.map (fun (i,v) -> i, v |> Seq.length |> int64)
        |> Map.ofSeq

    let rules = 
        lines[2..]
        |> List.map (fun line -> 
            line.Split("->", StringSplitOptions.TrimEntries))
        |> List.map (fun line -> line.[0].ToCharArray(), line.[1].ToCharArray().[0])
        |> List.map (fun (c, i) -> (c.[0], c.[1]), i)
        |> Map.ofList

    let result =
        [1..10]
        |> List.fold (fun tem _ -> Polymerization.cycle rules tem) initial

    let lastLetter = initialLine |> Array.last
    let counted = Polymerization.countItems lastLetter result
    let count = counted |> Seq.map (fun kv -> kv.Value) |> Seq.sort
    let small = Seq.head count
    let big = Seq.last count
    
    big - small

[<TestCase("day14_testinput.txt", ExpectedResult = 2188189693529L)>]
[<TestCase("day14_input.txt", ExpectedResult = 2959788056211L)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    
    let initialLine = lines.Head.ToCharArray()

    let initial = 
        initialLine
        |> Array.pairwise
        |> Array.groupBy id
        |> Array.map (fun (i,v) -> i, v |> Seq.length |> int64)
        |> Map.ofSeq

    let rules = 
        lines[2..]
        |> List.map (fun line -> 
            line.Split("->", StringSplitOptions.TrimEntries))
        |> List.map (fun line -> line.[0].ToCharArray(), line.[1].ToCharArray().[0])
        |> List.map (fun (c, i) -> (c.[0], c.[1]), i)
        |> Map.ofList

    let result =
        [1..40]
        |> List.fold (fun tem _ -> Polymerization.cycle rules tem) initial

    let lastLetter = initialLine |> Array.last
    let counted = Polymerization.countItems lastLetter result
    let count = counted |> Seq.map (fun kv -> kv.Value) |> Seq.sort
    let small = Seq.head count
    let big = Seq.last count
    
    big - small

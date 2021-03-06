[<NUnit.Framework.TestFixture>]
module Solutions.Day10

open Solutions.Helpers
open NUnit.Framework

type SyntaxAnalisys =
    | NoErrors
    | Error
    | Incomplete of unmatched: char list
    | Corrupted of expected: char * found: char

module Analisys =
    let isOpen = function '(' | '[' | '{' | '<' -> true | _ -> false
    let isClose = function ')' | ']' | '}' | '>' -> true | _ -> false

    let matching = function '(' -> ')' | '[' -> ']' | '{' -> '}' | '<' -> '>' | c -> failwithf "UNEXPECTED %A" c

    let analyse (line: string) =
        let brackets = line.ToCharArray() |> List.ofArray

        let rec analyseNext (brackets: char list) (unmatched: char list) (corrupted: (char*char) list) =
            match brackets with
            | c :: next when isOpen c -> analyseNext next (c :: unmatched) corrupted
            | c :: next when isClose c ->
                match unmatched with
                | [] -> Error
                | u :: unext -> 
                    let m = matching u
                    let corrupted = if c = m then corrupted else (m,c) :: corrupted
                    analyseNext next unext corrupted
            | c :: _ -> failwithf "UNEXPECTED %A" c
            | [] ->
                match unmatched, corrupted with
                | [], [] -> NoErrors
                | un, [] -> Incomplete un
                | _, corr -> List.last corr |> Corrupted
        analyseNext brackets [] []

    let fillUnmatched (unmatched: char list) =
        unmatched |> List.map matching

[<TestCase("day10_testinput.txt", ExpectedResult = 26397)>]
[<TestCase("day10_input.txt", ExpectedResult = 415953)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    let corruptedScore = function ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137 | _ -> 0
    
    let analised =
        lines
        |> List.map Analisys.analyse
    
    let result1 =
        analised
        |> List.choose (function Corrupted (_,f) -> Some f | _ -> None)
        |> List.map corruptedScore
        |> List.sum
    printfn "%A" analised
    printfn "%A" result1
    result1

[<TestCase("day10_testinput.txt", ExpectedResult = 288957)>]
[<TestCase("day10_input.txt", ExpectedResult = 2292863731L)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName

    let completionBracketScore = function ')' -> 1L | ']' -> 2L | '}' -> 3L | '>' -> 4L | _ -> 0L
    
    let rec completionScore score (completed: char list)  =
        match completed with
        | [] -> score
        | c :: next -> 
            let cs = completionBracketScore c
            completionScore (score * 5L + cs) next

    let analised =
        lines
        |> List.map Analisys.analyse
    
    let matching =
        analised
        |> List.choose (function Incomplete unm -> Some unm | _ -> None)
        |> List.map Analisys.fillUnmatched
        |> List.map (completionScore 0)
    
    let result2 =
        matching
        |> List.sort
        |> List.skip (matching.Length / 2)
        |> List.head
    
    printfn "%A" analised
    printfn "%A" matching
    printfn "%A" result2
    result2

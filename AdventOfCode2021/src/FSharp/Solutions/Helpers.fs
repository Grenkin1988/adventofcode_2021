module Solutions.Helpers

open System

let readLines filePath = 
    System.IO.File.ReadLines(filePath)
    |> List.ofSeq

let linesToNymbers (lines: string list) = lines |> List.map int

let splitAndMap (separator: string) f (lines: string list) =
    lines
    |> List.map (fun line -> 
        line.Split(separator, StringSplitOptions.TrimEntries) 
        |> Array.filter (String.IsNullOrWhiteSpace >> not) 
        |> Array.map f |> List.ofArray)

let splitToInt (separator: string) (lines: string list) =
    splitAndMap separator int lines

let readInput fileName = 
    let fileName = sprintf "%s/Input/%s" __SOURCE_DIRECTORY__ fileName
    readLines fileName

let matrix (lines: string list) =
    let empty = Array2D.create lines.Length lines.Length 0
    let matrix =
        lines
        |> List.map (fun line -> line |> Seq.map (fun c -> int c - int '0') |> Array.ofSeq |> Array.indexed)
        |> List.indexed
        |> List.fold (fun mat (i,line) -> 
            line 
            |> Array.fold (fun mat (j,v) -> 
                Array2D.set mat i j v
                mat) mat) empty
    matrix

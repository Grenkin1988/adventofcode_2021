module Helpers

let readLines filePath = 
    System.IO.File.ReadLines(filePath)
    |> List.ofSeq

let lineToNymbers (lines: string list) = lines |> List.map int

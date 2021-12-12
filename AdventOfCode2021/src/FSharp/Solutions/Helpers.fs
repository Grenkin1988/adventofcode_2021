module Solutions.Helpers

let readLines filePath = 
    System.IO.File.ReadLines(filePath)
    |> List.ofSeq

let linesToNymbers (lines: string list) = lines |> List.map int

let readInput fileName = 
    let fileName = sprintf "%s/Input/%s" __SOURCE_DIRECTORY__ fileName
    readLines fileName

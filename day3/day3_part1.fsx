open System

let testData =
    [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]

let readLines filePath = System.IO.File.ReadLines(filePath)
let lines = readLines (__SOURCE_DIRECTORY__ + "/day3_input.txt")

let result =
    lines
    |> Seq.head
    |> Seq.mapi (fun i _ -> 
        lines 
        |> Seq.map (fun line -> line.[i])
        |> Seq.map (function '1' -> (1, 0) | '0' -> (0, 1) | _ -> failwith "Wrong"))
    |> Seq.map (fun seq -> seq |> Seq.fold (fun (ff, ss) (f,s) -> ff + f, ss + s) (0,0))
    |> Seq.map (fun (i,j) -> i >= j, j >= i)
    |> Seq.map (fun (i,j) -> System.Convert.ToInt32(i),System.Convert.ToInt32(j))
    |> Seq.fold (fun (s1,s2) (i,j) -> s1 + i.ToString(),s2 + j.ToString()) ("","")
    |> fun (s1,s2) -> System.Convert.ToInt32(s1, 2),System.Convert.ToInt32(s2, 2)

Console.WriteLine(result)
let (x,y) = result
Console.WriteLine(x * y)
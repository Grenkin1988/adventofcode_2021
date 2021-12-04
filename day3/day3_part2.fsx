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
let lines = readLines (__SOURCE_DIRECTORY__ + "/day3_input.txt") |> List.ofSeq

let result =
    let calculateAtPosition (data: string list) position =
        data 
        |> List.map (fun line -> line.[position])
        |> List.map (function '1' -> (1, 0) | '0' -> (0, 1) | _ -> failwith "Wrong")
        |> List.fold (fun (ff, ss) (f,s) -> ff + f, ss + s) (0,0)

    let rec rating data position (bitCriteria: int*int -> char) =
        let res = calculateAtPosition data position
        let value = bitCriteria(res)
        let filtered = data |> List.filter (fun line -> line.[position] = value)
        match filtered with
        | [single] -> single
        | _ -> rating filtered (position + 1) bitCriteria

    let oxygen = rating lines 0 (fun (x,y) -> if x >= y then '1' else '0')
    let co2 = rating lines 0 (fun (x,y) -> if y <= x then '0' else '1')
    (oxygen, co2)
    |> fun (s1,s2) -> System.Convert.ToInt32(s1, 2),System.Convert.ToInt32(s2, 2)

Console.WriteLine(result)
let (x,y) = result
Console.WriteLine(x * y)
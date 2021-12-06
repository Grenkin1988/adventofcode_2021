open System
let fileName = sprintf "%s/%s" __SOURCE_DIRECTORY__ "/day3_input.txt"
let lines = Helpers.readLines fileName

let part1 =
    lines
    |> Seq.head
    |> Seq.mapi (fun i _ -> 
        lines 
        |> Seq.map (fun line -> line.[i])
        |> Seq.map (function '1' -> (1, 0) | '0' -> (0, 1) | _ -> failwith "Wrong"))
    |> Seq.map (fun seq -> seq |> Seq.fold (fun (ff, ss) (f,s) -> ff + f, ss + s) (0,0))
    |> Seq.map (fun (i,j) -> i >= j, j >= i)
    |> Seq.map (fun (i,j) -> System.Convert.ToInt32(i),System.Convert.ToInt32(j))
    |> Seq.fold (fun (s1,s2) (i,j) -> s1 + i.ToString(), s2 + j.ToString()) ("","")
    |> fun (s1,s2) -> System.Convert.ToInt32(s1, 2),System.Convert.ToInt32(s2, 2)

printfn "%A" part1
let (x,y) = part1
printfn "%A" (x * y)

let part2 =
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

printfn "%A" part2
let (x',y') = part2
printfn "%A" (x' * y')

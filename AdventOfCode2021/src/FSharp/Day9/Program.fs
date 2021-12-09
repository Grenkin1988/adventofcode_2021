open System
let fileName = sprintf "%s/%s" __SOURCE_DIRECTORY__ "/day9_input.txt"
let lines = Helpers.readLines fileName

let matrix =
    lines
    |> List.map (fun line -> line.ToCharArray() |> Array.map (string >> int))
    |> Array.ofList

let withIndex = matrix |> Array.mapi (fun i a -> i, a |> Array.mapi(fun j n -> j,n))

let windowed = withIndex |> Array.map (fun (_,a) -> a |> Array.windowed 3)

let findLowPoints index (full: (int)[][]) (row: (int*int)[][]) =
    let checkHorizontal length (i: int) (a: (int*int)[]) =
        let n0 = snd a.[0]
        let n1 = snd a.[1]
        let n2 = snd a.[2]
        match i with
        | 0 -> 
            if n0 < n1 then Some a.[0]
            elif n1 < n2 then Some a.[1]
            else None
        | i when i = (length - 1) ->
            if n2 < n1 then Some a.[2] 
            elif n1 < n0 then Some a.[1]
            else None
        | i -> 
            if n0 > n1 && n1 < n2 then Some a.[1]
            else None

    let checkVertical (i,number) =
        match index with
        | 0 -> 
            let next = full.[index + 1].[i]
            if number < next then Some ((index,i), number)
            else None
        | index when index = (full.Length - 1) ->
            let prev = full.[index - 1].[i]
            if number < prev then Some ((index,i), number)
            else None
        | index ->
            let next = full.[index + 1].[i]
            let prev = full.[index - 1].[i]
            if number < prev && number < next then Some ((index,i), number)
            else None
    let horizontal =
        row
        |> Array.mapi (checkHorizontal row.Length)
        |> Array.choose id
    horizontal
    |> Array.map checkVertical
    |> Array.choose id

let check = 
    windowed
    |> Array.mapi (fun i a -> findLowPoints i matrix a)
    |> Array.filter (Array.isEmpty >> not)
 
let sum =
    check
    |> Array.map (fun a -> a |> Array.map (fun (_,n) -> n + 1) |> Array.sum)
    |> Array.sum

printfn "%A" sum

let findBasinPoints (full: (int)[][]) point =
    let checkPoint ((i,j),n) =
        let checkVertical () =
            match i with
            | 0 -> 
                let next = full.[i + 1].[j]
                if 9 <> next then [ ((i + 1,j), next) ]
                else []
            | i when i = (full.Length - 1) ->
                let prev = full.[i - 1].[j]
                if 9 <> prev then [ ((i - 1,j), prev) ]
                else []
            | i ->
                let next = full.[i + 1].[j]
                let prev = full.[i - 1].[j]
                if 9 <> prev && 9 <> next then [ ((i-1,j), prev); ((i+1,j), next) ]
                elif 9 <> prev then [ ((i-1,j), prev) ]
                elif 9 <> next then [ ((i+1,j), next) ]
                else []

        let checkHorizontal () =
            match j with
            | 0 -> 
                let next = full.[i].[j+1]
                if 9 <> next then [ ((i,j+1), next) ]
                else []
            | j when j = (full.[i].Length - 1) ->
                let prev = full.[i].[j-1]
                if 9 <> prev then [ ((i,j-1), prev) ]
                else []
            | j ->
                let next = full.[i].[j+1]
                let prev = full.[i].[j-1]
                if 9 <> prev && 9 <> next then [ ((i,j-1), prev); ((i,j+1), next) ]
                elif 9 <> prev then [ ((i,j-1), prev) ]
                elif 9 <> next then [ ((i,j+1), next) ]
                else []

        checkHorizontal() |> List.append (checkVertical())

    let rec checkPoints points result =
        match points with
        | [] -> result
        | head :: tail ->
            let newPoints = checkPoint head |> List.filter (fun p -> List.contains p result |> not)
            let result = head :: result |> List.distinct
            checkPoints (tail @ newPoints) result

    checkPoints [point] []

let collected =
    check
    |> Array.collect id

let basins =
    collected
    |> Array.map (fun p -> findBasinPoints matrix p)
    |> Array.sortByDescending List.length

let result = basins |> Array.take 3 |> Array.map List.length |> Array.fold (*) 1

// For more information see https://aka.ms/fsharp-console-apps
printfn "%A" result

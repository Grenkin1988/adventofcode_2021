[<NUnit.Framework.TestFixture>]
module Solutions.Day11

open Solutions.Helpers
open NUnit.Framework

module Array2D =
    let find2D f (arr: 'a [,]) = Seq.choose id <| seq {
        for i in 0..(arr.GetLength 0 - 1) do
            for j in 0..(arr.GetLength 1 - 1) do
                if f arr.[i,j] 
                    then yield Some arr.[i,j]
                    else yield None
    }

    let flatMap f (arr: 'a [,]) = seq {
        for i in 0..(arr.GetLength 0 - 1) do
            for j in 0..(arr.GetLength 1 - 1) do
                yield f arr.[i,j]
    }

type DumboOctopus =
    {
        Energy: int
        Position: int * int
        HasFlashed: bool
        Adjacent: (int * int) list
    }

module DumboOctopus =
    let getAdjacent length (x, y) =
        let length = length - 1
        match (x, y) with
        // Corners
        | (0, 0) ->
            [(x, y+1); (x+1, y); (x+1, y+1)]
        | (x, y) when x = length && y = length ->
            [(x, y-1); (x-1, y); (x-1, y-1)]
        | (0, y) when y = length ->
            [(x, y-1); (x+1, y); (x+1, y-1)]
        | (x, 0) when x = length ->
            [(x-1, y); (x, y+1); (x-1, y+1)]
        // Edges
        | (x, y) when x = length ->
            [(x, y+1); (x-1, y+1); (x-1, y); (x-1, y-1); (x, y-1)]
        | (0, y) ->
            [(x, y+1); (x+1, y+1); (x+1, y); (x+1, y-1); (x, y-1)]        
        | (x, y) when y = length ->
            [(x+1, y); (x+1, y-1); (x, y-1); (x-1,y-1); (x-1,y)]
        | (x, 0) ->
            [(x, y+1); (x+1, y+1); (x+1, y); (x-1,y); (x-1,y+1)]

        | (x, y) ->
            [ (x, y+1); (x+1, y+1); (x+1,y); (x+1,y-1); (x,y-1); (x-1,y-1); (x-1,y); (x-1,y+1) ]

    let create length position value =
        { Energy = value; Position = position; HasFlashed = false; Adjacent = getAdjacent length position }

    let gainEnergy octo =
        { octo with Energy = octo.Energy + 1}

    let shouldFlash octo =
        not octo.HasFlashed && octo.Energy > 9

    let recharge octo =
        { octo with HasFlashed = false; Energy = 0 }

module DumboOctopusesField =
    let step (field: DumboOctopus[,]) =
        let gainedEnergy = field |> Array2D.map DumboOctopus.gainEnergy

        let rec flash (field: DumboOctopus[,]) shoulFlash =
            let flashSingle fl =
                let (x,y) = fl.Position
                Array2D.set field x y { fl with HasFlashed = true }
                fl.Adjacent
                |> List.iter (fun (x,y) -> Array2D.set field x y (DumboOctopus.gainEnergy field.[x,y]))

            shoulFlash |> List.iter flashSingle
            let shoulFlash = 
                gainedEnergy
                |> Array2D.find2D DumboOctopus.shouldFlash
                |> List.ofSeq
            match shoulFlash with
            | [] -> ()
            | _ ->
                flash field shoulFlash

        let shoulFlash = 
            gainedEnergy
            |> Array2D.find2D DumboOctopus.shouldFlash
            |> List.ofSeq
        flash gainedEnergy shoulFlash

        let flashed =
            gainedEnergy 
            |> Array2D.flatMap (fun o -> if o.HasFlashed then 1 else 0)
            |> Seq.sum

        gainedEnergy 
        |> Array2D.iteri (fun x y v -> if v.HasFlashed then gainedEnergy.[x,y] <- DumboOctopus.recharge v)

        gainedEnergy, (flashed)

[<TestCase("day11_testinput.txt", ExpectedResult = 1656)>]
[<TestCase("day11_input.txt", ExpectedResult = 1620)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    let empty = Array2D.create lines.Length lines.Length 0
    let matrix =
        lines
        |> List.map (fun line -> line.ToCharArray() |> Array.map (string >> int) |> Array.indexed)
        |> List.indexed
        |> List.fold (fun mat (i,line) -> 
            line 
            |> Array.fold (fun mat (j,v) -> 
                Array2D.set mat i j v
                mat) mat) empty
    let length = matrix.[0,*].Length
    let octopuses =
        matrix
        |> Array2D.mapi (fun i j v -> DumboOctopus.create length (i,j) v)

    let octopuses =
        [1..100]
        |> List.fold (fun (oct,fl) _ -> 
            let (octo,flashed) = DumboOctopusesField.step oct
            (octo, fl + flashed)) (octopuses,0)
    snd octopuses

[<TestCase("day11_testinput.txt", ExpectedResult = 195)>]
[<TestCase("day11_input.txt", ExpectedResult = 371)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    let lines = readInput fileName
    let empty = Array2D.create lines.Length lines.Length 0
    let matrix =
        lines
        |> List.map (fun line -> line.ToCharArray() |> Array.map (string >> int) |> Array.indexed)
        |> List.indexed
        |> List.fold (fun mat (i,line) -> 
            line 
            |> Array.fold (fun mat (j,v) -> 
                Array2D.set mat i j v
                mat) mat) empty
    let length = matrix.[0,*].Length
    let octopuses =
        matrix
        |> Array2D.mapi (fun i j v -> DumboOctopus.create length (i,j) v)

    let octopuses =
        Seq.initInfinite id
        |> Seq.scan (fun (oct,fl) _ -> DumboOctopusesField.step oct) (octopuses,0)
        |> Seq.findIndex (fun (_,fl) -> fl = 100)
    octopuses

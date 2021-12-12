[<NUnit.Framework.TestFixture>]
module Solutions.Day04

open Solutions.Helpers
open NUnit.Framework
open System

type Row = (int * bool)[]

module Row =
    let checkNumber number (row: Row) =
        row
        |> Array.map (fun (n, c) -> if n = number then n, true else n, c)

[<CustomEquality;NoComparison>]
type Card = 
    {
        ID: Guid
        Rows: Row list
        Numbers: Set<int>
    }
    override x.Equals(yobj) =  
        match yobj with 
        | :? Card as y -> 
           y.ID = x.ID
        | _ -> false 
    override x.GetHashCode() = x.ID.GetHashCode()

module Card =
    let create (rows: Row list) =
        let numbers =
            rows
            |> List.collect (Array.map fst >> List.ofArray)
            |> Set.ofList
        { Rows = rows; Numbers = numbers; ID = Guid.NewGuid() }

    let checkNymber number (card: Card) =
        if not <| Set.contains number card.Numbers
        then card
        else
            let rows =
                card.Rows
                |> List.map (Row.checkNumber number)
            { card with Rows = rows }

    let isBingo (card: Card) =
        let rows = card.Rows |> List.map (fun a -> a |> Array.forall (fun (_, w) -> w))
        let firstRowHaveChecks = card.Rows.Head |> Array.exists (fun (_, w) -> w)
        if rows |> List.exists id then
            true
        elif firstRowHaveChecks then
            let columns = 
                card.Rows.Head 
                |> Array.mapi (fun i _ -> 
                    card.Rows |> List.forall (fun a -> snd a.[i]))
            columns |> Array.exists id
        else
            false

    let getScore (card: Card) =
        card.Rows 
        |> List.map (fun a -> 
            a 
            |> Array.map (fun (n, w) -> if w then 0 else n)
            |> Array.sum)
        |> List.sum

[<TestCase("day04_testinput.txt", ExpectedResult = 4512)>]
[<TestCase("day04_input.txt", ExpectedResult = 14093)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    let numbers = [lines.Head] |> splitToInt "," |> List.head
    
    let boards = lines.Tail |> List.filter (String.IsNullOrEmpty >> not) |> List.chunkBySize 5
    let boards = 
        boards 
        |> List.map (fun list ->
            list |> splitAndMap " " (fun i -> (int i,false)) |> List.map (fun l -> l |> Array.ofList ))
        |> List.map Card.create
    
    let rec playNumber numbers boards =
        match numbers with
        | [] -> failwith "BAD"
        | current :: next ->
            let boards = boards |> List.map (Card.checkNymber current)
            let winner = boards |> List.tryFind Card.isBingo
            match winner with
            | Some win -> win, current
            | None -> playNumber next boards
    
    let winner, current = playNumber numbers boards
    let winnerScore = Card.getScore winner
    
    printfn "%A" winner
    printfn "%i" winnerScore
    (winnerScore * current)

[<TestCase("day04_testinput.txt", ExpectedResult = 1924)>]
[<TestCase("day04_input.txt", ExpectedResult = 17388)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    let numbers = [lines.Head] |> splitToInt "," |> List.head
    let boards = lines.Tail |> List.filter (String.IsNullOrEmpty >> not) |> List.chunkBySize 5
    let boards = 
        boards 
        |> List.map (fun list ->
            list |> splitAndMap " " (fun i -> (int i,false)) |> List.map (fun l -> l |> Array.ofList ))
        |> List.map Card.create

    let rec playNumber numbers boards sortedWinnners =
        match numbers with
        | [] -> failwith "BAD"
        | current :: next ->
            let boards' = boards |> List.map (Card.checkNymber current)
            let winners = boards' |> List.filter Card.isBingo
            let sortedWinnners = winners |> List.except sortedWinnners |> List.append sortedWinnners
            if sortedWinnners.Length = boards.Length then
                List.last sortedWinnners, current
            else
                playNumber next boards' sortedWinnners

    let winner, current = playNumber numbers boards []
    let winnerScore = Card.getScore winner
    
    printfn "%A" winner
    printfn "%i" winnerScore
    (winnerScore * current)

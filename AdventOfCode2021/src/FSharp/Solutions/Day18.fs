[<NUnit.Framework.TestFixture>]
module Solutions.Day18

open System
open Solutions.Helpers
open NUnit.Framework

type SnailNumber =
    | Number of number: int
    | Nested of left: SnailNumber * right: SnailNumber

module SnailNumber =
    let rec create (line: string) =
        let left, rest =
            if line.StartsWith("[") then
                line.Substring(1) |> create
            else
                let sp = line.IndexOf(",")
                let l = line.Split(",", StringSplitOptions.TrimEntries).[0]
                l |> int |> Number, line.Substring(sp + 1)
        if rest = "" || rest.Replace("]","") = "" then
            left, rest
        else
            let right, rest =
                if rest.StartsWith("[") then
                    rest.Substring(1) |> create
                else
                    let sp = rest.IndexOf("]")
                    let r = rest.Split("]", StringSplitOptions.TrimEntries).[0]
                    r |> int |> Number, rest.Substring(sp + 1).TrimStart(']').TrimStart(',')
            Nested (left, right), rest

    let rec addToLeft toAdd (number: SnailNumber) =
        let tryAddToLeft toAdd (number: SnailNumber) =
            match number with
            | Nested(Number l, Nested(Number a, Number r)) when a = toAdd ->
                Nested(Number(l + a), Nested(Number a, Number r)) |> Some
            | _ -> None
        match number with
        | Number _ -> number
        | Nested (l, r) ->
            match tryAddToLeft toAdd r with
            | Some r -> Nested(l, r)
            | None -> 
                let r = addToLeft toAdd r
                Nested(l, r)

    let rec addToRight toAdd (number: SnailNumber) =
        let tryAddToRight toAdd (number: SnailNumber) =
            match number with
            | Nested(Nested(Number l, Number a), Number r) when a = toAdd ->
                Nested(Nested(Number l, Number a), Number (r+a)) |> Some
            | _ -> None
        match number with
        | Number _ -> number
        | Nested (l, r) ->
            match tryAddToRight toAdd l with
            | Some l -> Nested(l, r)
            | None -> 
                let l = addToRight toAdd l
                Nested(l, r)

    let tryExplode (number: SnailNumber) =
        let rec tryExplode nesting (number: SnailNumber) (initial: SnailNumber) =
            match number with
            | Number _ -> None
            | Nested(Number l, Number r) when nesting >= 4 ->
                let rec explode (number: SnailNumber) =
                    match number with
                    | Number _ -> number
                    | Nested(Number nl, Number nr) when nl = l && nr = r ->
                        Number 0
                    | Nested(l,r) ->
                        Nested(explode l, explode r)

                addToLeft l initial
                |> addToRight r
                |> explode
                |> Some
            | Nested (l,r) -> 
                let nesting = nesting + 1
                match tryExplode nesting l initial with
                | Some n -> Some n
                | None -> tryExplode nesting r initial
        tryExplode 0 number number

    let rec trySplit (number: SnailNumber) =
        match number with
        | Number n when n >= 10 ->
            let l = n / 2
            Nested(Number l, Number (n - l)) |> Some
        | Number _ -> None
        | Nested(l,r) ->
            match trySplit l with
            | Some l -> Nested(l, r) |> Some
            | None ->
                match trySplit r with
                | Some r -> Nested(l, r) |> Some
                | None -> None

    let rec reduce (number: SnailNumber) =
        match tryExplode number with
        | Some n -> reduce n
        | None ->
            match trySplit number with
            | Some n -> reduce n
            | None -> number

    let tryExplode2 (line: string) =
        let rec findNested nested (line: string) =
            if line.Length < 1 then None
            else
                let c = line.[0]
                let n = line.Substring(1)
                match c with
                | '[' -> findNested (nested + 1) n
                | ']' -> findNested (nested - 1) n
                | _ when nested >= 5 ->
                    let next = line.IndexOf("]")
                    let nums = line.Substring(0, next)
                    Some nums
                | _ ->
                    let n1 = max (n.IndexOf("[")) 0
                    let n2 = max (n.IndexOf("]")) 0
                    findNested nested (n.Substring(min n1 n2))
        match findNested 0 line with
        | None -> None
        | Some nested ->
            let sp = nested.Split(",")
            let l,r = sp.[0] |> int, sp.[1] |> int
            let index = line.IndexOf(nested)
            let before = line.Substring(0, index - 1)
            let after = line.Substring(index + 1 + nested.Length)
            let addToLeft (line: string) =
                match line |> Seq.tryFindIndexBack Char.IsDigit with
                | Some index ->
                    let num = line.[index] |> string |> int
                    let before = line.Substring(0, index)
                    let after = line.Substring(index+1)
                    sprintf "%s%i%s" before (num + l) after
                | None -> line
            let addToRight (line: string) =
                match line |> Seq.tryFindIndex Char.IsDigit with
                | Some index ->
                    let num = line.[index] |> string |> int
                    let before = line.Substring(0, index)
                    let after = line.Substring(index+1)
                    sprintf "%s%i%s" before (num + r) after
                | None -> line

            let before = addToLeft before
            let after = addToRight after

            sprintf "%s%i%s" before 0 after
            |> Some

    let trySplit2 (line: string) =
        match line.Split([|","; "]"; "["|], StringSplitOptions.TrimEntries) |> Array.tryFind (fun s -> s.Length > 1) with
        | Some num ->
            let n = num |> int
            let l = n / 2
            let r = n - l
            let sp = sprintf "[%i,%i]" l r
            line.Replace(num, sp) |> Some
        | None -> None

    let rec reduce2 (line: string) =
        match tryExplode2 line with
        | Some n -> reduce2 n
        | None ->
            match trySplit2 line with
            | Some n -> reduce2 n
            | None -> line

//[<TestCase("[[[[[9,8],1],2],3],4]", ExpectedResult = 54)>]
//[<TestCase("[7,[6,[5,[4,[3,2]]]]]", ExpectedResult = 54)>]
//[<TestCase("[[6,[5,[4,[3,2]]]],1]", ExpectedResult = 54)>]
//[<TestCase("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", ExpectedResult = 54)>]
//[<TestCase("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", ExpectedResult = 54)>]
[<TestCase("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", ExpectedResult = 3)>]
[<TestCase("04005AC33890", ExpectedResult = 54)>]
[<TestCase("880086C3E88112", ExpectedResult = 7)>]
[<TestCase("CE00C43D881120", ExpectedResult = 9)>]
[<TestCase("D8005AC2A8F0", ExpectedResult = 1)>]
[<TestCase("F600BC2D8F", ExpectedResult = 0)>]
[<TestCase("9C005AC2F8F0", ExpectedResult = 0)>]
[<TestCase("9C0141080250320F1802104A08", ExpectedResult = 1)>]
let ``Tests``(packet) =

    let n,s = SnailNumber.create packet

    let uuu = SnailNumber.reduce2 packet

    let rrrrrr = SnailNumber.tryExplode2 packet

    packet

[<TestCase("day18_testinput.txt", ExpectedResult = 4140)>]
[<TestCase("day18_input.txt", ExpectedResult = 619)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    


    lines

[<TestCase("day18_testinput.txt", ExpectedResult = 40)>]
[<TestCase("day18_input.txt", ExpectedResult = 619)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    


    lines

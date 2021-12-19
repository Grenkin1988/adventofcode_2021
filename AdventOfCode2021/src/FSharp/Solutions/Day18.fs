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

    let tryGetNumbers (line: string) =
        line.Split([|","; "]"; "["|], StringSplitOptions.TrimEntries)
        |> Array.choose (fun n ->
            match Int32.TryParse n with
            | false, _ -> None
            | true, n -> Some n)

    let tryExplode (line: string) =
        let rec internalExplode nesting chars index =
            match chars with
            | [] -> line
            | '[' :: t -> internalExplode (nesting + 1) t (index + 1)
            | ']' :: t -> internalExplode (nesting - 1) t (index + 1)
            | n :: t when Char.IsNumber n && nesting >= 5 ->
                let until = line.Substring(index).IndexOf(']') + index
                let nums = line.Substring(index, until - index).Split(",")
                let l,r = nums.[0] |> int, nums.[1] |> int
                let addToLeft () =
                    let before = line.Substring(0, index - 1)
                    match tryGetNumbers before with
                    | [||] -> before
                    | nums ->
                        let n = nums |> Array.last
                        let ns = n |> string
                        let ni = before.LastIndexOf(ns)
                        let be = before.Substring(0, ni)
                        let af = before.Substring(ni + ns.Length)
                        sprintf "%s%i%s" be (n + l) af
                let addToRight () =
                    let after = line.Substring(until + 1)
                    match tryGetNumbers after with
                    | [||] -> after
                    | nums ->
                        let n = nums |> Array.head
                        let ns = n |> string
                        let ni = after.IndexOf(ns)
                        let be = after.Substring(0, ni)
                        let af = after.Substring(ni + ns.Length)
                        sprintf "%s%i%s" be (n + r) af

                let left = addToLeft()
                let right = addToRight()

                sprintf "%s0%s" left right
            | c :: t -> internalExplode nesting t (index + 1)
        internalExplode 0 (line.ToCharArray() |> List.ofArray) 0

    let trySplit (line: string) =
        match tryGetNumbers line with
        | [||] -> line
        | nums ->
            match Array.tryFind (fun n -> n > 9) nums with
            | None -> line
            | Some n ->
                let ns = string n
                let index = line.IndexOf(ns)
                let be = line.Substring(0, index)
                let af = line.Substring(index + ns.Length)
                sprintf "%s[%i,%i]%s" be (n/2) (n-(n/2)) af

    let rec reduce (line: string) =
        let after = tryExplode line
        if after <> line then
            reduce after
        else
            let after = trySplit line
            if after <> line then
                reduce after
            else
                line

    let add (line1: string) (line2: string) =
        match line1, line2 with
        | "", "" -> ""
        | "", l2 -> l2
        | l1, "" -> l1
        | _ -> sprintf "[%s,%s]" line1 line2

    let magnitude (line: string) =
        let number,_ = create line
        let rec magnitude number =
            match number with
            | Number n -> n
            | Nested(Number l, Number r) ->
                3 * l + 2 * r
            | Nested(l, r) ->
                3 * (magnitude l) + (2 * magnitude r)
        magnitude number

[<TestCase("[[[[[9,8],1],2],3],4]", ExpectedResult = "[[[[0,9],2],3],4]")>]
[<TestCase("[7,[6,[5,[4,[3,2]]]]]", ExpectedResult = "[7,[6,[5,[7,0]]]]")>]
[<TestCase("[[6,[5,[4,[3,2]]]],1]", ExpectedResult = "[[6,[5,[7,0]]],3]")>]
[<TestCase("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", ExpectedResult = "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
[<TestCase("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", ExpectedResult = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
let ``Test 1``(packet) =

    SnailNumber.reduce packet

[<TestCase("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", ExpectedResult = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
[<TestCase("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]", "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]", ExpectedResult = "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")>]
[<TestCase("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]", "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]", ExpectedResult = "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]")>]
[<TestCase("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]", "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]", ExpectedResult = "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]")>]
[<TestCase("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]", "[7,[5,[[3,8],[1,4]]]]", ExpectedResult = "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]")>]
[<TestCase("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]", "[[2,[2,2]],[8,[8,1]]]", ExpectedResult = "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]")>]
[<TestCase("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]", "[2,9]", ExpectedResult = "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]")>]
[<TestCase("[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]", "[1,[[[9,3],9],[[9,0],[0,7]]]]", ExpectedResult = "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]")>]
[<TestCase("[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]", "[[[5,[7,4]],7],1]", ExpectedResult = "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]")>]
[<TestCase("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]", "[[[[4,2],2],6],[8,7]]", ExpectedResult = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")>]
let ``Test 2``(first, second) =
    
    let sum = SnailNumber.add first second
    let result = SnailNumber.reduce sum

    result

[<TestCase("[[1,2],[[3,4],5]]", ExpectedResult = 143)>]
[<TestCase("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", ExpectedResult = 1384)>]
[<TestCase("[[[[1,1],[2,2]],[3,3]],[4,4]]", ExpectedResult = 445)>]
[<TestCase("[[[[3,0],[5,3]],[4,4]],[5,5]]", ExpectedResult = 791)>]
[<TestCase("[[[[5,0],[7,4]],[5,5]],[6,6]]", ExpectedResult = 1137)>]
[<TestCase("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", ExpectedResult = 3488)>]
let ``Test 3``(packet) =

    SnailNumber.magnitude packet

[<TestCase("day18_tiny_test_1.txt", ExpectedResult = "[[[[1,1],[2,2]],[3,3]],[4,4]]")>]
[<TestCase("day18_tiny_test_2.txt", ExpectedResult = "[[[[3,0],[5,3]],[4,4]],[5,5]]")>]
[<TestCase("day18_tiny_test_3.txt", ExpectedResult = "[[[[5,0],[7,4]],[5,5]],[6,6]]")>]
[<TestCase("day18_testinput_1.txt", ExpectedResult = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")>]
[<TestCase("day18_testinput_2.txt", ExpectedResult = "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")>]
let ``Test 4``(fileName) =
    let lines = readInput fileName
    
    let folder prev next =
        let sum = SnailNumber.add prev next
        SnailNumber.reduce sum

    let result = 
        List.fold folder "" lines

    result

[<TestCase("day18_testinput_2.txt", ExpectedResult = 4140)>]
[<TestCase("day18_input.txt", ExpectedResult = 3359)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    
    let folder prev next =
        let sum = SnailNumber.add prev next
        SnailNumber.reduce sum

    let result = 
        List.fold folder "" lines

    SnailNumber.magnitude result

let allPairs (arr: 'a[]) = Seq.choose id <| seq {
        for i in 0..(arr.Length - 1) do
            for j in 0..(arr.Length - 1) do
                if i <> j 
                then yield Some (arr.[i], arr.[j])
                else yield None
   }

[<TestCase("day18_testinput_2.txt", ExpectedResult = 3993)>]
[<TestCase("day18_input.txt", ExpectedResult = 4616)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName |> Array.ofList
    
    let allPairs = lines |> allPairs

    let magnitudes = 
        allPairs 
        |> Seq.map (fun (l,r) -> 
            SnailNumber.add l r 
            |> SnailNumber.reduce 
            |> SnailNumber.magnitude)

    magnitudes |> Seq.max

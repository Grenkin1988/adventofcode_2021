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
        let rec findNested nested (line: string) =
            if line.Length < 1 then None
            else
                let c = line.[0]
                let n = line.Substring(1)
                match c with
                | '[' -> findNested (nested + 1) n
                | ']' -> findNested (nested - 1) n
                | n when Char.IsNumber n && nested >= 5 ->
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
                match tryGetNumbers line with
                | [||] -> line
                | numbers ->
                    let num = numbers |> Array.last
                    let nums = num.ToString()
                    let index = line.LastIndexOf(nums)
                    let before = line.Substring(0, index)
                    let after = line.Substring(index+nums.Length)
                    sprintf "%s%i%s" before (num + l) after
            let addToRight (line: string) =
                match tryGetNumbers line with
                | [||] -> line
                | numbers ->
                    let num = numbers.[0]
                    let nums = num.ToString()
                    let index = line.IndexOf(nums)
                    let before = line.Substring(0, index)
                    let after = line.Substring(index+nums.Length)
                    sprintf "%s%i%s" before (num + r) after

            let before = addToLeft before
            let after = addToRight after

            sprintf "%s%i%s" before 0 after
            |> Some

    let trySplit (line: string) =
        match line.Split([|","; "]"; "["|], StringSplitOptions.TrimEntries) |> Array.tryFind (fun s -> s.Length > 1) with
        | Some num ->
            let n = num |> int
            let l = n / 2
            let r = n - l
            let sp = sprintf "[%i,%i]" l r
            let index = line.IndexOf(num)
            let before = line.Substring(0, index)
            let after = line.Substring(index+num.Length)
            sprintf "%s%s%s" before sp after |> Some
        | None -> None

    let rec reduce (line: string) =
        match tryExplode line with
        | Some n -> reduce n
        | None ->
            match trySplit line with
            | Some n -> reduce n
            | None -> line

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
//[<TestCase("day18_input.txt", ExpectedResult = 619)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    
    let folder prev next =
        let sum = SnailNumber.add prev next
        SnailNumber.reduce sum

    let result = 
        List.fold folder "" lines

    SnailNumber.magnitude result

//[<TestCase("day18_testinput.txt", ExpectedResult = 40)>]
//[<TestCase("day18_input.txt", ExpectedResult = 619)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    


    lines

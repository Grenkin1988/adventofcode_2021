open System
let fileName = sprintf "%s/%s" __SOURCE_DIRECTORY__ "/day8_input.txt"
let lines = Helpers.readLines fileName

let split = lines |> List. map (fun line -> line.Split("|", StringSplitOptions.TrimEntries))

let toDigits (line: string) = line.Split(" ", StringSplitOptions.TrimEntries) |> List.ofArray

let afterPipe = 
    split
    |> List.map (fun line -> toDigits line.[1])

let pipeSplit = 
    split
    |> List.map (fun line -> toDigits line.[0], toDigits line.[1])

let distinct = [ 2; 4; 3; 7]

let matches =
    afterPipe
    |> List.map (fun list -> list |> List.fold (fun n l -> if List.contains l.Length distinct then n + 1 else n) 0)
    |> List.sum

printfn "%A" matches

type SevenSegmentDisplay =
    {
        a: Set<char>
        b: Set<char>
        c: Set<char>
        d: Set<char>
        e: Set<char>
        f: Set<char>
        g: Set<char>
    }
    member display.Zero =
        Set.difference display.Eight display.d
    member display.One =
        display.c
        |> Set.union display.f
    member display.Two =
        display.b
        |> Set.union display.f
        |> Set.difference display.Eight
    member display.Three =
        display.b
        |> Set.union display.e
        |> Set.difference display.Eight
    member display.Four =
        display.a
        |> Set.union display.g
        |> Set.difference display.Nine
    member display.Five =
        Set.difference display.Six display.e
    member display.Six =
        Set.difference display.Eight display.c
    member display.Seven =
        display.One
        |> Set.union display.a
    member display.Eight =
        display.a 
        |> Set.union display.b 
        |> Set.union display.c
        |> Set.union display.d
        |> Set.union display.e
        |> Set.union display.f
        |> Set.union display.g
    member display.Nine =
        Set.difference display.Eight display.e

    static member Empty =
        { a = Set.empty; b = Set.empty; c = Set.empty; d = Set.empty; e = Set.empty; f = Set.empty; g = Set.empty; }

module SevenSegmentDisplay =
    let isDefined { a = a; b = b; c = c; d = d; e = e; f = f; g = g } =
        a.Count = 1 &&
        b.Count = 1 &&
        c.Count = 1 &&
        d.Count = 1 &&
        e.Count = 1 &&
        f.Count = 1 &&
        g.Count = 1

    let fullSet (display: SevenSegmentDisplay) =
        display.Eight

    let defineDisply (digits: string list) =
        let addOne (d: string) display =
            if d.Length <> 2 then failwith "NOT ONE"
            else
                let digits = d.ToCharArray() |> Set.ofArray
                { display with c = digits; f = digits }

        let addSeven (d: string) display =
            if d.Length <> 3 then failwith "NOT SEVEN"
            else
                let digits = d.ToCharArray() |> Set.ofArray
                let a = Set.difference digits display.c
                { display with a = a }

        let addFour (d: string) display =
            if d.Length <> 4 then failwith "NOT FOUR"
            else
                let digits = d.ToCharArray() |> Set.ofArray
                let bd = Set.difference digits display.c
                { display with b = bd; d = bd }

        let addEight (d: string) display =
            if d.Length <> 7 then failwith "NOT EIGHT"
            else
                let digits = d.ToCharArray() |> Set.ofArray
                let eg = display.c |> Set.union display.a |> Set.union display.b |> Set.difference digits 
                { display with e = eg; g = eg }

        let addZeroAndNine (d: string list) display =
            let full = fullSet display
            let addZeroOrNine display (d: string)  =
                if d.Length <> 6 then failwith "NOT ZERO OR NINE"
                else
                    let digits = d.ToCharArray() |> Set.ofArray
                    let missing =  Set.difference full digits
                    if Set.isProperSubset missing display.d then
                        { display with d = missing; b = Set.difference display.d missing }
                    elif Set.isProperSubset missing display.g then
                        { display with e = missing; g = Set.difference display.g missing }
                    else
                        { display with c = missing; f = Set.difference display.f missing }
            
            d |> List.fold addZeroOrNine display

        let one = digits |> List.find (fun l -> l.Length = 2)
        let disp = addOne one SevenSegmentDisplay.Empty
        let seven = digits |> List.find (fun l -> l.Length = 3)
        let disp = addSeven seven disp
        let four = digits |> List.find (fun l -> l.Length = 4)
        let disp = addFour four disp
        let eight = digits |> List.find (fun l -> l.Length = 7)
        let disp = addEight eight disp
        let zeroAndNine = digits |> List.filter (fun l -> l.Length = 6)
        let disp = addZeroAndNine zeroAndNine disp

        if isDefined disp then
            disp
        else failwith "THIS ISREALLY BAD"

    let displayDigit (display: SevenSegmentDisplay) (digit: string) =
        let digits = digit.ToCharArray() |> Set.ofArray
        match digits with
        | d when d = display.Zero -> "0"
        | d when d = display.One -> "1"
        | d when d = display.Two -> "2"
        | d when d = display.Three -> "3"
        | d when d = display.Four -> "4"
        | d when d = display.Five -> "5"
        | d when d = display.Six -> "6"
        | d when d = display.Seven -> "7"
        | d when d = display.Eight -> "8"
        | d when d = display.Nine -> "9"
        | _ -> failwith "WRONG INPUT"

    let printDigit (display: SevenSegmentDisplay, digits: string list) =
        digits 
        |> List.map (displayDigit display) |> String.concat ""

let result =
    pipeSplit
    |> List.map (fun (b,a) -> SevenSegmentDisplay.defineDisply b, a)
    |> List.map SevenSegmentDisplay.printDigit
    |> List.map int

printfn "%A" result
result |> List.sum |> printfn "%A"
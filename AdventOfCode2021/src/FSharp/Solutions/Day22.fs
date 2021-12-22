[<NUnit.Framework.TestFixture>]
module Solutions.Day22

open System
open Solutions.Helpers
open NUnit.Framework

type OnOff = On | Off

type Range = 
    {
        X1: int
        X2: int
        Y1: int
        Y2: int
        Z1: int
        Z2: int
    }

type Instruction = 
    {
        IsOn: bool
        X1: int
        X2: int
        Y1: int
        Y2: int
        Z1: int
        Z2: int
    }
    override this.ToString() =
        let on = if this.IsOn then "on" else "off"
        sprintf "%s x=%i..%i,y=%i..%i,z=%i..%i" on this.X1 this.X2 this.Y1 this.Y2 this.Z1 this.Z2

type Instruction2 = 
    {
        IsOn: bool
        X: int64 seq
        Y: int64 seq
        Z: int64 seq
    }

module Instruction =
    let create (line: string) : Instruction =
        let sp = line.Replace("on", "").Replace("off", "").Trim().Split(",")
        let x = sp.[0].Replace("x=", "").Split("..") |> Array.map int |> Array.sort
        let y = sp.[1].Replace("y=", "").Split("..") |> Array.map int |> Array.sort 
        let z = sp.[2].Replace("z=", "").Split("..") |> Array.map int |> Array.sort 
        {
            IsOn = line.StartsWith("on");
            X1 = x.[0];
            X2 = x.[1];
            Y1 = y.[0];
            Y2 = y.[1];
            Z1 = z.[0];
            Z2 = z.[1];
        }

    let create2 (line: string) =
        let sp = line.Replace("on", "").Replace("off", "").Trim().Split(",")
        let x = sp.[0].Replace("x=", "").Split("..") |> Array.map int64 |> Array.sort
        let y = sp.[1].Replace("y=", "").Split("..") |> Array.map int64 |> Array.sort 
        let z = sp.[2].Replace("z=", "").Split("..") |> Array.map int64 |> Array.sort
        let x1,x2 = x.[0], x.[1]
        let y1,y2 = y.[0], y.[1]
        let z1,z2 = z.[0], z.[1]
        {
            IsOn = line.StartsWith("on");
            X = seq {x1..1L..x2}
            Y = seq {y1..1L..y2}
            Z = seq {z1..1L..z2}
        }

    let clamp (range:Range) (ins: Instruction) =
        let x =
            if ins.X2 < range.X2 && ins.X2 < range.X1 then
                None
            elif ins.X1 > range.X1 && ins.X1 > range.X2 then
                None
            else
                ((max ins.X1 range.X1),(min ins.X2 range.X2)) |> Some
        let y =
            if ins.Y2 < range.Y2 && ins.Y2 < range.Y1 then
                None
            elif ins.Y1 > range.Y1 && ins.Y1 > range.Y2 then
                None
            else
                ((max ins.Y1 range.Y1),(min ins.Y2 range.Y2)) |> Some
        let z =
            if ins.Z2 < range.Z2 && ins.Z2 < range.Z1 then
                None
            elif ins.Z1 > range.Z1 && ins.Z1 > range.Z2 then
                None
            else
                ((max ins.Z1 range.Z1),(min ins.Z2 range.Z2)) |> Some
        match x,y,z with
        | Some (x1,x2), Some(y1,y2), Some(z1,z2) ->
            { ins with
                X1 = x1
                X2 = x2
                Y1 = y1
                Y2 = y2
                Z1 = z1
                Z2 = z2
            } |> Some
        | _ -> None

    let isValid ins =
        ins.X1 <> ins.X2 && ins.Y1 <> ins.Y2 && ins.Z1 <> ins.Z2

[<TestCase("day22_testinput_1.txt", ExpectedResult = 39)>]
[<TestCase("day22_testinput_2.txt", ExpectedResult = 590784)>]
[<TestCase("day22_testinput_3.txt", ExpectedResult = 474140)>]
[<TestCase("day22_input.txt", ExpectedResult = 570915)>]
let ``Part 1``(fileName) =
    let range = { X1 = -50; X2 = 50; Y1 = -50; Y2 = 50; Z1 = -50; Z2 = 50;}
    let lines = 
        readInput fileName 
        |> List.map (Instruction.create >> (Instruction.clamp range))
        |> List.choose id

    let result = Collections.Generic.Dictionary<int*int*int, bool>()
    
    let update (ins:Instruction) = 
        for x in ins.X1..ins.X2 do
            for y in ins.Y1..ins.Y2 do
                for z in ins.Z1..ins.Z2 do
                    result.[(x,y,z)] <- ins.IsOn
    
    lines
    |> List.iter update

    result |> Seq.map (fun kv -> if kv.Value then 1 else 0) |> Seq.sum

//[<TestCase("day22_testinput_3.txt", ExpectedResult = 2758514936282235L)>]
//[<TestCase("day22_input.txt", ExpectedResult = 974)>]
let ``Part 2``(fileName) =
    let lines = 
        readInput fileName 
        |> List.map Instruction.create2

    let mutable total = 0L

    let clip ins clip =
        let start,stop = (Seq.head ins), (Seq.last ins)
        let cstart,cstop = (Seq.head clip), (Seq.last clip)
        if stop <= cstart || start >= cstop then
            Seq.empty
        else
            let x = max start cstart
            let y = min stop cstop
            seq { x .. 1L .. y  }

    let after after (list:'a list) =
        if after + 1 >= list.Length then
            []
        else
            let rest = List.skip (after + 1) list
            rest

    let rec count (ins: Instruction2) (rest: Instruction2 list) =
        let total = (Seq.length ins.X |> int64) * (Seq.length ins.Y |> int64) * (Seq.length ins.Z |> int64)
        let conflicts =
            rest
            |> List.map (fun r ->
                let xr2 = clip r.X ins.X
                let yr2 = clip r.Y ins.Y
                let zr2 = clip r.Z ins.Z

                if Seq.length xr2 = 0 || Seq.length yr2 = 0 || Seq.length zr2 = 0 then
                    None
                else
                    Some { r with X = xr2; Y = yr2; Z = zr2 })
            |> List.choose id
        let total = 
            conflicts
            |> List.indexed
            |> List.fold (fun total (i,r) ->
                let after = after i conflicts
                total - (count r after)) total
        total

    lines
    |> List.indexed
    |> List.fold (fun total (i,ins) ->
        if ins.IsOn then
            let after = after i lines
            total + count ins after
        else total) 0L

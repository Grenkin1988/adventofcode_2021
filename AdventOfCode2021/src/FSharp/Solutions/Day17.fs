[<NUnit.Framework.TestFixture>]
module Solutions.Day17

open System
open Solutions.Helpers
open NUnit.Framework

type Target = { X1: int; X2: int; Y1: int; Y2: int }

module Target =
    let create (line: string) =
        let split = line.Split(",", StringSplitOptions.TrimEntries)
        let x = split.[0].Substring(split.[0].LastIndexOf("x=") + 2)
        let y = split.[1].Substring(split.[1].LastIndexOf("y=") + 2)
        let splitInTwo (line: string) =
            let split = line.Split("..")
            let o,t = split.[0] |> int, split.[1] |> int
            if o > t then (t,o) else (o,t)
        let x1,x2 = splitInTwo x
        let y1,y2 = splitInTwo y
        { X1 = x1; X2 = x2; Y1 = y1; Y2 = y2 }

    let isOnTarget { X1 = x1; X2 = x2; Y1 = y1; Y2 = y2 } (x,y) =
        x >= x1 && x <= x2 && y >= y1 && y <= y2

    let isOverTheTarget { X2 = x2; Y1 = y1 } (x,y) =
        x > x2 || y < y1

    let getMax { X1 = x1; X2 = x2; Y1 = y1; Y2 = y2 } =
        max (abs x1) (abs x2), (max (abs y1) (abs y2))

module Probe =
    let move position speed = 
        let x,y = position
        let vx, vy = speed

        let x = x + vx
        let y = y + vy
        let changeVerticalSpeed vx =
            match vx with
            | 0 -> 0
            | vx when vx > 0 -> vx - 1
            | vx when vx < 0 -> vx + 1

        let vx = changeVerticalSpeed vx
        let vy = vy - 1

        (x,y), (vx,vy)

    let rec tryReachTarget target position speed initialSpeed maxHight =
        let isOnTarget = Target.isOnTarget target
        let isOverTheTarget = Target.isOverTheTarget target
        let maxHight = if snd position > maxHight then snd position else maxHight
        match position with
        | pos when isOnTarget pos -> Some (initialSpeed, maxHight)
        | pos when isOverTheTarget pos -> None
        | pos ->
            let pos, speed = move pos speed
            tryReachTarget target pos speed initialSpeed maxHight


[<TestCase("target area: x=20..30, y=-10..-5", ExpectedResult = 45)>]
[<TestCase("target area: x=143..177, y=-106..-71", ExpectedResult = 5565)>]
let ``Part 1``(input) =
    let target = Target.create input
    let position = (0,0)
    let maxSpeed = Target.getMax target

    let rec tryFindCoolest maxSpeed speed =
        let maxHight = snd maxSpeed * -1
        let increaseSpeed maxSpeed speed =
            let maxX, maxY = maxSpeed
            let x, y = speed
            if x >= maxX && y <= 0 then
                None
            elif x >= maxX then
                Some (1, y - 1)
            else
                Some (x + 1, y)
        let nextSpeed = increaseSpeed maxSpeed speed
        match Probe.tryReachTarget target position speed speed maxHight, nextSpeed with
        | Some (speed, max), _ when max > maxHight -> Some (speed,max)
        | _, None -> None
        | _, Some next ->
            tryFindCoolest maxSpeed next
            
    let coollest = tryFindCoolest maxSpeed (1, (snd maxSpeed))
    let (_, hight) = coollest.Value
    hight

[<TestCase("target area: x=20..30, y=-10..-5", ExpectedResult = 112)>]
[<TestCase("target area: x=143..177, y=-106..-71", ExpectedResult = 2118)>]
let ``Part 2``(input) =
    let target = Target.create input
    let position = (0,0)
    let maxSpeed = Target.getMax target

    let rec findAllpossible maxSpeed speed possible =
        let maxHight = snd maxSpeed * -1
        let increaseSpeed maxSpeed speed =
            let maxX, maxY = maxSpeed
            let x, y = speed
            if x >= maxX && y < (maxY * -1) then
                None
            elif x >= maxX then
                Some (1, y - 1)
            else
                Some (x + 1, y)
        let nextSpeed = increaseSpeed maxSpeed speed
        let x,y = speed
        match Probe.tryReachTarget target position speed speed maxHight, nextSpeed with        
        | _, None -> possible
        | Some (speed, _), Some next -> 
            findAllpossible maxSpeed next (speed :: possible)
        | _, Some next ->
            findAllpossible maxSpeed next possible
            
    let all = findAllpossible maxSpeed (1, (snd maxSpeed)) []
    let result = all |> List.distinct

    result.Length

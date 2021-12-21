[<NUnit.Framework.TestFixture>]
module Solutions.Day21

open System
open Solutions.Helpers
open NUnit.Framework

type IDice =
    abstract Roll: unit -> int64
    abstract Rolled: int64

type DeterministicDice() =
    let mutable rolled = 0L;
    let mutable previousNumber = 0L;
   
    let nextRoll() =
        let next = previousNumber + 1L
        if next > 100L then 1L
        else next

    interface IDice with
        member __.Roll() =
            previousNumber <- nextRoll()
            rolled <- rolled + 1L
            previousNumber

        member __.Rolled = rolled

type Player = int64 * int64

module Game =
    let move from moves = 
        let result = from + moves
        if result % 10L = 0L then 10L
        elif result % 10L > 0L then result % 10L
        else result

    let playerTurn (dice: IDice) (place, score) =
        let movement = dice.Roll() + dice.Roll() + dice.Roll()
        let place = move place movement
        let score = score + place
        place, score

    let rec play (dice: IDice) (current: Player) (next: Player) =
        let (place, score) = playerTurn dice current
        if score >= 1000 then
            next
        else
            play dice next (place, score)

[<TestCase(4, 8, ExpectedResult = 739785)>]
[<TestCase(7, 1, ExpectedResult = 684495)>]
let ``Part 1``(start1, start2) =
    let dice = DeterministicDice() :> IDice
    let player1 = (start1, 0L)
    let player2 = (start2, 0L)

    let (place, score) = Game.play dice player1 player2
    score * dice.Rolled

[<TestCase(4, 8, ExpectedResult = 444356092776315L)>]
[<TestCase(7, 1, ExpectedResult = 152587196649184L)>]
let ``Part 2``(p1, p2) =
    let allRolls () = seq {
        for d1 in 1L..3L do
            for d2 in 1L..3L do
                for d3 in 1L..3L do
                    yield d1,d2,d3
    }

    let results = Collections.Generic.Dictionary<(int64*int64*int64*int64), int64*int64>()

    let rec countWins p1 p2 s1 s2 =
        if s1 >= 21L then
            (1L,0L)
        elif s2 >= 21L then
            (0L,1L)
        elif results.ContainsKey (p1, p2, s1, s2) then
            results.[(p1, p2, s1, s2)]
        else
            let rolls = allRolls()
            let ans =
                rolls
                |> Seq.map (fun (d1,d2,d3) ->
                    let newp1 = Game.move p1 (d1+d2+d3)
                    let news1 = s1 + newp1
                    countWins p2 newp1 s2 news1)
                |> Seq.fold (fun (r1,r2) (a1,a2) -> r1+a2,r2+a1) (0L, 0L)
            results.Add((p1,p2,s1,s2), ans)
            ans

    let count = countWins p1 p2 0L 0L

    max (fst count) (snd count)

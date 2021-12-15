[<NUnit.Framework.TestFixture>]
module Solutions.Day15

open System
open Solutions.Helpers
open NUnit.Framework
open System.Collections.Generic

type Node = 
    { X: int; Y: int; Cost: int }
    override this.ToString() = sprintf "X:%i Y:%i C:%i" this.X this.Y this.Cost

type Graph =
    {
        Neighbors: Map<Node, Node list>
    }

type PriorityQueue<'a>() =
    let dictionary = Dictionary<int, 'a list>()

    member __.Enqueue value priority =
        match dictionary.TryGetValue priority with
        | false, _ -> dictionary.Add(priority, [ value ])
        | true, list ->
            dictionary.[priority] <- value :: list

    member __.TryDequeue () =
        if dictionary.Count = 0 then
            None
        else
            let min = dictionary.Keys |> Seq.min
            let values = dictionary.[min]
            dictionary.Remove(min) |> ignore
            match values with
            | v :: [] ->
                Some v
            | v :: next ->
                dictionary.[min] <- next
                Some v
            | [] -> None

module Graph =
    let create (matrix: Node[,]) =
        let lengthX= matrix.GetLength 0
        let lengthY= matrix.GetLength 1
        let neighbors (node) =
            [ (1, 0); (0, 1); (-1, 0); (0, -1)]
            |> List.map (fun (x,y) -> node.X + x, node.Y + y)
            |> List.filter (fun (x,y) -> x >= 0 && x < lengthX && y >= 0 && y < lengthY)
            |> List.map (fun (x,y) -> matrix.[x,y])
        let neighbors =
            matrix
            |> Matrix.flatMap (fun n -> n, neighbors n)
            |> Map.ofSeq
        { Neighbors = neighbors }

module PathFinding =
    let uniformCostSearch (graph: Graph) (start: Node) (goal: Node) =
        let frontier = PriorityQueue<_,_>()
        frontier.Enqueue(start, start.Cost)
        let cameFrom = [(start, None)] |> Map.ofList
        let costSoFar = [(start, start.Cost)] |> Map.ofList

        let rec calculate (costSoFar: Map<Node,int>) (cameFrom: Map<Node, Node option>) =
            match frontier.TryDequeue() with
            | false, _, _ -> costSoFar, cameFrom
            | true, current, _ when current = goal -> costSoFar, cameFrom
            | true, current, _ ->
                let folder (costSoFar: Map<Node,int>, cameFrom: Map<Node, Node option>) (next: Node) =
                    let newCost = costSoFar.[current] + next.Cost
                    match costSoFar.TryGetValue next with
                    | true, cost when newCost < cost ->
                        let costSoFar = Map.add next newCost costSoFar
                        frontier.Enqueue(next, newCost)
                        let cameFrom = Map.add next (Some current) cameFrom
                        costSoFar, cameFrom
                    | false, _ ->
                        let costSoFar = Map.add next newCost costSoFar
                        frontier.Enqueue(next, newCost)
                        let cameFrom = Map.add next (Some current) cameFrom
                        costSoFar, cameFrom
                    | _ -> costSoFar, cameFrom

                let costSoFar, cameFrom =
                    let neighbors = graph.Neighbors.[current]
                    neighbors |> List.fold folder (costSoFar, cameFrom)
                calculate costSoFar cameFrom
        let _, cameFrom = calculate costSoFar cameFrom
        cameFrom

    let findPath (cameFrom: Map<Node, Node option>) (start: Node) (goal: Node) =
        let rec findPath current path =
            match Map.tryFind current cameFrom with
            | None -> path
            | Some None -> path
            | Some (Some current) when current = start -> path
            | Some (Some current) ->
                current :: path |> findPath current
        start :: (findPath goal [goal])

    let heuristic a b =
        // Manhattan distance on a square grid
        abs(a.X - b.X) + abs(a.Y - b.Y)

    let aStart (graph: Graph) (start: Node) (goal: Node) =
        let frontier = PriorityQueue<_>()
        frontier.Enqueue start start.Cost
        let cameFrom = [(start, None)] |> Map.ofList
        let costSoFar = [(start, start.Cost)] |> Map.ofList

        let rec calculate (costSoFar: Map<Node,int>) (cameFrom: Map<Node, Node option>) =
            match frontier.TryDequeue() with
            | None  -> costSoFar, cameFrom
            | Some current when current = goal -> costSoFar, cameFrom
            | Some current ->
                let folder (costSoFar: Map<Node,int>, cameFrom: Map<Node, Node option>) (next: Node) =
                    let newCost = costSoFar.[current] + next.Cost
                    let priority = newCost + (heuristic goal next)
                    match costSoFar.TryGetValue next with
                    | true, cost when newCost < cost ->
                        let costSoFar = Map.add next newCost costSoFar
                        frontier.Enqueue next priority
                        let cameFrom = Map.add next (Some current) cameFrom
                        costSoFar, cameFrom
                    | false, _ ->
                        let costSoFar = Map.add next newCost costSoFar
                        frontier.Enqueue next priority
                        let cameFrom = Map.add next (Some current) cameFrom
                        costSoFar, cameFrom
                    | _ -> costSoFar, cameFrom

                let costSoFar, cameFrom =
                    let neighbors = graph.Neighbors.[current]
                    neighbors |> List.fold folder (costSoFar, cameFrom)
                calculate costSoFar cameFrom
        let _, cameFrom = calculate costSoFar cameFrom
        cameFrom

[<TestCase("day15_testinput.txt", ExpectedResult = 40)>]
[<TestCase("day15_input.txt", ExpectedResult = 619)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    
    let matrix = 
        matrix lines
        |> Array2D.mapi (fun x y v -> { X = x; Y = y; Cost = v })

    let goalX= matrix.GetLength 0 - 1
    let goalY= matrix.GetLength 1 - 1
    let start,goal = matrix.[0,0], matrix.[goalX,goalY]

    let graph = Graph.create matrix

    let cameFrom = PathFinding.uniformCostSearch graph start goal

    let path = PathFinding.findPath cameFrom start goal

    (path |> List.sumBy (fun n -> n.Cost)) - start.Cost

[<TestCase("day15_testinput.txt", ExpectedResult = 315)>]
[<TestCase("day15_input.txt", ExpectedResult = 2922)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    
    let matrix = matrix lines

    let lengthX= matrix.GetLength 0
    let lengthY= matrix.GetLength 1
    let bigX= lengthX * 5
    let bigY= lengthY * 5

    let bigMatrix = Array2D.create bigX bigY 0

    for i in 0..4 do
        for j in 0..4 do
            let value v = 
                let v = v + i + j
                if v > 9 then v - 9 else v
            let ii = lengthX * i
            let jj = lengthY * j
            matrix |> Array2D.iteri (fun x y v -> bigMatrix[x+ii,y+jj] <- value v)

    let bigMatrix = bigMatrix |> Array2D.mapi (fun x y v -> { X = x; Y = y; Cost = v })

    let goalX= bigMatrix.GetLength 0 - 1
    let goalY= bigMatrix.GetLength 1 - 1
    let start,goal = bigMatrix.[0,0], bigMatrix.[goalX,goalY]

    let graph = Graph.create bigMatrix

    let cameFrom = PathFinding.aStart graph start goal

    let path = PathFinding.findPath cameFrom start goal

    (path |> List.sumBy (fun n -> n.Cost)) - start.Cost

[<NUnit.Framework.TestFixture>]
module Solutions.Day12

open Solutions.Helpers
open NUnit.Framework

type NodeType =
    | Start
    | SmallCave of string
    | BigCave of string
    | End
    override this.ToString() =
        match this with
        | Start -> "start"
        | End -> "end"
        | SmallCave id
        | BigCave id -> id

module NodeType = 
    let create (id: string) =
        match id with
        | "start" -> Start
        | "end" -> End
        | id when id.ToUpper() = id -> BigCave id
        | id -> SmallCave id

type Node =
    {
        NodeType : NodeType
        Linked : NodeType list
    }
    override this.ToString() = this.NodeType.ToString()

module CaveMap =
    let addLinked n1 n2 map =
        let addLinked n1 n2 map =
            let newNode, map =
                match Map.tryFind n1 map with
                | Some node -> 
                    { node with Linked = n2 :: node.Linked }, Map.remove n1 map
                | None ->
                    { NodeType = n1; Linked = [ n2 ]}, map
            Map.add newNode.NodeType newNode map

        let n1 = NodeType.create n1
        let n2 = NodeType.create n2
        let map =
            addLinked n1 n2 map
            |> addLinked n2 n1
            |> Map.change End (function Some e -> { e with Linked = [] } |> Some | None -> None)
        map

    let findRoutes getNext map =
        let map = map
        let start, finish =
            match Map.tryFind Start map, Map.tryFind End map with
            | Some start, Some finish -> start, finish
            | _ -> failwith "NO START/FINISH"

        let rec findRoutes (routes: Node list list) current =
            let current = map |> Map.find current
            let routes = routes |> List.map (fun route -> route @ [current])
            let next = getNext routes current
            match next with
            | [] -> routes
            | next ->
                next
                |> List.collect (fun n -> findRoutes routes n)

        findRoutes [[]] start.NodeType

    let validRoute route =
        let last = List.last route
        route.Head.NodeType = Start && last.NodeType = End

[<TestCase("day12_testinput_small.txt", ExpectedResult = 10)>]
[<TestCase("day12_testinput_medium.txt", ExpectedResult = 19)>]
[<TestCase("day12_testinput_big.txt", ExpectedResult = 226)>]
[<TestCase("day12_input.txt", ExpectedResult = 5252)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName

    let links = lines |> List.map (fun line -> line.Split("-"))

    let map =
        links
        |> List.fold (fun map link -> CaveMap.addLinked link.[0] link.[1] map) Map.empty

    let getNext routes current =
        match current.NodeType with
        | End -> [ ]
        | Start
        | BigCave _
        | SmallCave _ ->
            let routes = routes |> List.concat
            current.Linked 
            |> List.choose (fun n -> 
                match n with
                | SmallCave _ when routes |> List.exists (fun node -> node.NodeType = n) -> None
                | Start -> None
                | _ -> Some n)

    let routes = 
        CaveMap.findRoutes getNext map
        |> List.filter CaveMap.validRoute

    routes
    |> List.map (fun route -> route |> List.fold(fun s n -> sprintf "%s, %O" s n) "")
    |> List.iter (printfn "%s")

    routes.Length

[<TestCase("day12_testinput_small.txt", ExpectedResult = 36)>]
[<TestCase("day12_testinput_medium.txt", ExpectedResult = 103)>]
[<TestCase("day12_testinput_big.txt", ExpectedResult = 3509)>]
[<TestCase("day12_input.txt", ExpectedResult = 147784)>]
let ``Part 2``(fileName) =
    let lines = readInput fileName
    
    let getNext routes current =
        match current.NodeType with
        | End -> [ ]
        | Start
        | BigCave _
        | SmallCave _ ->
            let routes = routes |> List.concat |> List.groupBy (fun n -> n.NodeType) |> List.map (fun (k,v) -> k,v|>List.length) |> Map.ofList
            let alreadyVisitedTwice = routes |> Map.exists (fun k v -> match k with SmallCave _ -> v = 2 | _ -> false)
            current.Linked 
            |> List.choose (fun n -> 
                match n with
                | SmallCave _ ->
                    match Map.tryFind n routes with
                    | None -> Some n
                    | Some _ when alreadyVisitedTwice -> None
                    | Some _ -> Some n
                | Start -> None
                | _ -> Some n)

    let links = lines |> List.map (fun line -> line.Split("-"))

    let map =
        links
        |> List.fold (fun map link -> CaveMap.addLinked link.[0] link.[1] map) Map.empty

    let routes = 
        CaveMap.findRoutes getNext map
        |> List.filter CaveMap.validRoute

    routes
    |> List.map (fun route -> route |> List.fold(fun s n -> sprintf "%s, %O" s n) "")
    |> List.iter (printfn "%s")

    routes.Length

open System.Collections.Generic
open AdventOfCodeUtils.AdventOfCode

type Node(subFolders: Dictionary<string, Node>, ?parent: Node) = 
    member val size = 0 with get, set
    member this.subFolders = subFolders
    member this.parent = parent

let (|CD|LS|DIR|FILE|) (l: string) = 
    if l.StartsWith "$ cd " then CD l[5..] else 
    if l.StartsWith "$ ls" then LS else
    if l.StartsWith "dir" then DIR l[4..] else
    FILE (l.Split(" ")[0])

let rec parse (node:Node) (lines:string seq) = 
    let parseLine (node:Node) line = 
        match line with 
        | CD "/" -> node // we suppose this only happens once and the currentNode is already initialized to root node
        | CD ".." -> match node.parent with 
                                | Some value -> value
                                | None -> failwith "No parents found." 
        | CD dir -> node.subFolders[dir]
        | LS -> node 
        | DIR dir -> 
            node.subFolders.Add(dir, new Node(new Dictionary<string, Node>(), node))
            node
        | FILE size -> 
            node.size <- node.size + int size
            node
    lines |> Seq.fold parseLine node

let rec computeFullSizes (node:Node) = 
    node.size <- 
        node.subFolders 
        |> Seq.sumBy (fun v -> computeFullSizes v.Value)
        |> (+) node.size
    node.size

let rec computePart1 (node:Node) = 
    node.subFolders
        |> Seq.sumBy (fun v -> computePart1 v.Value)
        |> (+) (if node.size <= 100000 then node.size else 0)

let rec computePart2Sub (node:Node) spaceToFree = 
    if (node.subFolders.Count = 0)
    then (if node.size >= spaceToFree then node.size else Core.int.MaxValue)
    else 
        node.subFolders
            |> Seq.map (fun v -> computePart2Sub v.Value spaceToFree)
            |> Seq.min
            |> min (if node.size >= spaceToFree then node.size else Core.int.MaxValue)

let computePart2 (node:Node) = 
    computePart2Sub node (30000000 - (70000000 - (node.size)))

let part compute lines = 
    let rootNode = new Node(new Dictionary<string, Node>())
    parse rootNode lines |> ignore
    computeFullSizes rootNode |> ignore
    compute rootNode

run (part computePart1)
run (part computePart2)
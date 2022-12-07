open System.Collections.Generic
open AdventOfCodeUtils.AdventOfCode

type Node(subFolders: Dictionary<string, Node>, ?parent: Node) = 
    member val size = 0 with get, set
    member this.subFolders = subFolders
    member this.parent = parent

let rec parse (node:Node) (lines:string seq) = 
    let rec parseLs (node:Node) folderLines = 
        match (folderLines:string list) with 
        | cd::tail when cd.StartsWith("$") -> cd::tail
        | cd::tail when cd.StartsWith("dir") -> 
            node.subFolders.Add(cd[4..], new Node(new Dictionary<string, Node>(), node))
            parseLs node tail
        | cd::tail -> 
            node.size <- node.size + int(cd.Split(" ")[0])
            parseLs node tail
        | [] -> []
    match List.ofSeq lines with 
    | "$ cd /"::tail -> parse node tail // we suppose this only happens once and the currentNode is already initialized
    | "$ cd .."::tail -> match node.parent with 
                            | Some value -> parse value tail
                            | None -> failwith "No parents found." 
    | cd::tail when cd.StartsWith("$ cd ") -> 
        parse node.subFolders[cd[5..]] tail
    | "$ ls"::tail -> 
        parse node (parseLs node tail)
    | [] -> ()
    | _ -> failwith "Unrecognized line"

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
    parse rootNode lines
    computeFullSizes rootNode |> ignore
    compute rootNode

run (part computePart1)
run (part computePart2)
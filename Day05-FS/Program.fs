
open System.Text.RegularExpressions
open AdventOfCodeUtils.AdventOfCode
open System.Collections.Generic

let mutable towers = new Dictionary<int, List<char>>()

let parseRow (line:string) = 
    line
    |> Seq.chunkBySize(4)
    |> Seq.map (fun s -> s[1])
    |> Seq.iteri (fun i elem -> if elem <> ' ' then towers[i+1].Insert(0, elem) else ())

let (|ParseMovement|_|) input =
    let m = Regex.Match(input, @"move (\d+) from (\d) to (\d)")
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ] |> List.map int)
    else None

let rec doMovementPart1 n x y = 
    match n with 
    | 0 -> ()
    | _ -> 
        let lastIndex = towers[x].Count - 1;
        let elem = towers[x][lastIndex];
        towers[x].RemoveAt(lastIndex);
        towers[y].Add(elem);
        doMovementPart1 (n-1) x y

let doMovementPart2 n x y = 
    match n with 
    | 0 -> ()
    | _ -> 
        let lastIndex = towers[x].Count - 1;
        let elem = towers[x].GetRange(lastIndex - n + 1, n);
        towers[x].RemoveRange(lastIndex - n + 1, n);
        towers[y].AddRange(elem);
        
let print () = 
    seq { 1..9 } 
    |> Seq.map (fun i -> if towers[i].Count > 0 then (towers[i][towers[i].Count - 1]).ToString() else "")
    |> String.concat ""

let parseData doMovement line = 
    match line with 
    | ParseMovement [n; x; y] -> doMovement n x y
    | s -> parseRow s

let part doMovement lines = 
    towers <- new Dictionary<int, List<char>>() 
    seq { 1..9 } |> Seq.iter (fun i -> towers[i] <- new List<char>())
    lines 
    |> Seq.iter (parseData doMovement)
    print ()

run (part doMovementPart1)
run (part doMovementPart2)


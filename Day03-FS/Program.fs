open System.Linq
open System

type ENV = T | P

let env = P
let inputFile = env |> function
    | T -> "test.txt"
    | P -> "input.txt"

let lines = System.IO.File.ReadLines inputFile

let part1 = 
    lines
    |> Seq.map (fun s -> 
        let len = s.Length
        let p = Set.intersect (Set.ofArray <| s.ToCharArray(0, len/2)) (Set.ofArray <| s.ToCharArray(len/2, len/2))
        p.Single())
    |> Seq.sumBy (fun s -> if Char.IsLower(s) then int s - int 'a' + 1 else int s - int 'A' + 27)

printfn "Part 1 : %A" (part1)

let part2 = 
    lines
    |> Seq.chunkBySize 3
    |> Seq.map (fun s -> 
        s
        |> Seq.map Set.ofSeq
        |> Set.intersectMany)
    |> Seq.map (fun s -> s.Single())
    |> Seq.sumBy (fun s -> if Char.IsLower(s) then int s - int 'a' + 1 else int s - int 'A' + 27)

printfn "Part 2 : %A" (part2)

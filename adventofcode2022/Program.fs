open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type ENV = T | P

let env = P
let inputFile = env |> function
    | T -> "test.txt"
    | P -> "input.txt"

let file = System.IO.File.ReadAllText inputFile

let part1 = 
    file.Split "\r\n\r\n"
    |> Seq.map (fun x -> x.Split("\r\n"))
    |> Seq.map (fun x -> Seq.sumBy int x)
    |> Seq.max

let part2 = 
    file.Split "\r\n\r\n"
    |> Seq.map (fun x -> x.Split("\r\n"))
    |> Seq.map (fun x -> Seq.sumBy int x)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

printfn "Part 1 : %d" (part1)
printfn "Part 1 : %d" (part2)
namespace AdventOfCodeUtils
open System.IO

module AdventOfCode =
    type ENV = T | P

    let inputFile env = 
        match env with
        | T -> "test.txt"
        | P -> "input.txt"

    let lines env = File.ReadLines (inputFile env)

    let run part = 
        printfn "Test data : %A" (part (lines T))
        printfn "Input data : %A" (part (lines P))
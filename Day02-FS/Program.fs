type ENV = T | P

let env = P
let inputFile = env |> function
    | T -> "test.txt"
    | P -> "input.txt"

let lines = System.IO.File.ReadLines inputFile

let getPoints v = 
    match v with 
        | "X" -> 1
        | "Y" -> 2
        | "Z" -> 3
        | _ -> 0


let part1 = 
    lines
    |> Seq.map (fun x -> x.Split(" "))
    |> Seq.fold (fun n values -> n + getPoints values[1] + 
        match values with
            | [|"A"; "Z"|] |[|"B"; "X"|] |[|"C"; "Y"|] -> 0
            | [|"A"; "X"|] |[|"B"; "Y"|] |[|"C"; "Z"|] -> 3
            | [|"A"; "Y"|] |[|"B"; "Z"|] |[|"C"; "X"|] -> 6
            | _ -> 0
        ) 0

printfn "Part 1 : %d" (part1)

let getPointsPart2 v = 
    match v with 
    | "X" -> 0
    | "Y" -> 3
    | "Z" -> 6
    | _ -> 999

let part2 = 
    lines
    |> Seq.map (fun x -> x.Split(" "))
    |> Seq.fold (fun n values -> n + getPointsPart2 values[1] + 
        match values with
            | [|"A"; "Y"|] |[|"B"; "X"|] |[|"C"; "Z"|] -> 1
            | [|"A"; "Z"|] |[|"B"; "Y"|] |[|"C"; "X"|] -> 2
            | [|"A"; "X"|] |[|"B"; "Z"|] |[|"C"; "Y"|] -> 3
            | _ -> 0
        ) 0

printfn "Part 2 : %d" (part2)

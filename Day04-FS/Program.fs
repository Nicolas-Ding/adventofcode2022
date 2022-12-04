
open System.Text.RegularExpressions
open AdventOfCodeUtils.AdventOfCode

let (|ParseData|_|) input =
    let m = Regex.Match(input, @"(\d+)-(\d+),(\d+)-(\d+)")
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ] |> List.map int)
    else None

let compute filter line = 
    match line with 
    | ParseData [a; b; x; y] when filter a b x y || filter x y a b -> 1
    | _ -> 0

let isIncluded a b x y = (a >= x && b <= y)

let hasOverlap a b x y = (a >= x && a <= y) || (b >= x && b <= y)

let part isValid lines = 
    lines
    |> Seq.sumBy (compute isValid)

run (part isIncluded)
run (part hasOverlap)


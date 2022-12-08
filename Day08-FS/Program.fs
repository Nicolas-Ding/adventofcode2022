open AdventOfCodeUtils.AdventOfCode
open System

let parseData (lines: string seq) = 
    let array = Array2D.zeroCreate (Seq.length lines) (Seq.head lines |> String.length)
    lines |> Seq.iteri (fun i line -> array[i, *] <- (Seq.map (fun c -> int c - int '0') line|> Array.ofSeq))
    array

// PART 1 
let isVisibleLine array =  // returns if the last item of the array is visible from the front of the array
    if Array.length array = 1 then true else Array.last array > Array.max array[0..(Array.length array) - 2]

let isVisible i j (array2D:int[,]) = 
    isVisibleLine array2D[i, ..j] || 
    isVisibleLine array2D[..i, j] ||
    isVisibleLine (array2D[i, j..] |> Array.rev) ||
    isVisibleLine (array2D[i.., j] |> Array.rev)

let countVisible (array:int[,]) = 
    array 
    |> Array2D.mapi (fun i j elem -> isVisible i j array)
    |> Seq.cast<bool>
    |> Seq.filter ((=) true)
    |> Seq.length

// PART 2
let getVisibility height array = 
    array 
    |> Seq.ofArray 
    |> Seq.tail
    |> Seq.tryFindIndex (fun i -> i >= height)
    |> Option.fold (fun _ i -> i + 1) (array.Length - 1)

assert (getVisibility 5 [|5; 2; 3; 2; 9|] = 4)

let getVisionScore i j (array2D:int[,]) = 
    getVisibility array2D[i,j] array2D[i, j..] *
    getVisibility array2D[i,j] array2D[i.., j] *
    getVisibility array2D[i,j] (array2D[i, ..j] |> Array.rev) *
    getVisibility array2D[i,j] (array2D[..i, j] |> Array.rev)

let getMaxScore (array: int[,]) = 
    array 
    |> Array2D.mapi (fun i j _ -> getVisionScore i j array)
    |> Seq.cast<int>
    |> Seq.max

run (parseData >> countVisible)
run (parseData >> getMaxScore)
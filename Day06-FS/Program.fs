open AdventOfCodeUtils.AdventOfCode

let solve n = 
    Seq.head
    >> Seq.windowed n 
    >> Seq.findIndex (Seq.distinct >> Seq.length >> (=) n)
    >> (+) n

let part1 = solve 4
let part2 = solve 14

run part1
run part2

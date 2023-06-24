open System.IO

let input reader num = reader <| $"C:/Users/novik/IdeaProjects/aoc-fs/input/day{num}.txt"
let inputLines = input File.ReadAllLines 
let inputText = input File.ReadAllText

let lines (l: string) = l.Split null

module day1 = 
    let fuelByMassSimple mass = mass / 3 - 2
    let rec fuelByMassFull mass = 
        let req = mass / 3 - 2
        if req <= 0 then 0 else req + fuelByMassFull req

    let public part1 input = input |> Array.map int |> Array.map fuelByMassSimple |> Array.sum
    let public part2 input = input |> Array.map int |> Array.map fuelByMassFull |> Array.sum

module day2 = 
    let gravityAssist (program: int array) =
        let mutable memory = program
        let rec runProgram pc =
            match memory.[pc] with
            | 1 ->
                let a = memory.[memory.[pc + 1]]
                let b = memory.[memory.[pc + 2]]
                let c = memory.[pc + 3]
                memory.[c] <- a + b
                runProgram (pc + 4)
            | 2 ->
                let a = memory.[memory.[pc + 1]]
                let b = memory.[memory.[pc + 2]]
                let c = memory.[pc + 3]
                memory.[c] <- a * b
                runProgram (pc + 4)
            | 99 -> ()
            | _ -> failwith "Unknown opcode"
        memory.[1] <- 12
        memory.[2] <- 2
        runProgram 0
        memory.[0]

    let runProgram (program: int array) noun verb =
        let mutable memory = program
        let rec run pc =
            match memory.[pc] with
            | 1 ->
                let a = memory.[memory.[pc + 1]]
                let b = memory.[memory.[pc + 2]]
                let c = memory.[pc + 3]
                memory.[c] <- a + b
                run (pc + 4)
            | 2 ->
                let a = memory.[memory.[pc + 1]]
                let b = memory.[memory.[pc + 2]]
                let c = memory.[pc + 3]
                memory.[c] <- a * b
                run (pc + 4)
            | 99 -> ()
            | _ -> failwith "Unknown opcode"
        memory.[1] <- noun
        memory.[2] <- verb
        run 0
        memory.[0]

    let findInputs targetOutput program =
        [for noun in 0..99 do
            for verb in 0..99 do
                if runProgram program noun verb = targetOutput then
                    yield (noun, verb)]

    let public part1 (input: string) = input.Split ',' |> Array.map int |> gravityAssist
    let public part2 (input: string) = 
        let (noun, verb) = input.Split ',' |> Array.map int |> findInputs 19690720 |> List.head
        100 * noun + verb




[<EntryPoint>]
let main _ = 
    let s = [1 .. 10] |> Seq.chunkBySize 4 |> Seq.map List.ofArray |> List.ofSeq
    printfn $"{s}"
    printfn "Day 1, Part 1: %d" (day1.part1 <| inputLines 1)
    printfn "Day 1, Part 2: %d" (day1.part2 <| inputLines 1)
    printfn "Day 2, Part 1: %d" (day2.part1 <| inputText 2)
    printfn "Day 2, Part 2: %d" (day2.part2 <| inputText 2)
    0
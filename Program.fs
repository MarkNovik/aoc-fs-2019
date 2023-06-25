open System.IO

let input reader num = reader <| $"./input/day{num}.txt"
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
    let gravityAssist noun verb (program: int array) =
            let mutable memory = Array.copy program
            let rec runProgram pc =
                match memory.[pc] with
                | 1 ->
                    memory.[memory.[pc + 3]] <- memory.[memory.[pc + 1]] + memory.[memory.[pc + 2]]
                    runProgram (pc + 4)
                | 2 ->
                    memory.[memory.[pc + 3]] <- memory.[memory.[pc + 1]] * memory.[memory.[pc + 2]]
                    runProgram (pc + 4)
                | 99 -> ()
                | _ -> failwith $"Unknown opcode {memory.[pc]}"
            memory.[1] <- noun
            memory.[2] <- verb
            runProgram 0
            memory.[0]

    let findPair forOutput (program : int array) = 
        [for noun in 0..99 do
            for verb in 0..99 do
                if gravityAssist noun verb program = forOutput then
                    yield (noun, verb)] |> Seq.head

    let computePassword (noun, verb) = 100 * noun + verb
    
    let public part1 (input: string) = input.Split ',' |> Array.map int |> gravityAssist 12 2
    let public part2 (input: string) = input.Split ',' |> Array.map int |> findPair 19690720 |> computePassword




[<EntryPoint>]
let main _ = 
    printfn "Day 1, Part 1: %d" (day1.part1 <| inputLines 1)
    printfn "Day 1, Part 2: %d" (day1.part2 <| inputLines 1)
    printfn "Day 2, Part 1: %d" (day2.part1 <| inputText 2)
    printfn "Day 2, Part 2: %d" (day2.part2 <| inputText 2)
    0
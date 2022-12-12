module Program = 

    let [<EntryPoint>] main _ = 

        //let answer = Day1.Part1.solution "Day1.txt"

        let answer = Day1.Part2.solution "Day1.txt"

        //let answer = Day1.Part2.sortIntoTop3 3 [5;4;2]
        printfn "answer: %A" answer

        0

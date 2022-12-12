namespace Day2

module Part1 = 

    open System
    open System.IO

    type RPS =
        | Rock
        | Paper
        | Scissors
    with 
        static member fromString s = 
            match s with
            | "A" | "X" -> RPS.Rock
            | "B" | "Y" -> RPS.Paper
            | _ -> RPS.Scissors
        static member value rps =
            match rps with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

    type Score =
        | Loose = 0
        | Draw = 3
        | Win = 6


    let evaluateRound you opponent =
        let choiceScore = RPS.value you

        let wldScore =
            match you, opponent with
            | (Rock,Paper) | (Paper,Scissors) | (Scissors, Rock) -> Score.Loose
            | (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) -> Score.Win
            | _ -> Score.Draw
        choiceScore + (int)wldScore

    let solution inputFile =

        File.ReadAllLines inputFile
        |> Array.map(fun line -> 
            let xs = line.Split(' ')
            evaluateRound(RPS.fromString xs[1]) (RPS.fromString xs[0])
        )
        |> Array.sum
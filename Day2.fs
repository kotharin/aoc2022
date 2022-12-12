namespace Day2

module Shared = 

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
        static member forWin rps =
            match rps with
            | Rock -> Paper
            | Paper -> Scissors
            | Scissors -> Rock
        static member forLoose rps =
            match rps with
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper
        static member forDraw (rps:RPS) =
            rps            

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


module Part1 =

    open Shared
    open System
    open System.IO

    let solution inputFile =

        File.ReadAllLines inputFile
        |> Array.map(fun line -> 
            let xs = line.Split(' ')
            evaluateRound(RPS.fromString xs[1]) (RPS.fromString xs[0])
        )
        |> Array.sum

module Part2 =

    open Shared
    open System.IO

    let evaluateMyChoice opponent you =
        match you with
        // Loose
        |Rock -> RPS.forLoose opponent
        // Draw
        | Paper -> RPS.forDraw opponent
        // win
        | Scissors -> RPS.forWin opponent

    let solution inputFile =

        File.ReadAllLines inputFile
        |> Array.map(fun line -> 
            let xs = line.Split(' ')
            let opponent, you = (RPS.fromString xs[0]), (RPS.fromString xs[1])
            let newMyChoice = evaluateMyChoice opponent you
            evaluateRound newMyChoice opponent
        )
        |> Array.sum

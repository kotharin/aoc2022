namespace Day1

open System
open System.IO

module Part1 =

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let _,maxVal =
            lines
            |> Array.fold (fun state line ->
                let curr,max = state
                
                if (line.Trim().Length = 0) then
                    // check if the current is greater than max
                    let newMax = 
                        if (curr > max) then curr else max
                    (0,newMax)
                else
                    // add the current line value to running total
                    let cal = int(line.Trim())
                    (curr + cal, max)
            ) (0,0)
        maxVal
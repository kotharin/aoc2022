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
module Part2 =


    let sortIntoTop3 num top3 =

        let rec sort number top (newTop:List<int>) =
            if (List.length newTop < 3) then
                match top with
                | [] ->
                    List.append newTop [number]
                | head::tail ->
                    if (number > head) then
                        List.append newTop (number::[head])
                    else
                        // add head to the newTop
                        let nt = List.append newTop [head]
                        sort number tail nt
            else newTop

        sort num top3 []

    let solution inputFile = 
        let lines = File.ReadAllLines inputFile

        let lastVal,maxVals =
            lines
            |> Array.fold (fun state line ->
            
                let curr, maxList = state
                if (line.Trim().Length = 0) then
                    // check if the current is greater than max
                    let newMax = 
                        sortIntoTop3 curr maxList
                    (0,newMax)
                else
                    // add the current line value to running total
                    let cal = int(line.Trim())
                    (curr + cal, maxList)
            ) (0,[0;0;0])
        sortIntoTop3 lastVal maxVals
        |> List.take 3
        |> List.sum
namespace Day3


module Shared =


    // Shared constants
    let av = (int)'a'
    let zv = (int)'z'
    let AV = (int)'A'

    // load all unique characters of a string
    // into a map
    let loadIntoMap (s:string) =
        s.ToCharArray()
        |> Array.map(fun c -> c,1)
        |> Map.ofArray

module Part1 = 

    open System.IO
    open Shared

    // Split a string into 2 halves
    let splitString (s:string) = 
        let midPoint = s.Length/2
    
        let firstHalf = s.Substring(0,midPoint)
        let secondHalf = s.Substring(midPoint)

        firstHalf, secondHalf

    // Load the string into a map and
    // keep track of duplicates based on
    // the map supplied.
    let loadIntoMapAndCheckDuplicates map (s:string) =
        s.ToCharArray()
        |> Array.fold (fun state c ->
            let mapOfChars, dups = state
            // add to the new map
            let newMap = Map.add c 1 mapOfChars
            // check if the char exists, if so add to dups
            let newDups =
                if (Map.containsKey c map) then
                    Map.add c 1 dups
                else dups
            (newMap,newDups)
        ) (Map.empty,Map.empty)

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        
        let dupValue = 
            lines
            |> Array.map splitString
            |> Array.map(fun ss ->
                let first,second = ss
                let firstHalf = loadIntoMap first
                let duplicates = snd (loadIntoMapAndCheckDuplicates firstHalf second)
                duplicates.Keys
            )
            |> Seq.concat
            |> Seq.sumBy (fun c ->
                let cv = (int)c

                if ((av <= cv) && (cv <= zv)) then
                    cv - av + 1
                else
                    cv - AV + 27
            )
        dupValue

module Part2 =

    open System.IO
    open Shared

    let getCommonElementInGroups (groups:string array) =
        // get the first group and use to 
        // loop over the items
        let grp1 = groups.[0]

        // add the other groups into maps
        let grpMap2 = loadIntoMap groups.[1]
        let grpMap3 = loadIntoMap groups.[2]

        // iterate over the first list
        let common =
            grp1.ToCharArray()
            |> Array.fold (fun state c ->
                if ((Map.containsKey c grpMap2) && (Map.containsKey c grpMap3)) then
                    // add to common
                    Map.add c 1 state
                else
                    state
            ) (Map.empty)

        Map.keys common

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        // break up the data into groups of 3 lines each
        let chunks = Array.chunkBySize 3 lines

        // Iterate over the chhunks
        chunks
        |> Array.map (fun chunk ->
            getCommonElementInGroups chunk
        )
        |> Seq.concat
            |> Seq.sumBy (fun c ->
                let cv = (int)c

                if ((av <= cv) && (cv <= zv)) then
                    cv - av + 1
                else
                    cv - AV + 27
            )
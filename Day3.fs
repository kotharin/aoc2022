namespace Day3

module Part1 = 

    open System.IO
    
    // Split a string into 2 halves
    let splitString (s:string) = 
        let midPoint = s.Length/2
    
        let firstHalf = s.Substring(0,midPoint)
        let secondHalf = s.Substring(midPoint)

        firstHalf, secondHalf

    // load all unique characters of a string
    // into a map
    let loadIntoMap (s:string) =
        s.ToCharArray()
        |> Array.map(fun c -> c,1)
        |> Map.ofArray

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

        let av = (int)'a'
        let zv = (int)'z'
        let AV = (int)'A'
        
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
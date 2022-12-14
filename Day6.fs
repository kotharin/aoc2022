namespace Day6

module Part1 = 
    open System.IO

    let hasNoRepeatingChars (chars:char array) =
        let map =
            chars
            |> Array.map(fun c ->
                c,1
            )
            |> Map.ofArray

        Map.count map = chars.Length

    let getMarker (data: char array) =

        let rec getMarkerRec (d:char array) start length =

            let chunk = Array.sub d start length

            if (hasNoRepeatingChars chunk) then
                start + length
            else
                getMarkerRec d (start + 1) length
        
        getMarkerRec data 0 4

    let solution inputFile =

        let data = (File.ReadAllText inputFile).ToCharArray()

        getMarker data
namespace Day6

module Shared = 
   

    let hasNoRepeatingChars (chars:char array) =
        let map =
            chars
            |> Array.map(fun c ->
                c,1
            )
            |> Map.ofArray

        Map.count map = chars.Length

    let getMarker (data: char array) numOfUniqueChars=

        let rec getMarkerRec (d:char array) start length =

            let chunk = Array.sub d start length

            if (hasNoRepeatingChars chunk) then
                start + length
            else
                getMarkerRec d (start + 1) length
        
        getMarkerRec data 0 numOfUniqueChars

module Part1 =

    open System.IO
    open Shared
    let solution inputFile =

        let data = (File.ReadAllText inputFile).ToCharArray()

        getMarker data 4

module Part2 =

    open System.IO
    open Shared
    let solution inputFile =

        let data = (File.ReadAllText inputFile).ToCharArray()

        getMarker data 14

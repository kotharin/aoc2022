namespace Day4

module Shared =

    type Section = {
        start: int
        stop : int
    }
    with
        // data = start-end (eg: 2-4)
        static member parse (data:string) =
            let ss = data.Split('-')
            {
                start = (int)ss.[0]
                stop = (int)ss.[1]
            }

    type Pairs = {
        Section1:Section
        Section2:Section
    }
    with 
        static member parse (data:string) = 
            let secs = data.Split(',')
            {
                Section1 = Section.parse secs.[0]
                Section2 = Section.parse secs.[1]
            }
        static member hasFullOverlap (pairs:Pairs) =
            if (
                ((pairs.Section1.start <= pairs.Section2.start) && (pairs.Section1.stop >= pairs.Section2.stop)) ||
                ((pairs.Section2.start <= pairs.Section1.start) && (pairs.Section2.stop >= pairs.Section1.stop))) then
                true
            else false
        static member hasOverlap pairs =
            if ((pairs.Section1.start <= pairs.Section2.stop) && (pairs.Section1.start >= pairs.Section2.start) ||
                (pairs.Section1.stop <= pairs.Section2.stop) && (pairs.Section1.stop >= pairs.Section2.start )) ||
                ((pairs.Section2.start <= pairs.Section1.stop) && (pairs.Section2.start >= pairs.Section1.start) ||
                (pairs.Section2.stop <= pairs.Section1.stop) && (pairs.Section2.stop >= pairs.Section1.start )) then
                true
            else false


module Part1 = 

    open System.IO
    open Shared

    let solution inputFile =

        File.ReadAllLines inputFile
        |> Array.map(Pairs.parse)
        |> Array.filter Pairs.hasFullOverlap
        |> Array.length

module Part2 =

    open System.IO
    open Shared
    let solution inputFile =
        File.ReadAllLines inputFile
        |> Array.map(Pairs.parse)
        |> Array.filter Pairs.hasOverlap
        |> Array.length

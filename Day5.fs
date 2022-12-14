namespace Day5

module Shared =

    type MoveInstruction = {
        From: int
        To: int
        Quantity: int
    }
    with 
        // Parse instructions
        static member parse (s:string) = 
            let parts = s.Split(' ')
            {
                From = (int)parts.[3]
                To = (int)parts.[5]
                Quantity = (int)parts.[1]
            }

    let addToStackIndex index crate (stackMap:Map<int, char list>) =
        if (crate <> ' ') then
            let newIndexList =
                Map.tryFind index stackMap
                |> Option.map(fun existing -> crate::existing)
                |> Option.defaultValue [crate]
            Map.add index newIndexList stackMap
        else
            stackMap

    // Convert the stack/crate data into a map
    // 1 -> [N,Z]
    // 2 -> [D,Z,M]
    let readStackLineData (line:string) (map:Map<int, char list>) =
        // convert into a list
        let ll = Array.toList (line.ToCharArray())

        let rec mapToStacks index (stackData:char list) (stackMap:Map<int, char list>) =
            match stackData with
            | x::y::z::' '::tail ->
                // Add data to map
                let newMap = addToStackIndex index y stackMap
                mapToStacks (index + 1) tail newMap
            | x::y::z::[] ->
                // Add data to map
                addToStackIndex index y stackMap
            | _ ->
                stackMap

        mapToStacks 1 ll map

    // Get the top most elements from each stack
    let topElementsOfStacks (stackMap:Map<int, char list>) =
        [1..stackMap.Count]
        |> List.fold (fun state i ->
            // get the crates list for that stack
            let list =
                Map.find i stackMap
            let topItem = List.last list
            (state + topItem.ToString())
        ) ""

    
module Part1 =

    open System.IO
    open Shared

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let lineBreak = 
            lines
            |> Array.findIndex(fun line -> line.Length = 0)

        let stackData, ins = Array.splitAt (lineBreak-1) lines

        // Get the initial stack representation
        let stackMap =
            stackData
            |> Array.fold(fun state line ->
                readStackLineData line state
            ) Map.empty

        // Remove the first 2 rows below the stack data for the instruction set
        let instructions = 
            Array.splitAt 2 ins
            |> snd
            |> Array.map MoveInstruction.parse

        let final = 
            instructions
            |> Array.fold (fun state ins ->

                // get the list to move FROM
                let from = Map.find ins.From state
                let insLength = List.length from
                // remove the number of crates as specified by QUANTITY
                let newStack, add = List.splitAt (insLength - ins.Quantity) from
                // Set the FROM stack to the updated value 
                let newState = Map.add ins.From newStack state
                // Add the items to the destination TO stack
                let toStack = Map.find ins.To newState
                let newToStack = List.append  toStack (List.rev add)
                let nextState = Map.add ins.To newToStack newState
                nextState
            ) stackMap

        // get the last elements in each stack
        topElementsOfStacks final

module Part2 = 
    
    open System.IO
    open Shared

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let lineBreak = 
            lines
            |> Array.findIndex(fun line -> line.Length = 0)

        let stackData, ins = Array.splitAt (lineBreak-1) lines

        // Get the initial stack representation
        let stackMap =
            stackData
            |> Array.fold(fun state line ->
                readStackLineData line state
            ) Map.empty

        // Remove the first 2 rows below the stack data for the instruction set
        let instructions = 
            Array.splitAt 2 ins
            |> snd
            |> Array.map MoveInstruction.parse

        let final = 
            instructions
            |> Array.fold (fun state ins ->

                // get the list to move FROM
                let from = Map.find ins.From state
                let insLength = List.length from
                // remove the number of crates as specified by QUANTITY
                let newStack, add = List.splitAt (insLength - ins.Quantity) from
                // Set the FROM stack to the updated value 
                let newState = Map.add ins.From newStack state
                // Add the items to the destination TO stack
                let toStack = Map.find ins.To newState
                let newToStack = List.append  toStack add
                let nextState = Map.add ins.To newToStack newState
                nextState
            ) stackMap

        // get the last elements in each stack
        topElementsOfStacks final
namespace Day9

module Part1 = 
    open System
    open System.IO

    type Position = {
        Row:int
        Col:int
    } with
        static member zero =
            {
                Row = 0
                Col = 0
            }

    type Direction =
        | Right
        | Left
        | Up
        | Down
    type Instruction = {
        Direction:Direction
        Length: int
    }
    with 
        static member parse (s:string) =
            let parts = s.Split(' ')
            let distance = int parts.[1]
            match parts.[0] with
            | "R" -> {Direction = Right; Length = distance}
            | "L" -> {Direction = Left; Length = distance}
            | "U" -> {Direction = Up; Length = distance}
            | _ -> {Direction = Down; Length = distance}

    let isAdjacent headPosition tailPosition =
        if (headPosition = tailPosition) then
            true
        elif ((headPosition.Row = tailPosition.Row) && (Math.Abs(headPosition.Col - tailPosition.Col) = 1)) then
            true
        elif ((headPosition.Col = tailPosition.Col) && (Math.Abs(headPosition.Row - tailPosition.Row) = 1)) then
            true
        elif ((Math.Abs(headPosition.Row - tailPosition.Row) = 1) && (Math.Abs(headPosition.Col - tailPosition.Col) = 1)) then
            // diagonally adjacent
            true
        else
            false
    let moveHeadByOne position instruction = 
        match instruction with
        | Right -> {position with Col = position.Col + 1}
        | Left -> {position with Col = position.Col - 1}
        | Up -> {position with Row = position.Row - 1}
        | Down -> {position with Row = position.Row + 1}
    
    let moveTail headPosition tailPosition =
        // if adjacent, don't move tail
        if (isAdjacent headPosition tailPosition) then
            tailPosition
        else
            // head tail diff is 2 or more columns away
            if ((headPosition.Row = tailPosition.Row) && (Math.Abs(headPosition.Col - tailPosition.Col) > 1)) then
                {Position.Row = tailPosition.Row; Col = (headPosition.Col + tailPosition.Col)/2}
            elif ((headPosition.Col = tailPosition.Col) && (Math.Abs(headPosition.Row - tailPosition.Row) > 1)) then
                {Row = (headPosition.Row + tailPosition.Row)/2; Position.Col = tailPosition.Col}
            elif ((Math.Abs(headPosition.Col - tailPosition.Col) = 1) && (Math.Abs(headPosition.Row - tailPosition.Row) > 1)) then
                {Position.Row = (headPosition.Row + tailPosition.Row)/2; Col = headPosition.Col}
            else
                {Row = headPosition.Row; Position.Col = (headPosition.Col + tailPosition.Col)/2}

    let move headPosition tailPosition instruction moves=
        [1..instruction.Length]
        |> List.fold(fun state _ ->
            let currHead, currTail, currMoves = state
            // Move head in appropriate direction
            let newHeadPosition = moveHeadByOne currHead instruction.Direction
            // Move tail appropriately
            let newTailPosition = moveTail newHeadPosition currTail
            // Add this position if it doesn't exist
            let newMoves = Set.add (newTailPosition.Row, newTailPosition.Col)  currMoves
            newHeadPosition,newTailPosition,newMoves
        )(headPosition, tailPosition, moves)

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let _,_,moves = 
            lines
            |> Array.map Instruction.parse
            |> Array.fold (fun state ins ->
                let h, t, m = state

                let newH, newT, newM = move h t ins m
                newH,newT,newM
            ) (Position.zero, Position.zero, Set.empty)

        Set.count moves
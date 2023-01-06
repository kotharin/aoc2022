namespace Day8

module Shared = 

    let createMatrixMap maxRows maxCols (lines:string[]) =
        lines
        |> Array.fold(fun state line ->
            let row,matrx = state
            // get each columns and set in array
            let _,_,mx =
                line.ToCharArray()
                |> Array.fold(fun rowState c ->
                    let cr,cc,m  = rowState

                    Array2D.set m cr cc ((int)c - 48)

                    (cr, cc+1, m)
                ) (row, 0, matrx)
            (row+1), mx
        ) (0,(Array2D.zeroCreate<int> maxRows maxCols))
        |> snd


module Part1 = 
    open System.IO
    open Shared

    type Visibility =
        | None = 0b0000
        | Left = 0b0001
        | Right = 0b0010
        | Top = 0b0100
        | Bottom = 0b1000


    let splitVisibility data splitAt locationValue =
        let left,right = 
            Array.splitAt splitAt data
            |> (fun (l,r) -> 
                    l, Array.removeAt 0 r
                )
        let lv = not (Array.exists (fun v -> v >= locationValue) left) 
        let rv = not (Array.exists (fun v -> v >= locationValue) right) 
   
        lv, rv

    let checkVisibility (matrix:int[,]) row col =
        // get the location value
        let value = Array2D.get matrix row col
        // isolate the row info
        let rowData = matrix.[row,*]

        let lv, rv =
            splitVisibility rowData col value
            |> (fun (l,r) -> 
                let leftV = if l then Visibility.Left else Visibility.None
                let rightV = if r then Visibility.Right else Visibility.None
                leftV, rightV
            )
        // isolate the column info
        let colData = matrix.[*,col]

        let tv, bv =
            splitVisibility colData row value
            |> (fun (t,b) -> 
                let topV = if t then Visibility.Top else Visibility.None
                let bottomV = if b then Visibility.Bottom else Visibility.None
                topV, bottomV
            )

        lv ||| rv ||| tv ||| bv
        
    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let maxRows = Array.length lines

        let maxCols = lines.[0].Length

        let matrix = createMatrixMap maxRows maxCols lines

        // iterate through the populated matrix and get visibility
        let _, visibilityCount = 
            [|0..maxRows - 1|]
            |> Array.fold(fun state row ->
                let matrx, visCount = state
                // get each columns and set in array
                let _,mx, vCount =
                    [|0..maxCols - 1|]
                    |> Array.fold(fun rowState cc ->
                        let cr,m,vc  = rowState
                        // check if its visible
                        let visibility = 
                            // if on outer edge, its visible
                            if ((cr = 0) || (cr = maxRows - 1) || (cc = 0) || (cc = maxCols - 1)) then
                                Visibility.Left // Defaulting as its visible
                            else
                                checkVisibility m cr cc
                        let newVC =
                            if (visibility = Visibility.None) then
                                vc
                            else
                                vc + 1

                        (cr, m, newVC)
                    ) (row, matrx, visCount)
                mx,vCount
            ) (matrix, 0)

        visibilityCount

module Part2 = 

    open System.IO
    open Shared

    let getCountOfTreesVisible data currentTreeValue =
        let smallerTrees = Array.takeWhile (fun i -> i < currentTreeValue) data
        // If array length > smallertTrees then there must be a tree as tall or taller
        // than current value imediately after . Add 1 to the count
        if ((data.Length) > smallerTrees.Length) then
            smallerTrees.Length + 1
        else
            smallerTrees.Length

    let splitCount data splitAt currentValue =
        let part1,part2 = 
            Array.splitAt splitAt data
            |> (fun (a,b) -> 
                    a, Array.removeAt 0 b
                )
        let part1Count = getCountOfTreesVisible (Array.rev part1) currentValue
        let part2Count = getCountOfTreesVisible part2 currentValue

        part1Count, part2Count

    let scenicScore matrix row col =
        
        // Get the tree hiehgt at the current location
        let currentValue = Array2D.get matrix row col

        // Get tree count (scenic score) for left and right sides
        let rowData = matrix.[row,*]
        let leftCount, rightCount = splitCount rowData col currentValue
        // Get tree count (scenic score) for top and bottom sides
        let colData = matrix.[*,col]
        let topCount, bottomCount = splitCount colData row currentValue

        leftCount * rightCount * topCount * bottomCount

    let solution inputFile =
        let lines = File.ReadAllLines inputFile

        let maxRows = Array.length lines

        let maxCols = lines.[0].Length

        let matrix = createMatrixMap maxRows maxCols lines

        [|0..maxRows - 1|]
        |> Array.fold (fun state row ->
            let currMax = state
            let _, max = 
                [|0..maxCols - 1|]
                |> Array.fold(fun state col ->
                    let currRow, cMax = state
                    // Get the scenic score for the current location
                    let scenicScore = scenicScore matrix currRow col
                    //printfn "row:%i, col:%i, sc:%i" currRow col scenicScore
                    // Reset max if needed
                    let newMax =
                        if (scenicScore > cMax) then
                            scenicScore
                        else cMax
                    (currRow,newMax) 
                )(row, currMax)
            max
        )(0)
namespace Day8

module Part1 = 
    open System.IO

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

        let matrix = 
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
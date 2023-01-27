namespace Day10

module Part1 = 
    open System.IO

    type Instruction =
        | Noop
        | Add of int
    with 
        static member parse s =
            if (s = "noop") then
                Noop
            else
                let parts = s.Split(' ')
                Add(int(parts.[1]))

    let countCycles inAddStage instruction recCycle recCount =
        match instruction with
        | Noop ->
            false, (recCycle), recCount
        | Add i ->
            if (not inAddStage) then
                true, (recCycle), (recCount)
            else
                false, recCycle, recCount + i

    let collectSpecificValues newCycle count =
        if ((newCycle = 20) || (newCycle = 60) || (newCycle = 100) || (newCycle = 140) || (newCycle = 180) || (newCycle = 220)) then
            Some(newCycle*count)
        else
            None


    let solution inputFile =
        let lines = File.ReadAllLines inputFile

        let _,_,_,x = 
            lines
            |> Array.fold (fun state line ->
                let instruction = Instruction.parse line
                let currInAddStage, currCycle, currCount, accum = state
                let newInAddStage, newCycle, newCount = countCycles currInAddStage instruction currCycle currCount
                let newAccum = 
                    collectSpecificValues newCycle newCount
                    |> Option.map(fun cv ->
                        cv::accum
                    )|> Option.defaultValue accum

                if (newInAddStage) then
                    let nAdd,nCyc,nCnt = countCycles newInAddStage instruction (newCycle + 1) newCount
                    let nAccum = 
                        collectSpecificValues nCyc newCount
                        |> Option.map(fun cv ->
                            cv::accum
                        )|> Option.defaultValue newAccum

                    nAdd,nCyc+1,nCnt,nAccum
                else
                    false, newCycle + 1, newCount, newAccum
            ) (false, 1, 1, List.empty)

        printfn "%A" x
        List.sum x
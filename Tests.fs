module Tests

    open Xunit

    [<Fact>]
    let ``Day1 Part1`` () =
        let answer = Day1.Part1.solution "Day1.txt"

        Assert.Equal(74711,answer)    
    
    [<Fact>]
    let ``Day1 Part2`` () =
        let answer = Day1.Part2.solution "Day1.txt"

        Assert.Equal(209481,answer)
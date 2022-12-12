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

    [<Fact>]
    let ``Day2 RPS.fromString`` () =
        let a = Day2.Shared.RPS.fromString "A"
        Assert.Equal(Day2.Shared.RPS.Rock, a)
        let b = Day2.Shared.RPS.fromString "X"
        Assert.Equal(Day2.Shared.RPS.Rock, b)
        let c = Day2.Shared.RPS.fromString "B"
        Assert.Equal(Day2.Shared.RPS.Paper, c)
        let d = Day2.Shared.RPS.fromString "Y"
        Assert.Equal(Day2.Shared.RPS.Paper, d)
    
    [<Fact>]
    let ``Day2 evaluateRound`` () =
        let x = Day2.Shared.evaluateRound Day2.Shared.Rock Day2.Shared.Paper
        Assert.Equal(x,1)
        let x = Day2.Shared.evaluateRound Day2.Shared.Paper Day2.Shared.Rock
        Assert.Equal(x,8)
        let x = Day2.Shared.evaluateRound Day2.Shared.Scissors Day2.Shared.Scissors
        Assert.Equal(x,6)
    
    [<Fact>]
    let ``Day2 Part1`` () =
        let answer = Day2.Part1.solution "Day2.txt"

        Assert.Equal(13009, answer)

    [<Fact>]
    let ``Day2 Part2`` () =
        let answer = Day2.Part2.solution "Day2.txt"

        Assert.Equal(10398, answer)
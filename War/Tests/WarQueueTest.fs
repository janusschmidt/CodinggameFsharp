module War.WarQueueTest

open Xunit
open WarUsingQueue
open Queue

[<Fact>]
let ``One card`` () =
    let p1Cards = Queue.Empty.Enqueue Rank.A 
    let p2Cards = Queue.Empty.Enqueue Rank.K

    let (res, rounds) = war p1Cards p2Cards
    Assert.Equal(GameResult.Winner P1, res)
    Assert.Equal(1, rounds)

[<Fact>]
let ``Three cards`` () =
    let p1Cards = Queue.Empty.AppendList [Rank.R4; Rank.R3; Rank.R2]
    let p2Cards = Queue.Empty.AppendList [Rank.R5; Rank.R4; Rank.R3]

    let (res, rounds) = war p1Cards p2Cards
    Assert.Equal(GameResult.Winner P2, res)
    Assert.Equal(3, rounds)

[<Fact>]
let ``PAT1`` () =
    let p1Cards = Queue.Empty.AppendList [Rank.R4; Rank.R3; Rank.R2; Rank.A; Rank.K]
    let p2Cards = Queue.Empty.AppendList [Rank.R4; Rank.R4; Rank.R3; Rank.J; Rank.K]

    let (res, rounds) = war p1Cards p2Cards

    Assert.Equal(GameResult.DRAW, res )
    
[<Fact>]
let ``PAT2`` () =
    let p1Cards = Queue.Empty.AppendList [Rank.R4; Rank.R3; Rank.R2; Rank.A; Rank.K]
    let p2Cards = Queue.Empty.AppendList [Rank.R4; Rank.R4]

    let (res, rounds) = war p1Cards p2Cards

    Assert.Equal(GameResult.DRAW, res )

[<Fact>]
let ``War`` () =
    let p1Cards = Queue.Empty.AppendList [Rank.R4; Rank.R3; Rank.R2; Rank.A; Rank.K]
    let p2Cards = Queue.Empty.AppendList [Rank.R4; Rank.R4; Rank.R3; Rank.J; Rank.Q]

    let (res, rounds) = war p1Cards p2Cards
    Assert.Equal(GameResult.Winner P1, res)
    Assert.Equal(1, rounds)

[<Fact>]
let ``Successive wars`` () =
    let p1Cards = Queue.Empty.AppendList [Rank.R4; Rank.R3; Rank.R2; Rank.A; Rank.R4; Rank.R3; Rank.R2; Rank.A; Rank.R3]
    let p2Cards = Queue.Empty.AppendList [Rank.R4; Rank.R4; Rank.R3; Rank.J; Rank.R4; Rank.R4; Rank.R3; Rank.J; Rank.R2]

    let (res, rounds) = war p1Cards p2Cards
    Assert.Equal(GameResult.Winner P1, res)
    Assert.Equal(1, rounds)
    

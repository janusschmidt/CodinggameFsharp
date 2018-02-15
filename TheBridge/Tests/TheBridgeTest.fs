module TheBridge.Tests.TheBridgeTest

open Xunit
open TheBridge.TheBridge

[<Fact>]
let ``One Lonely hole`` () =
    let numberOfBikesThatMustSurvive = 1
    let lane0Info = ".............................." |> laneSeqParser
    let lane1Info = ".............................." |> laneSeqParser
    let lane2Info = "...........0.................." |> laneSeqParser
    let lane3Info = ".............................." |> laneSeqParser
    let road = [|lane0Info; lane1Info; lane2Info; lane3Info|]
    let startPosInfo = [{x=0;y=2;active=true;speed=0}]
    
    let sut = TheBridge.TheBridge.calcStates startPosInfo road numberOfBikesThatMustSurvive
    
    match sut with
    | NoResultFound _ -> Assert.True(false, "No solution found")
    | Succes s -> Assert.Equal([|Speed; Speed; Speed; Speed; Jump; Speed; Speed; Speed|], s |> List.map (snd))

[<Fact>]
let ``Chained jumps of decreasing length`` () =
    let numberOfBikesThatMustSurvive = 4
    let lane0Info = "..............00000......0000.....00......" |> laneSeqParser
    let lane1Info = "..............00000......0000.....00......" |> laneSeqParser
    let lane2Info = "..............00000......0000.....00......" |> laneSeqParser
    let lane3Info = "..............00000......0000.....00......" |> laneSeqParser
    let road = [|lane0Info; lane1Info; lane2Info; lane3Info|]
    let startPosInfo = [
        {x=0;y=0;active=true;speed=8};
        {x=0;y=1;active=true;speed=8};
        {x=0;y=2;active=true;speed=8};
        {x=0;y=3;active=true;speed=8};
        ]
    
    let sut = TheBridge.TheBridge.calcStates startPosInfo road numberOfBikesThatMustSurvive
    
    match sut with
    | NoResultFound _ -> Assert.True(false, "No solution found")
    | Succes s -> Assert.Equal([|Slow; Slow; Jump; Slow; Jump; Slow; Jump; Speed; Speed|], s |> List.map (snd))

[<Fact>]
let ``One bike obstackle course`` () =
    let numberOfBikesThatMustSurvive = 1
    let lane0Info = ".............................0..0...." |> laneSeqParser
    let lane1Info = ".0.0..................000....000....." |> laneSeqParser
    let lane2Info = "....000.........0.0...000............" |> laneSeqParser
    let lane3Info = "............0.0......................" |> laneSeqParser
    let road = [|lane0Info; lane1Info; lane2Info; lane3Info|]
    let startPosInfo = [{x=0;y=2;active=true;speed=4}]
    
    let sut = TheBridge.TheBridge.calcStates startPosInfo road numberOfBikesThatMustSurvive
    
    match sut with
    | NoResultFound _ -> Assert.True(false, "No solution found")
    | Succes s -> Assert.Equal([|Down; Wait; Up; Up; Up; Speed; Jump; Jump; Speed|], s |> List.map (snd))
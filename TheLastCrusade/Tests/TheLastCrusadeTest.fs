module Tests.TheLastCrusadeTest

open Xunit
open TheLastCrusade

type t = Engine.turnInput
type pos = Engine.position

[<Fact>]
let ``Example 1`` () =
    let roomLayout = [|[|4;3|];[|12;10|];[|11;5|];[|2;3|];|]
    let turn1In = {t.row=0;t.col=1;t.pos=pos.Top}
    
    let turn1Out = Engine.gameTurn turn1In roomLayout
    let turn2Out = Engine.gameTurn turn1Out roomLayout
    let turn3Out = Engine.gameTurn turn2Out roomLayout
    let turn4Out = Engine.gameTurn turn3Out roomLayout
    let turn5Out = Engine.gameTurn turn4Out roomLayout
    
    Assert.Equal({t.row=1;t.col=1;t.pos=pos.Top}, turn1Out)
    Assert.Equal({t.row=1;t.col=0;t.pos=pos.Right}, turn2Out)
    Assert.Equal({t.row=2;t.col=0;t.pos=pos.Top}, turn3Out)
    Assert.Equal({t.row=2;t.col=1;t.pos=pos.Left}, turn4Out)
    Assert.Equal({t.row=3;t.col=1;t.pos=pos.Top}, turn5Out)
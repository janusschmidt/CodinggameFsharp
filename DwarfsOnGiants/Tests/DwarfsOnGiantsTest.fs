module DwarfsOnGiantsTest

open Xunit
open DwarfsOnGiants

[<Fact>]
let ``Depth 3`` () =
    let edges =[
        {node1=1;node2=2};
        {node1=1;node2=3};
        {node1=3;node2=4}]
    
    let act = getMaxChainLength edges
    let expectedNodesInvolvedInLongestPath = 3
    Assert.Equal(expectedNodesInvolvedInLongestPath, act)

[<Fact>]
let ``Depth 4`` () =
    let edges =[
        {node1=1;node2=2};
        {node1=1;node2=3};
        {node1=3;node2=4}
        {node1=2;node2=4};
        {node1=2;node2=5};
        {node1=10;node2=11};
        {node1=10;node2=1};
        {node1=10;node2=3};
        ]
    
    let act = getMaxChainLength edges
    let expectedNodesInvolvedInLongestPath = 4
    Assert.Equal(expectedNodesInvolvedInLongestPath, act)
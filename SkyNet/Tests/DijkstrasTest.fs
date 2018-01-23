module SkyNet.DijkstrasTest

open Xunit
open SkyNet.Dijkstras

[<Fact>]
let ``Example 1`` () =
    let edges = [
        {node1="c"; node2="a"; weight=1};
        {node1="a"; node2="b"; weight=3};
        {node1="c"; node2="b"; weight=7};
        {node1="b"; node2="d"; weight=5};
        {node1="c"; node2="d"; weight=2};
        {node1="d"; node2="e"; weight=7};
        {node1="b"; node2="e"; weight=1};
    ]
    
    let graph = computeDistances edges "c"
    
    Assert.Equal(5, List.length graph.visitedNodes)

    //let expected = [
    //    {id="a"; shortestDistance=0;path="a"};
    //    {id="a"; shortestDistance=0;path="a"};
    //    {id="a"; shortestDistance=0;path="a"};
    //    {id="a"; shortestDistance=0;path="a"};
    //    {id="a"; shortestDistance=0;path="a"}]

    Assert.Equal(1,2)
    
    //res
    //c: dist=0 path=[c]
    //a: dist=1 path=[c,a]
    //b: dist=4 path=[c,a,b]
    //d: dist=2 path=[c,d]
    //e: dist=5 path=[c,a,b,e]

    //let res = losses [3;2;4;2;1;5]
    
    //Assert.Equal(3, res.largestDrop)
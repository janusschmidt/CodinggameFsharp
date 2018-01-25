module Algorithms.DijkstrasTest

open Xunit
open Algorithms.Dijkstras

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
    
    let nodes = computeDistances edges "c" |> List.sortBy (fun x->x.id)
    
    let expected = [
        {id="a"; shortestDistance=1;path=["a";"c"]};
        {id="b"; shortestDistance=4;path=["b";"a";"c"]};
        {id="c"; shortestDistance=0;path=["c"]};
        {id="d"; shortestDistance=2;path=["d";"c"]};
        {id="e"; shortestDistance=5;path=["e";"b";"a";"c"]}];

    Assert.Equal<list<node<string>>>(expected, nodes)
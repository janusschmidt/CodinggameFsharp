module Algorithms.TreeTest 

open Xunit
open Tree

let edges1 = [
        {node1="1"; node2="2";};
        {node1="2"; node2="3";};
        {node1="3"; node2="4";};
        {node1="3"; node2="7";};
        {node1="4"; node2="5";};
        {node1="4"; node2="6";};
        {node1="7"; node2="8";};
    ]
    
let edges2 = [
        {node1="0";  node2="1"; };
        {node1="0";  node2="8"; };
        {node1="0";  node2="15";};
        {node1="1";  node2="2"; };
        {node1="1";  node2="5"; };
        {node1="2";  node2="3"; };
        {node1="2";  node2="4"; };
        {node1="5";  node2="6"; };
        {node1="5";  node2="7"; };
        {node1="8";  node2="9"; };
        {node1="8";  node2="12";};
        {node1="9";  node2="10";};
        {node1="9";  node2="11";};
        {node1="12"; node2="13";};
        {node1="12"; node2="14";};
        {node1="15"; node2="16";};
        {node1="15"; node2="19";};
        {node1="16"; node2="17";};
        {node1="16"; node2="18";};
        {node1="19"; node2="20";};
        {node1="19"; node2="21";};
    ]
    
[<Fact>]
let ``farthestNode 1`` () =
    let sut = edges1 |> Tree.farthestNode "1"
    
    Assert.Equal(4,sut.distance)
    
[<Fact>]
let ``farthestNode 2`` () =
    
    let sut = edges2 |> Tree.farthestNode  "0"
    
    Assert.Equal(3,sut.distance)

[<Fact>]
let ``farthestApartNodesNode 1`` () =
    let (node1,node2, distance) = Tree.farthestApartNodesNode edges1
    
    Assert.Equal(4, distance)
    
[<Fact>]
let ``farthestApartNodesNode 2`` () =
    
    let (node1,node2, distance) = Tree.farthestApartNodesNode edges2
    
    Assert.Equal(6, distance)
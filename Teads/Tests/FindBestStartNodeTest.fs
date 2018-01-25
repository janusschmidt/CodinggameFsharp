﻿namespace Teads
module FindBestStartNodeTest =

    open Xunit
    open Algorithms.Dijkstras
    open Teads.FindBestStartNode

    [<Fact>]
    let ``Example 1`` () =
        let edges = [
            {node1="1"; node2="2"; weight=1};
            {node1="2"; node2="3"; weight=1};
            {node1="3"; node2="4"; weight=1};
            {node1="3"; node2="7"; weight=1};
            {node1="4"; node2="5"; weight=1};
            {node1="4"; node2="6"; weight=1};
            {node1="7"; node2="8"; weight=1};
        ]
    
        let sut = calculateStartNodeWithMinimumNetworkTraversalTime edges
    
        Assert.True(sut.IsSome)
        Assert.Equal("3",sut.Value.id)
        Assert.Equal(2,sut.Value.shortestDistance)
    
    [<Fact>]
    let ``Example 2`` () =
        let edges = [
            {node1="0";  node2="1";  weight=1};
            {node1="0";  node2="8";  weight=1};
            {node1="0";  node2="15"; weight=1};
            {node1="1";  node2="2";  weight=1};
            {node1="1";  node2="5";  weight=1};
            {node1="2";  node2="3";  weight=1};
            {node1="2";  node2="4";  weight=1};
            {node1="5";  node2="6";  weight=1};
            {node1="5";  node2="7";  weight=1};
            {node1="8";  node2="9";  weight=1};
            {node1="8";  node2="12"; weight=1};
            {node1="9";  node2="10"; weight=1};
            {node1="9";  node2="11"; weight=1};
            {node1="12"; node2="13"; weight=1};
            {node1="12"; node2="14"; weight=1};
            {node1="15"; node2="16"; weight=1};
            {node1="15"; node2="19"; weight=1};
            {node1="16"; node2="17"; weight=1};
            {node1="16"; node2="18"; weight=1};
            {node1="19"; node2="20"; weight=1};
            {node1="19"; node2="21"; weight=1};
        ]
    
        let sut = calculateStartNodeWithMinimumNetworkTraversalTime edges
    
        Assert.True(sut.IsSome)
        //Assert.Equal("3",sut.Value.id)
        Assert.Equal(3,sut.Value.shortestDistance)

﻿module Algorithms.Dijkstras

open Graph

let computeDistances (allEdges:edge<'a> list) (focusNodeId:nodeId<'a>):node<'a> list =
    //Helpers
    let isVisited id visitedNodes = List.exists (fun e-> e=id) visitedNodes
    let findEdges id = List.filter (fun e-> e.node1=id || e.node2=id) allEdges
    let getNeighbour edge nodeId = if edge.node1=nodeId then edge.node2 else edge.node1
    let findUnvisitedEdges id visitedNodes = findEdges id |> List.filter (fun e-> not(isVisited (getNeighbour e id) visitedNodes))
    let updateShortestDistance node nodes= 
        List.fold (fun a i -> 
            if i.id = node.id && i.shortestDistance>node.shortestDistance then 
                node::a
            else
                i::a) [] nodes
    let tryFindNode id graph = List.tryFind (fun n->n.id = id) graph.nodes

    let rec calculate initialGraph =
   
        let processOneUnvisitedEdge cur graph acc e =
            let neighbourId = getNeighbour e cur.id
            let node = {id=neighbourId; shortestDistance = cur.shortestDistance+e.weight; path = neighbourId::cur.path}
            match tryFindNode neighbourId graph with
            | Some existingNode when existingNode.shortestDistance>node.shortestDistance ->
                updateShortestDistance node acc
            | Some _ -> acc
            | None -> node::acc
    
        let processNode graph cur =
            let unvisitedEdges = findUnvisitedEdges cur.id graph.visitedNodes
            let processEdge = processOneUnvisitedEdge cur graph
            let updatedNodes = unvisitedEdges |> List.fold processEdge graph.nodes
            {graph with nodes=updatedNodes;visitedNodes=cur.id::graph.visitedNodes}
            
        let orderedUnVisitedNodes = initialGraph.nodes |> List.filter (fun n-> not(isVisited n.id initialGraph.visitedNodes)) |> List.sortBy (fun i->i.shortestDistance)

        match orderedUnVisitedNodes with
        | [] -> initialGraph.nodes
        | curNode::_ ->
            let updatedGraph = processNode initialGraph curNode
            calculate updatedGraph
        
    //init
    let focusNode = {id=focusNodeId; shortestDistance=0; path=[focusNodeId]}
    let intialGraph = {nodes=[focusNode]; focusNode=focusNodeId; visitedNodes=[]}
    calculate intialGraph
    
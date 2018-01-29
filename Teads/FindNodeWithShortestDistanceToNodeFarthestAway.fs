namespace Teads
(* This method works by using dijkstras on every node, but is too slow*)
module FindNodeWithShortestDistanceToNodeFarthestAway =
    open Algorithms.Dijkstras

    let isLeafNode nodeId edges =
        let noEdgesAttachedToNode = edges |> List.filter (fun e-> e.node1=nodeId || e.node2=nodeId) |> List.length 
        noEdgesAttachedToNode=1

    let tryGetLeafNodeIdOfEdge e edges = 
        match e with
        | {node1=n} when isLeafNode n edges -> Some n
        | {node2=n} when isLeafNode n edges -> Some n
        | _ -> None

    let tryGetLeafNodeId edges = edges |> List.tryPick (fun e -> tryGetLeafNodeIdOfEdge e edges)

    let nodeWithLargestDistance n1 n2 = if n1.shortestDistance>n2.shortestDistance then n1 else n2 

    let divideRoundUp a b = 1 + (a-1)/b

    let calculateStartNodeWithMinimumNetworkTraversalTime relations=
        let rec calcInner currentBestOption edges =
            match tryGetLeafNodeId edges with
            | None -> currentBestOption
            | Some targetNodeId ->
                let farthestNode = computeDistances edges targetNodeId |> List.reduce nodeWithLargestDistance
                let newBestBet = 
                    match currentBestOption with
                    | None -> farthestNode
                    | Some currentBest -> nodeWithLargestDistance currentBest farthestNode
                let prunedEdges = edges |> List.fold (fun a i-> if i.node1=targetNodeId || i.node2=targetNodeId then a else i::a) []
                calcInner (Some newBestBet) prunedEdges

        let farthestNode = calcInner None relations
    
        farthestNode |> Option.map (fun v->
                let bestTranversalTime = divideRoundUp v.shortestDistance 2
                {v with id=v.path.[bestTranversalTime]; shortestDistance=bestTranversalTime})






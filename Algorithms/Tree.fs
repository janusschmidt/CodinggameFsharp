module Algorithms.Tree

type nodeId<'a when 'a : equality> = 'a
type edge<'a when 'a : equality> = {node1:nodeId<'a>; node2:nodeId<'a>}
type node<'a when 'a : equality> = {id:nodeId<'a>; children:node<'a> list; distance:int}

let farthestNode edges nodeID =
    let rec createTree edges nodeId =
        let rec createTreeInner edges nodeId distance =
            let isConnected e = e.node1=nodeId || e.node2=nodeId
            let getConnectedId e = if e.node1=nodeId then e.node2 else e.node1
            let (conectedEdges, reducedEdges) = edges |> List.partition isConnected
            {id=nodeId; distance= distance; children = conectedEdges |> List.map (fun e-> createTreeInner reducedEdges (getConnectedId e) (distance + 1))}
        createTreeInner edges nodeId 0
    
    let getMaxDistanceNode n1 n2 = if n1.distance>n2.distance then n1 else n2

    let findMax tree =
        let rec findMaxInner tree currentMaxNode:node<'a> =
            if tree.children |> List.isEmpty then
                getMaxDistanceNode currentMaxNode tree
            else 
                tree.children |> List.fold (fun a n-> getMaxDistanceNode a (findMaxInner n a)) tree
        findMaxInner tree tree
        
    let tree = createTree edges nodeID
    findMax tree

    
let farthestApartNodesNode edges =
    let node1 = farthestNode edges edges.[0].node1
    let node2 = farthestNode edges node1.id
    (node1,node2, node2.distance)
    


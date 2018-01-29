module Algorithms.Tree

type nodeId<'a> = 'a
type edge<'a> = {node1:nodeId<'a>; node2:nodeId<'a>}
type node<'a> = {id:nodeId<'a>; children:node<'a> list; distance:int}

let edgesMap (edges:edge<'a> list) = 
    List.concat [edges; edges |> List.map (fun e -> {node1 = e.node2; node2 = e.node1})] 
    |> List.groupBy (fun e -> e.node1) 
    |> Map.ofList 
    |> Map.map (fun k v -> List.map (fun e -> e.node2) v)

let farthestNodePrivate nodeID (edgesMap:Map<nodeId<'a>, nodeId<'a> list>) =
    
    let rec createTree (nodeId:nodeId<'a>) =
        let rec createTreeInner (nodeId:nodeId<'a>) (parentId:nodeId<'a>) (distance:int) =
            match Map.tryFind nodeId edgesMap with
            | None -> {id=nodeId; distance= distance; children = []}
            | Some conectedEdges -> 
                {
                id=nodeId; 
                distance= distance; 
                children = conectedEdges |> List.filter (fun cid->cid<>parentId) |> List.map (fun cid -> createTreeInner cid nodeId (distance + 1))
                }
        createTreeInner nodeId nodeId 0
    
    let getMaxDistanceNode n1 n2 = if n1.distance>n2.distance then n1 else n2

    let findMax tree =
        let rec findMaxInner tree currentMaxNode:node<'a> =
            if tree.children |> List.isEmpty then
                getMaxDistanceNode currentMaxNode tree
            else 
                tree.children |> List.fold (fun a n-> getMaxDistanceNode a (findMaxInner n a)) tree
        findMaxInner tree tree
        
    let tree = createTree nodeID
    findMax tree

let farthestNode nodeID edges =
    farthestNodePrivate nodeID (edges |> edgesMap)
    
let farthestApartNodesNode edges =
    let edgeMap = edgesMap edges
    let node1 = farthestNodePrivate edges.[0].node1 edgeMap
    let node2 = farthestNodePrivate node1.id edgeMap
    (node1,node2, node2.distance)
    


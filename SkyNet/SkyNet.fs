module SkyNet.SkyNet

open Algorithms.Dijkstras

let read = stdin.ReadLine
let readInts _= (read ()).Split [|' '|] |> Seq.toList |> List.map int

let numberOfNodesInclGateways, numberOfLinks, numberOfExitGateWays = 
    match readInts () with
    | x::y::z::[] -> x, y, z
    | _ -> failwith "Invalid input"

let links = List.init numberOfLinks (fun _-> 
    match readInts () with
    | h::t::[] -> h,t
    | _ -> failwith "Bad link input")

let exits = List.init numberOfExitGateWays (fun _-> read () |> int)

let allEdges = links |> List.map (fun (i,j)-> {node1=i;node2=j;weight=1})

let isExitNode nodeId = exits |> List.contains nodeId

let getExitsEdgesForNode nodeId edges = edges |> List.filter (fun e-> (e.node1=nodeId && isExitNode e.node2) || (e.node2=nodeId && isExitNode e.node1))

let hasExitEdges nodeId edges = getExitsEdgesForNode nodeId edges |> List.isEmpty |> not

let isNoChoiceNode nodeId edges= not(isExitNode nodeId) && hasExitEdges nodeId edges

(* Get edges, but set weight to zero on an edge ig the following holds*)
(* The edge is between two nodes that are both next to an exit*)
let getModifiedEdges edges=
    edges |> List.map (fun e-> 
        if (isNoChoiceNode e.node1 edges && isNoChoiceNode e.node2 edges) then
            {e with weight=0}
        else
            e)

let isSameEdge e1 e2 = (e1.node1 = e2.node1) && (e1.node2 = e2.node2) ||  (e1.node1 = e2.node2) && (e1.node2 = e2.node1)
            
let rec gameloop edges =
    let skynetAgentIndex = read() |> int
    let computedDistances = computeDistances edges skynetAgentIndex
    let distancesWithZeroWeights = computeDistances (edges |> getModifiedEdges) skynetAgentIndex
    let closestExitNode = computedDistances |> List.filter (fun i-> exits |> List.contains i.id) |> List.sortBy (fun x->x.shortestDistance) |> List.head
    let nodeWithMultipleAdjecentExits = distancesWithZeroWeights |> List.filter (fun i-> (getExitsEdgesForNode i.id edges |> List.length)>1) |> List.sortBy (fun x->x.shortestDistance)
    let edgeToCut =
        if closestExitNode.shortestDistance=1 || List.isEmpty nodeWithMultipleAdjecentExits then
            match closestExitNode.path with
            | n1::n2::_ -> {node1=n1;node2=n2;weight=1}
            | _ -> failwith "Invalid result"    
    
        else
            let closestNodeWithMultipleExits = nodeWithMultipleAdjecentExits |> List.head
            getExitsEdgesForNode closestNodeWithMultipleExits.id edges |> List.head
            
    printfn "%i %i" edgeToCut.node1 edgeToCut.node2
    
    let newEdges = edges |> List.fold (fun a i-> if isSameEdge i edgeToCut then a else i::a) []
    gameloop newEdges
    
gameloop allEdges
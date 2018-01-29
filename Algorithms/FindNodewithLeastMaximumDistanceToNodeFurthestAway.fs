module Algorithms.FindNodewithLeastMaximumDistanceToNodeFurthestAway 
open Graph
open FindThe2NodesFurthestApartInTree
   
type middleNodeResult<'a when 'a:equality> = {nodeId:nodeId<'a>; distance:int} 

(* Method: Find the two nodes furthest away from each other and pick the middle point *)
let calculate<'a when 'a:equality> (relations:edge<'a> list) =
    let divideRoundUp a b = 1 + (a-1)/b

    let nodeFurthestAway = FindThe2NodesFurthestApartInTree.calculate relations
    let middleNodeIndex = divideRoundUp nodeFurthestAway.shortestDistance 2
    let middleNode = nodeFurthestAway.path |> List.rev |> List.item middleNodeIndex
    {nodeId=middleNode; distance=middleNodeIndex}


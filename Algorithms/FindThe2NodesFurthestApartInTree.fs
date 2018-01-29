module Algorithms.FindThe2NodesFurthestApartInTree 

open Dijkstras

(* Method:
    1. Pick any node A
    2. Use Dijkstras on A
    3. Pick the Node B farthest away from A
    4. Let B be the new A and repeat from step 2
    5. When the farthest distance does not increase we are done
    *)
   
let calculate relations =
    let nodeWithLargestDistance n1 n2 = if n1.shortestDistance>n2.shortestDistance then n1 else n2 

    let farthestNode edges nodeId = computeDistances edges nodeId |> List.reduce nodeWithLargestDistance

    let rec calcInner A =
        let B = farthestNode relations A.id
        if B.shortestDistance>A.shortestDistance then
            calcInner B
        else
            A
        
    let someNodeId =  relations.[0].node1
    let A = farthestNode relations someNodeId
    calcInner A   

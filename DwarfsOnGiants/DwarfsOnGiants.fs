module DwarfsOnGiants

type nodeId = int
type edge = {node1:nodeId; node2:nodeId}

let getMaxChainLength (edges:edge list) =
    let directedEdgesMap = 
        edges
        |> List.groupBy (fun e -> e.node1) 
        |> Map.ofList 
        |> Map.map (fun k v -> List.map (fun e -> e.node2) v)

    let rec inner (nodeId:nodeId)  (maxDepth:int) depth:int=
        let newMax = max maxDepth depth
        match directedEdgesMap.TryFind nodeId with
        | None -> newMax
        | Some subNodes -> subNodes |> List.fold (fun agg v-> inner v agg (depth+1)) newMax
    
    let allChildren = directedEdgesMap |> Map.toList |> List.map (snd) |> List.concat 
    directedEdgesMap |> Map.filter (fun k _ -> allChildren |> List.contains k |> not)  |> Map.fold (fun agg k v ->  inner k agg 1) 0

let run () =
    let numberOfRelationShips = stdin.ReadLine() |> int
    let relationships = List.init numberOfRelationShips (fun _->
        let token = (stdin.ReadLine()).Split [|' '|]
        {node1=token.[0] |> int; node2=token.[1] |> int})
    let maxLength = getMaxChainLength relationships
    printfn "%i" maxLength

//run()
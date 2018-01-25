namespace Teads
module Teads =
    open Algorithms.Dijkstras
    open Teads.FindBestStartNode

    let read = stdin.ReadLine
    let readInts _= (read ()).Split [|' '|] |> Seq.toList |> List.map int

    let numberOfRelations = read() |> int

    let relations = List.init numberOfRelations (fun r ->
        match readInts () with
        | t1::t2::[] -> {node1=t1;node2=t2;weight=1} 
        | _ -> failwith "Invalid relation")

    match calculateStartNodeWithMinimumNetworkTraversalTime relations with
    | Some n -> printfn "%i" n.shortestDistance
    | None -> failwith "calculation error"
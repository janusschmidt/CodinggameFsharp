module SkyNet.SkyNet

let read = stdin.ReadLine
let readInts _= (read ()).Split [|' '|] |> Seq.toList |> List.map int

let numberOfNodesInclGateways, numberOfLinks, numberOfExitGateWays = 
    match readInts () with
    | numberOfNodesInclGateways::numberOfLinks::numberOfExitGateWays::[] -> numberOfNodesInclGateways, numberOfLinks, numberOfExitGateWays
    | _ -> failwith "Invalid input"

let links = List.init numberOfLinks (fun _-> 
    match readInts () with
    | h::t::[] -> h,t
    | _ -> failwith "Bad link input")

let exits = List.init numberOfExitGateWays (fun _-> read () |> int)

//(* game loop *)
//while true do
//    let SI = int(Console.In.ReadLine()) (* The index of the node on which the Skynet agent is positioned this turn *)
    
//    (* Write an action using printfn *)
//    (* To debug: Console.Error.WriteLine("Debug message") *)
    

//    (* Example: 3 4 are the indices of the nodes you wish to sever the link between *)
//    printfn "3 4"
//    ()


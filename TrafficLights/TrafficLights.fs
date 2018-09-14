module TrafficLights.TrafficLights

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

type trafficLightInfo = {distance:decimal<m>; duration:decimal<s>}

let getConsistentCruiseSpeed (lights:trafficLightInfo list) speedlimit =
    let lightIsOkForSpeed speed light =
        let time = light.distance / (decimal(speed) * 1000m /3600m)
        let lightturns = Decimal.Round(decimal(time / light.duration), 20)
        lightturns % 2m < 1m

    let willHitOnlyGreenLights speed =
        let hitsGreenLight = lightIsOkForSpeed speed
        List.forall hitsGreenLight lights
    
    [1..speedlimit]
    |> List.rev 
    |> List.find willHitOnlyGreenLights

let run () = 
    let readIntMsg msg = 
        let s = stdin.ReadLine()
        eprintfn "%s: %s" msg s
        s |> int

    let speedLimit = readIntMsg "Speedlimit" |> int
    let lightCount = readIntMsg "lightCount"
    
    let readLightsinfo _ = 
        match (stdin.ReadLine()).Split [|' '|] |> Array.toList |> List.map int with
        | h :: t :: [] -> 
            eprintfn "distance:%i, duration:%i" h t
            {distance=decimal(h)*1m<m>; duration=decimal(t)*1m<s>}
        | _ -> raise (System.ArgumentException("traffic light info incomplete"))

    let lights = List.init lightCount readLightsinfo
    let cruisespeed = getConsistentCruiseSpeed lights speedLimit
    printfn "%i" cruisespeed

//run ()
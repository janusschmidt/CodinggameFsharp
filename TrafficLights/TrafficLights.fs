module TrafficLights.TrafficLights

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// d = distance to light
/// t = time between shifts
/// conditions for hitting the green light in turn n:
/// speed >= d/(((n*2)+1)*t)
/// speed < d/(n*2*t)(except for n=0)
/// 
/// n = 0
/// speed >= d/t
/// speed < infinity
/// 
/// n = 1
/// speed >= d/3t 
/// speed < d/2t
/// 
///
/// and so forth

type trafficLightInfo = {distance:decimal<m>; duration:decimal<s>}

let kmh = 1000m<m>/3600m<s>

let getConsistentCruiseSpeed lights speedlimit =
    let r10ms d = Decimal.Round(d|>decimal,10)*1m<m/s>

    let kmhFloor ms = 
        let kmhspeed = Decimal.Round(ms / kmh, 10)
        let kmhspeedFloor = floor kmhspeed
        let msSpeed = kmhspeedFloor * kmh
        r10ms msSpeed

    let rec getCruiseSpeed lights speedlimit =
        let getCruiseSpeedForOneLight light speedlimit =
            let minspeed turn = light.distance / ((1m + decimal(turn) * 2m) * light.duration) |> r10ms
            let maxspeed turn = (if turn=0 then speedlimit else light.distance / (decimal(turn) * 2m * light.duration)) |> kmhFloor
            let maxTurnSpeed = 
                Seq.initInfinite (fun i -> (minspeed i, maxspeed i)) 
                |> Seq.find (fun (min, _) -> min < speedlimit)
                |> snd
            min maxTurnSpeed speedlimit

        match lights with
        | h :: t -> getCruiseSpeed t (getCruiseSpeedForOneLight h speedlimit)
        | [] -> speedlimit
    
    let speedlimitConvergentSeries = Seq.unfold (fun currentBestSpeedlimit -> 
        if currentBestSpeedlimit = 0m<m/s> then 
            None
        else
            let newSpeedLimit = getCruiseSpeed lights currentBestSpeedlimit
            Some(newSpeedLimit, if newSpeedLimit = currentBestSpeedlimit then 0m<m/s> else newSpeedLimit)) (kmhFloor speedlimit)
            
    Seq.last speedlimitConvergentSeries / kmh |> round |> int

let run () = 
    let readIntMsg msg = 
        let s = stdin.ReadLine()
        eprintfn "%s: %s" msg s
        s |> int

    let speedLimit = (readIntMsg "Speedlimit" |> decimal) * kmh
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
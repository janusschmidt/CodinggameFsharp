module TheBridge.TheBridge


type action = | Speed | Slow | Jump | Wait | Up | Down
let allActions = [Speed; Wait; Up; Down; Jump; Slow; ]

type roadType = | Safe | Hole
type bikeInfo = {x:int;y:int;active:bool;speed:int}
type bikesInfo = List<bikeInfo>
type result =
    | NoResultFound of (bikesInfo * action) list
    | Succes of (bikesInfo * action) list

let parseRoadType c = if c = '.' then Safe else Hole
let laneSeqParser l = Seq.map parseRoadType l |> Seq.toArray
let maxDepth = 3

(* get new state of bikes with crashed bikes removed *)
let getNewState action (road:roadType[][]) (bikeState:bikesInfo) =
    let roadLength = Array.length road.[0]

    let destinationIsHole x y = 
        (x<roadLength) && (road.[y].[x]=Hole)
    
    let safeDriveUntilDest before after =
        match after.x-before.x with
        | 0 | 1 -> true
        | d -> 
            let step = if  before.y>after.y then -1 else 1
            [before.y.. step ..after.y] |> List.exists (fun y -> 
                [(before.x+1)..(after.x-1)] |> List.exists (fun x -> 
                    destinationIsHole x y)) |> not

    bikeState |> List.choose (fun s ->
        let newState = 
            match action with
            | Speed -> 
                let newSpeed = s.speed + 1
                Some {s with x=s.x+newSpeed;speed=newSpeed}
            | Slow ->
                if s.speed=0 then 
                    None 
                else
                    let newSpeed = s.speed - 1
                    Some {s with x=s.x+newSpeed;speed=newSpeed}
            | Wait | Jump -> Some {s with x=s.x+s.speed}
            | Up -> if s.y=0 then None else Some {s with x=s.x+s.speed;y=s.y-1}
            | Down -> if s.y=3 then None else Some {s with x=s.x+s.speed;y=s.y+1}
        
        match newState with
        | None -> None
        | Some ns ->
            let safeDrive =
                (destinationIsHole ns.x ns.y |> not) && 
                match action with
                | Jump -> true
                | _ -> safeDriveUntilDest s ns

            if safeDrive then Some ns else None
        )

(* Calculate new action by using depth first search*)
let calculateAction (bikeInfos:bikesInfo) road numberOfBikesThatMustSurvive:action =
    let rec calculateActionInner (bikeInfos:bikesInfo) road depth  usedStates:action option =
        allActions |> List.tryFind (fun action -> 
            let newState = getNewState action road bikeInfos
            let noActiveBikesAfter = List.length newState
            let notTooHeavyBikeLoss = (noActiveBikesAfter >= numberOfBikesThatMustSurvive)
            let newStateNotUsedState = usedStates |> List.contains newState |> not
            let stateOk = notTooHeavyBikeLoss && newStateNotUsedState
            (depth=maxDepth || (stateOk && (calculateActionInner newState road (depth+1) (newState::usedStates) |> Option.isSome))))
    
    match calculateActionInner bikeInfos road 0 [bikeInfos] with
    | None -> failwith "No solution without bikeLoss"
    | Some s -> s

(* gameloop. tries to calculate safe path until finish *)
let calcStates (startBikeState:bikeInfo list) (road:roadType[][]) numberOfBikesThatMustSurvive = 
    let roadLength = Array.length road.[0]
    let gameStateInner startBikeState road = 
        let res = [1..roadLength] |> List.fold (fun (aggList, (bikeState:bikeInfo list)) _ -> 
            match bikeState with
            | [] -> (aggList, bikeState)
            | bi::t when bi.x>roadLength -> (aggList, bikeState)
            | _ ->
                let action = calculateAction bikeState road numberOfBikesThatMustSurvive
                let newState = getNewState action road bikeState
                ((bikeState, action)::aggList, newState)) ([], startBikeState)
        (fst res |> List.rev, snd res)
        
    
    match gameStateInner startBikeState road with
    | aggList, [] -> NoResultFound aggList
    | aggList, _ -> Succes aggList 

let run () =
    let readln _= 
        let r = stdin.ReadLine()
        eprintfn "%s" r
        r

    let readLane = readln >> laneSeqParser

    let readInt = readln >> int

    let actionToString a =
        match a with
        | Speed -> "SPEED"
        | Slow -> "SLOW"
        | Jump -> "JUMP" 
        | Wait -> "WAIT"
        | Up -> "UP"
        | Down -> "DOWN"

    let M = readInt() (* the amount of motorbikes to control *)
    let numberOfBikesThatMustSurvive = readInt() (* the minimum amount of motorbikes that must survive *)
    
    let L0 = readLane() (* L0 to L3 are lanes of the road. A dot character . represents a safe space, a zero 0 represents a hole in the road. *)
    let L1 = readLane()
    let L2 = readLane()
    let L3 = readLane()
    let road = [|L0;L1;L2;L3|]

    let readBikesState () =    
        let S = readInt() (* the motorbikes' speed *)
        List.init M (fun _->
            let token = readln().Split [|' '|]
            let X = int(token.[0]) (* X: x coordinate of the motorbike *)
            let Y = int(token.[1]) (* Y: y coordinate of the motorbike *)
            let A = int(token.[2]) (* A: indicates whether the motorbike is activated "1" or detroyed "0" *)
            {x=X;y=Y;speed=S;active=if A=1 then true else false}) 
        |> List.filter (fun i-> i.active)

    let state = readBikesState()
    
    let states = calcStates state road numberOfBikesThatMustSurvive

    match states with
    | NoResultFound _ -> failwith "No solution found"
    | Succes res ->
        res |> List.map snd |> List.iter (fun action-> 
            printfn "%s" (action |> actionToString)
            let dummyState = readBikesState ()
            ())

//run ()
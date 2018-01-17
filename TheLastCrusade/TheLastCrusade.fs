namespace TheLastCrusade
module Input =
    open System

    let read = Console.In.ReadLine
    let readArr _ = (read()).Split [|' '|]
    let readIntArr _ = readArr() |>  Array.map int

    let getInput () =
        let token = readIntArr()
        let numCols = token.[0]
        let numRows = token.[1]
        let rooms = Array.init numRows readIntArr
        let exitCol = read() |> int
        rooms

module Engine =
    type position = Top | Left | Right | Bottom
    type turnInput = {row:int; col:int; pos:position}
    type entryExit = {entry:position; exit:position}
    let roomTypes = [
        []; 
        [
            {entry=position.Left; exit=position.Bottom};
            {entry=position.Right; exit=position.Bottom};
            {entry=position.Top; exit=position.Bottom}];
        [
            {entry=position.Left; exit=position.Right};
            {entry=position.Right; exit=position.Left}];
        [
            {entry=position.Top; exit=position.Bottom}];
        [
            {entry=position.Top; exit=position.Left};
            {entry=position.Right; exit=position.Bottom}];
        [
            {entry=position.Top; exit=position.Right};
            {entry=position.Left; exit=position.Bottom}];
        [
            {entry=position.Left; exit=position.Right};
            {entry=position.Right; exit=position.Left}];
        [
            {entry=position.Top; exit=position.Bottom};
            {entry=position.Right; exit=position.Bottom}];
        [
            {entry=position.Left; exit=position.Bottom};
            {entry=position.Right; exit=position.Bottom}];
        [
            {entry=position.Left; exit=position.Bottom};
            {entry=position.Top; exit=position.Bottom}];
        [
            {entry=position.Top; exit=position.Left}];
        [
            {entry=position.Top; exit=position.Right}];
        [
            {entry=position.Right; exit=position.Bottom}];
        [
            {entry=position.Left; exit=position.Bottom}];
     ]

    let parsePosition s =
        match s with
        | "TOP" -> Top
        | "LEFT" -> Left
        | "RIGHT" -> Right
        | _ -> failwith "unknown position"
    
    let nextTurn roomWithExitPos =
        match roomWithExitPos with
        | {row=r;col=c;pos=p} when p = Bottom   -> {roomWithExitPos with row = r + 1; pos = Top}
        | {row=r;col=c;pos=p} when p = Left     -> {roomWithExitPos with col = c - 1; pos = Right}
        | {row=r;col=c;pos=p} when p = Right    -> {roomWithExitPos with col = c + 1; pos = Left}
        | _ -> failwith "unknown next room"

    let parseInputTurn (s:string array) = {col = s.[0] |> int; row = s.[1] |> int; pos = parsePosition s.[2]}

    let gameTurn t (roomLayout:int[][]) =
        let roomType = roomLayout.[t.row].[t.col]
        let entryExits = roomTypes.[roomType]
        let exitPos = entryExits |> List.find (fun ee -> ee.entry=t.pos)
        nextTurn {t with pos = exitPos.exit}
    
    let createOutputSequence turnInput roomLayout = Seq.map (fun t -> gameTurn t roomLayout) turnInput

module Runner =
    let inputSequence = Seq.initInfinite (fun _ -> Input.readArr() |> Engine.parseInputTurn)
    let run roomLayout = Engine.createOutputSequence inputSequence roomLayout |> Seq.iter (fun item -> printfn "%i %i" item.col item.row)
    //Input.getInput() |> run

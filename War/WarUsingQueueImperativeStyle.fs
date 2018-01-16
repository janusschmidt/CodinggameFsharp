module War.WarUsingQueueImperativeStyle

open System
open Queue

type Winner = P1| P2
type GameResult = Winner of Winner | DRAW
type Rank = | R2 = 2 | R3=3 | R4=4 | R5=5 | R6=6 | R7=7| R8=8 | R9=9 | R10=10 | J=11 | Q=12 | K=13 | A=14
        
let read() = Console.In.ReadLine()
            
let parseCard s =
    match s |> Seq.toList with
    | r::_ -> 
        match r with
            | '2' -> Rank.R2
            | '3' -> Rank.R3
            | '4' -> Rank.R4
            | '5' -> Rank.R5
            | '6' -> Rank.R6
            | '7' -> Rank.R7
            | '8' -> Rank.R8
            | '9' -> Rank.R9
            | '1' -> Rank.R10
            | 'J' -> Rank.J
            | 'Q' -> Rank.Q
            | 'K' -> Rank.K
            | 'A' -> Rank.A
            | x -> failwithf "Unknown rank %c" x
    | _ -> failwith "Cannot parse card"

let readPlayerCards() = List.init (read() |> int) (fun _ -> read()) |> List.map parseCard |> List.fold (fun (a:Queue<Rank>) i -> a.Enqueue i) Queue.Empty
        
let war (p1Cards:Queue<Rank>) (p2Cards:Queue<Rank>) =
    let roundref = ref 0
    let p1WarPileref = ref []
    let p2WarPileref = ref []

    let clearPiles () = 
        p1WarPileref := []
        p2WarPileref := []
    
    let addRanksToPile p1Rank p2Rank =
        p1WarPileref := p1Rank::!p1WarPileref
        p2WarPileref := p2Rank::!p2WarPileref
    
    let rec loop (p1Cards:Queue<Rank>) (p2Cards:Queue<Rank>) =
        let warPileLength = List.length !p1WarPileref
        if warPileLength=0 then roundref := !roundref+1 
        match warPileLength % 4, p1Cards.Dequeue, p2Cards.Dequeue, !p1WarPileref, !p2WarPileref with
        | 1, _, None, w1::_, w2::_ when w1>w2 -> Winner P1
        | 1, None, _, w1::_, w2::_ when w1<w2 -> Winner P2
        | _, None, _, _, _ | _, _, None, _, _-> DRAW
        | 1, p1, p2, w1::_, w2::_ ->
            match compare w1 w2 with
            | 1 ->
                let winnings = List.append !p1WarPileref !p2WarPileref
                clearPiles ()
                loop (p1Cards.AppendList winnings) p2Cards
            | -1 ->
                let winnings = List.append !p1WarPileref !p2WarPileref
                clearPiles ()
                loop p1Cards (p2Cards.AppendList winnings)
            | _ ->
                match p1, p2 with
                | None, _ | _, None -> DRAW
                | Some (p1Rank, p1), Some (p2Rank, p2) -> 
                    addRanksToPile p1Rank p2Rank
                    loop p1 p2 
        | _, Some (p1Rank, p1), Some (p2Rank, p2), _, _ -> 
            addRanksToPile p1Rank p2Rank
            loop p1 p2

    loop p1Cards p2Cards, !roundref

let run() =
    let p1Cards = readPlayerCards()
    let p2Cards = readPlayerCards()

    match war p1Cards p2Cards with
    | Winner P1, rounds -> printfn "1 %i" rounds
    | Winner P2, rounds -> printfn "2 %i" rounds
    | _ ->  printfn "PAT"
        
//run()



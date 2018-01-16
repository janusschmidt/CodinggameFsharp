module War.WarFirstStab
open System

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

let readPlayerCards() = List.init (read() |> int) (fun _ -> read()) |> List.map parseCard
let p1Cards = readPlayerCards()
let p2Cards = readPlayerCards()

type Winner = P1| P2
type GameResult = Winner of Winner | DRAW

let rec drawCard cards pile warPile=
    match (cards, pile) with
    | [], p -> drawCard (List.rev p) [] warPile
    | h::t, p -> t, p, h::warPile

let prepend l (prependees:'a list list) =
    let rec innerprepend l p =
        match p with
        | h::t -> innerprepend (h::l) t
        | [] -> l
    List.fold (fun acc p -> innerprepend acc (List.rev p)) l prependees

let prependWarPiles l w1 w2 =
    prepend l [w1;w2]

let rec war p1Cards p1Pile p1WarPile p2Cards p2Pile p2WarPile rounds=
    let outOfCards l1 l2 = List.append l1 l2 |> List.isEmpty
    let p1OutOfCards = outOfCards p1Cards p1Pile
    let p2OutOfCards = outOfCards p2Cards p2Pile
    
    let warPileDepth = List.length p1WarPile
    match warPileDepth, warPileDepth % 4, p1WarPile, p2WarPile with
    | 0, _, _, _ when p1OutOfCards -> Winner P2, rounds
    | 0, _, _, _ when p2OutOfCards -> Winner P1, rounds
    | _, m4, p1Rank::_, p2Rank::_ when (p1OutOfCards || p2OutOfCards) && (m4<>1 || p1Rank=p2Rank) -> DRAW, 0
    | _, 1, p1Rank::_, p2Rank::_ when p1Rank>p2Rank -> war p1Cards (prependWarPiles p1Pile p1WarPile p2WarPile) [] p2Cards p2Pile [] (rounds+1) 
    | _, 1, p1Rank::_, p2Rank::_ when p1Rank<p2Rank -> war p1Cards p1Pile [] p2Cards (prependWarPiles p2Pile p1WarPile p2WarPile) [] (rounds+1)
    | _ ->
        let (p1c, p1p, p1wp), (p2c, p2p, p2wp) = drawCard p1Cards p1Pile p1WarPile, drawCard p2Cards p2Pile p2WarPile
        war p1c p1p p1wp p2c p2p p2wp rounds

match war p1Cards [] [] p2Cards [] [] 0 with
| Winner P1, rounds -> printfn "1 %i" rounds
| Winner P2, rounds -> printfn "2 %i" rounds
| _ ->  printfn "PAT"

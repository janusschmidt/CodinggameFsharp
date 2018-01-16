module StockExchangeLosses.StockExchangeLosses

open System

type bestAnswer = {max:int; largestDrop:int}

let losses quotes =
    let folder (agg:bestAnswer) (quote:int) =
        let newMax = max agg.max quote
        let drop = newMax - quote
        let largestdrop = max agg.largestDrop drop
        {max=newMax; largestDrop=largestdrop}

    quotes |> List.fold folder {max=0;largestDrop=0}

let run() =
    let read() = Console.In.ReadLine()
    let numberOFQuotes = read() |> int
    let quotes = read().Split [|' '|] |> Array.map int |> Seq.toList

    let {largestDrop=d} = losses quotes
    if d>0 then
        printfn "-%i" d
    else
        printfn "0"
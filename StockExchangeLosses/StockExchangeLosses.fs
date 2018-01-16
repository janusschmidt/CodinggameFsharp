module StockExchangeLosses.StockExchangeLosses

open System

type bestAnswer = {max:int; largestDrop:int}

let losses quotes =
    let updateMax a b = if a<b then b else a

    let folder (agg:bestAnswer) (quote:int) =
        let max = updateMax agg.max quote
        let drop = max - quote
        let largestdrop = updateMax agg.largestDrop drop
        {max=max; largestDrop=largestdrop}

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
module ConwaySequence.ConwaySequence

let calculate (numbers:string) indexOfLineToCalculate =
    let rec calculateInner (numberSeq:string list) linesLeftToCalculate =
        let parseLine (lnumbers:string List) =
            let l = "X"::lnumbers |> List.rev
 
            let (_,_,newSeq) = l |> List.fold (fun (currentNumber, count, newSeq) item ->
                match currentNumber with 
                | None -> (Some item, 1, [])
                | Some cur when cur=item -> (Some item, count + 1, newSeq)
                | Some cur -> 
                    let countAsString = count |> string
                    (Some item, 1, countAsString::cur::newSeq)) (None, 0, [])
                
            newSeq
 
        if linesLeftToCalculate < 2 then
            numberSeq
        else
            let newSeq = numberSeq |> parseLine 
            calculateInner newSeq (linesLeftToCalculate - 1)
    
    calculateInner (numbers.Split [|' '|] |> Array.toList) indexOfLineToCalculate
    
let run _ =
    let numberSeq = stdin.ReadLine()
    let indexOfLineToCalculate = stdin.ReadLine() |> int
    
    let res = calculate numberSeq indexOfLineToCalculate
    printfn "%s" (res |> List.reduce (fun i1 i2 -> i1 + " " + i2))

//run ()
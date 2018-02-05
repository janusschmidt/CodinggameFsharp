module NetworkCabling.NetworkCabling

let N = int(stdin.ReadLine())
let coords = List.init N (fun _ -> 
    let s = stdin.ReadLine()
    eprintfn "coord %s" s
    let token = s.Split [|' '|]
    let X = token.[0]|>int
    let Y = token.[1]|>int
    (X,Y))

let isEven n = n % 2 = 0

let median (lin:int list) =
    let l = lin |> List.sort
    let n = List.length l
    if isEven n then
        bigint l.[n/2]
    else
        let n1= bigint l.[(n-1)/2]
        let n2= bigint l.[n/2]
        (n1+n2)/(bigint 2)
        
let yCoords = coords |> List.map (snd)
let xCoords = coords |> List.map (fst)
let maxXCoord = xCoords |> List.reduce (max)
let minXCoord = xCoords |> List.reduce (min)
let med = median yCoords
let xdist = bigint (abs (maxXCoord - minXCoord))
let ydist = yCoords |> List.fold (fun a c -> a + (abs (med - (bigint c)))) (bigint 0)
eprintfn "median %A" med
eprintfn "xdist %A" xdist
eprintfn "ydist %A" ydist

printfn "%A" (xdist + ydist)
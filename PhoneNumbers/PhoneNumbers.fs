module PhoneNumbers

type prefixTreeNode = {value:char; nodes:prefixTreeNode list; isContentNode:bool}

let countNodes node = 
    let rec inner node = 
        node.nodes |> List.fold (fun agg n -> agg + inner n) 1
    (inner node) - 1

let buildTrieTree (numbers:string list)=
    let rec buildInner numbers=
        let skipFirstChar s = Seq.skip 1 s |> Seq.toArray |> System.String
        let isEmptyString (s:string) = s = ""
        let isNotEmptyString = isEmptyString >> not
        let prefixGroups = numbers |> List.filter isNotEmptyString |> List.groupBy (fun n -> n.[0])
        prefixGroups |> List.map (fun g-> 
            let key = fst g
            let subNumbers = snd g |> List.map skipFirstChar
            let isContentNode = subNumbers |> List.exists isEmptyString
            {value=key;nodes=buildInner subNumbers; isContentNode=isContentNode}) 

    {value=' ';nodes=buildInner numbers;isContentNode=false}

let run () =
    let N = stdin.ReadLine() |> int
    let numbers = List.init N (fun n -> stdin.ReadLine())
    let tree = buildTrieTree numbers
    printfn "%i" (countNodes tree)

//run ()
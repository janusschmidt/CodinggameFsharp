module War.Queue

type Queue<'a> private (o:'a list,i:'a list) =
    member public __.Dequeue =
        match o with
        | h::t  -> Some (h, Queue(t, i)) 
        | []    -> 
            if List.isEmpty i then
                None
            else
                Queue(List.rev i, []).Dequeue
    
    member public __.Length = o.Length + i.Length

    member public __.Enqueue newItem = Queue(o, newItem::i)

    member public q.Append (newQ:Queue<'a>)  = 
        match newQ.Dequeue with
        | None -> q
        | Some (c, qn) -> (q.Enqueue c).Append qn

    member public q.AppendList = q |> List.fold (fun a i -> a.Enqueue i)

    static member Empty:Queue<'a> = Queue([],[])
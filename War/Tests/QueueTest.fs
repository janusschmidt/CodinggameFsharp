module War.Tests.QueueTest

open Xunit
open War.Queue

[<Fact>]
let ``Enqueue One Dequeue`` () =
    let q1 = Queue.Empty.Enqueue 3
    let res = q1.Dequeue
    
    Assert.True(res.IsSome)
    Assert.Equal(3, fst res.Value)

[<Fact>]
let ``Enqueue One Dequeue Twice`` () =
    let q1 = Queue.Empty.Enqueue 3 
    let res = q1.Dequeue
    let res = (snd res.Value).Dequeue
    
    Assert.True(res.IsNone)

[<Fact>]
let ``Enqueue Dequeue 3`` () =
    let q = Queue.Empty.Enqueue 3
    let q = q.Enqueue 4
    let q = q.Enqueue 5
    
    let res = q.Dequeue
    Assert.True(res.IsSome)
    Assert.Equal(3, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(4, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(5, fst res.Value)


[<Fact>]
let ``Append`` () =
    let q = Queue.Empty.Enqueue 3 
    let q = q.Enqueue 4
    let q = q.Enqueue 5
    
    let q2 = Queue.Empty.Enqueue 11
    let q2 = q2.Enqueue 12
    let q2 = q2.Enqueue 13

    let res = q.Append q2

    let res = res.Dequeue
    Assert.True(res.IsSome)
    Assert.Equal(3, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(4, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(5, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(11, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(12, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(13, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsNone)

[<Fact>]
let ``Append List`` () =
    let q = Queue.Empty.Enqueue 3 
    let q = q.Enqueue 4
    let q = q.Enqueue 5
    
    let l = [11;12;13]

    let res = q.AppendList l

    let res = res.Dequeue
    Assert.True(res.IsSome)
    Assert.Equal(3, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(4, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(5, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(11, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(12, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsSome)
    Assert.Equal(13, fst res.Value)

    let res = (snd res.Value).Dequeue 
    Assert.True(res.IsNone)

[<Fact>]
let ``Length`` () =
    let q = Queue.Empty.Enqueue 3 
    let q = q.Enqueue 4
    let q = q.Enqueue 5
   
    Assert.Equal(3, q.Length)
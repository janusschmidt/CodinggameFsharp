module ConwaySequence.Tests.ConwaySequenceTest

open Xunit

let toString l = l |> List.reduce (fun i1 i2 -> i1 + " " + i2)

[<Fact>]
let ``ConwaySequence 1 6`` () =
    //let expected = 
    //    ["1";
    //    "11";
    //    "21";
    //    "1211";
    //    "111221";
    //    "312211"]

    let res = ConwaySequence.ConwaySequence.calculate "1" 6
    
    Assert.Equal(toString ["3";"1";"2";"2";"1";"1"], toString res)

[<Fact>]
let ``ConwaySequence 25 10`` () =
   
    let res = ConwaySequence.ConwaySequence.calculate "25" 10
    
    Assert.Equal(toString ["3";"1";"1";"3";"1";"1";"2";"2";"2";"1";"1";"3";"1";"1";"1";"2";"3";"1";"1";"3";"3";"2";"2";"1";"1";"25"], toString res)




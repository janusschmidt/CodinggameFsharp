module ConwaySequence.Tests.ConwaySequenceTest

open Xunit

[<Fact>]
let ``ConwaySequence 1 6`` () =
    let expected = 
        ["1";
        "11";
        "21";
        "1211";
        "111221";
        "312211"]

    let res = ConwaySequence.ConwaySequence.calculate "1" 6
    
    Assert.Equal(expected.[6-1], res)


[<Fact>]
let ``ConwaySequence 25 10`` () =
   
    let res = ConwaySequence.ConwaySequence.calculate "25" 10
    
    Assert.Equal("311311222113111231133221125", res)




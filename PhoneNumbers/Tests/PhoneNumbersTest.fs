module PhoneNumbersTest

open Xunit

[<Fact>]
let ``One number`` () =
    let numbers = ["0467123456"]
    let expectedNumberOfNodes = 10
    let res = PhoneNumbers.buildTrieTree numbers  |> PhoneNumbers.countNodes
    Assert.Equal(expectedNumberOfNodes, res)


[<Fact>]
let ``Two numbers`` () =
    let numbers = ["0123456789";"1123456789"]
    let expectedNumberOfNodes = 20
    let res = PhoneNumbers.buildTrieTree numbers  |> PhoneNumbers.countNodes
    Assert.Equal(expectedNumberOfNodes, res)

[<Fact>]
let ``Two numbers, different length`` () =
    let numbers = ["0123456789";"0123"]
    let expectedNumberOfNodes = 10
    let res = PhoneNumbers.buildTrieTree numbers  |> PhoneNumbers.countNodes
    Assert.Equal(expectedNumberOfNodes, res)

[<Fact>]
let ``Multiple numbers`` () =
    let numbers = ["0412578440";"0412199803";"0468892011";"112";"15"]
    let expectedNumberOfNodes = 28
    let res = PhoneNumbers.buildTrieTree numbers  |> PhoneNumbers.countNodes
    Assert.Equal(expectedNumberOfNodes, res)
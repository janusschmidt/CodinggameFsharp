module StockExchangeLosses.Tests.StockExchangeLossesTest

open Xunit
open StockExchangeLosses.StockExchangeLosses

[<Fact>]
let ``Example 1`` () =
    let res = losses [3;2;4;2;1;5]
    
    Assert.Equal(3, res.largestDrop)

[<Fact>]
let ``Example 2`` () =
    let res = losses [5;3;4;2;3;1]
    
    Assert.Equal(4, res.largestDrop)

[<Fact>]
let ``Example 3`` () =
    let res = losses [1;2;4;4;5]
    
    Assert.Equal(0, res.largestDrop)


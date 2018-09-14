module TrafficLights.Tests.TrafficLightsTest

open Xunit
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open TrafficLights.TrafficLights
open System.Diagnostics

[<Fact>]
let ``One light. Max speed will do it`` () =
    let speed = TrafficLights.TrafficLights.getConsistentCruiseSpeed [{distance=200m<m>; duration=15m<s>}] (50m*kmh)
    Assert.Equal(50, speed)


[<Fact>]
let ``One light. Max speed will not suffice`` () =
    let speed = TrafficLights.TrafficLights.getConsistentCruiseSpeed [{distance=200m<m>; duration=10m<s>}] (50m*kmh)
    Assert.Equal(36, speed)

[<Fact>]
let ``Three lights`` () =
    let speed = 
        TrafficLights.TrafficLights.getConsistentCruiseSpeed [
            {distance=300m<m>; duration=30m<s>};
            {distance=1500m<m>; duration=20m<s>};
            {distance=3000m<m>; duration=10m<s>}] (90m*kmh)
    Assert.Equal(67, speed)

[<Fact>]
let ``German highway`` () =
    let speed = 
        TrafficLights.TrafficLights.getConsistentCruiseSpeed [
            {distance=1000m<m>; duration=15m<s>};
            {distance=3000m<m>; duration=10m<s>};
            {distance=4000m<m>; duration=30m<s>};
            {distance=5000m<m>; duration=30m<s>};
            {distance=6000m<m>; duration=5m<s>};
            {distance=7000m<m>; duration=10m<s>};] (200m*kmh)
    Assert.Equal(60, speed)

[<Fact>]
let ``Fast lights`` () =
    let speed = 
        TrafficLights.TrafficLights.getConsistentCruiseSpeed [
            {distance=1234m<m>;  duration=5m<s>};
            {distance=2468m<m>;  duration=5m<s>};
            {distance=3702m<m>;  duration=5m<s>};
            {distance=6170m<m>;  duration=5m<s>};
            {distance=8638m<m>;  duration=5m<s>};
            {distance=13574m<m>; duration=5m<s>};
            {distance=16042m<m>; duration=5m<s>};
            {distance=20978m<m>; duration=5m<s>};
            {distance=23446m<m>; duration=5m<s>};
            {distance=28382m<m>; duration=5m<s>};
            {distance=35786m<m>; duration=5m<s>};
            {distance=38254m<m>; duration=5m<s>};
            {distance=45658m<m>; duration=5m<s>};
            {distance=50594m<m>; duration=5m<s>};
            {distance=53062m<m>; duration=5m<s>};
            {distance=57998m<m>; duration=5m<s>};] (90m*kmh)
    Assert.Equal(74, speed)
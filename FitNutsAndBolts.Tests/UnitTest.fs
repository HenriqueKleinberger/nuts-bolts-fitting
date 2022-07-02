module FitNutsAndBolts.Tests

open NUnit.Framework
open Exercice

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.ShouldReturnEmptyListWhenBagIsEmpty() =
        let bag = []

        let result = fitBoltsAndNuts bag

        Assert.IsTrue(result.Length = 0)
    

    [<Test>]
    member this.ShouldReturnFittingNutsAndBoltsWhenAllFits() =
        let bag = [
            { ToolType = Nut; Size = 10 }
            { ToolType = Bolt; Size = 10 }
            { ToolType = Nut; Size = 5 }
            { ToolType = Bolt; Size = 12 }
            { ToolType = Nut; Size = 12 }
            { ToolType = Nut; Size = 18 }
            { ToolType = Bolt; Size = 18 }
            { ToolType = Nut; Size = 3 }
            { ToolType = Bolt; Size = 5 }
            { ToolType = Bolt; Size = 3 }
            { ToolType = Bolt; Size = 3 }
            { ToolType = Nut; Size = 3 }
        ]

        let result = fitBoltsAndNuts bag

        for (bolt, nut) in result do
            Assert.IsTrue(bolt.ToolType = ToolType.Bolt && nut.ToolType = ToolType.Nut && nut.Size = bolt.Size)

        Assert.AreEqual(bag.Length/2, result.Length)

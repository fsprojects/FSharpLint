module FSharpLint.Core.Tests.Rules.Conventions.FavourBasicControlFlow

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsFavourFavourBasicControlFlow() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FavourBasicControlFlow.rule)
    
    
    [<Test>]
    member this.MoreThanTwoClausesShouldNotProduceError() =
        this.Parse """
match foo with
| bar -> ()
| baz -> ()
| _ -> () """

        Assert.IsTrue this.NoErrorsExist


    [<Test>]
    member this.TwoClausesWithoutWildcardShouldNotProduceError() =
        this.Parse """
match foo with
| bar -> ()
| baz -> () """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.ThePresenceOfDUShouldNotProduceError() =
        this.Parse """
match foo with
| Bar baz -> ()
| _ -> () """

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.TwoClausesWithWildcardShouldProduceError() =
        this.Parse """
match foo with
| bar -> ()
| _ -> () """

        Assert.IsTrue this.ErrorsExist

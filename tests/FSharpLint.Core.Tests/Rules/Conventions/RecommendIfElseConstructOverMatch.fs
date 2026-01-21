module FSharpLint.Core.Tests.Rules.Conventions.RecommendIfElseConstructOverMatch

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsRecommendIfElseConstructOverMatch() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RecommendIfElseConstructOverMatch.rule)
    
    
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
    member this.ThePresenceOfWhenClauseShouldNotProduceError() =
        this.Parse """
match foo with
| bar when bar.Count = 0 -> ()
| _ -> ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.TwoClausesWithWildcardShouldProduceError() =
        this.Parse """
match foo with
| bar -> ()
| _ -> () """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.TwoClausesWithNullAndWildcardShouldProduceError() =
        this.Parse """
match foo with
| null -> ()
| _ -> () """

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.TwoClausesWithNullAndVariableShouldNotProduceError() =
        this.Parse """
match foo with
| null -> ()
| notNullFoo -> () """

        Assert.IsTrue this.NoErrorsExist

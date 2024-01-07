module FSharpLint.Core.Tests.Rules.Conventions.RaiseWithTooManyArguments

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsFailwithWithSingleArg() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FailwithWithSingleArgument.rule)

    [<Test>]
    member this.FailwithWithCorrectNumberOfArguments() =
        this.Parse """
module Program

failwith ""
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithWithExtraArgument() =
        this.Parse """
module Program

failwith "" ""
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithWithMultipleExtraArguments() =
        this.Parse """
module Program

failwith "" "" "" ""
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithWithExtraArgumentWithRightPipe() =
        this.Parse """
module Program

"" |> failwith ""
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

[<TestFixture>]
type TestConventionsRaiseWithSingleArg() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(RaiseWithSingleArgument.rule)

    [<Test>]
    member this.RaiseWithCorrectArguments() =
        this.Parse """
module Program

raise (System.ArgumentException("Divisor cannot be zero!"))
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.RaiseWithExtraArgument() =
        this.Parse """
module Program

raise (System.ArgumentException("Divisor cannot be zero!")) ""
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

[<TestFixture>]
type TestConventionsFailwithfWithArugmentMatchingFormatString() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(FailwithfWithArgumentsMatchingFormatString.rule)

    [<Test>]
    member this.FailwithfWithCorrectNumberOfArguments() =
        this.Parse """
module Program

failwithf "%d %s" 4 "dog"
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithfWithExtraArgument() =
        this.Parse """
module Program

failwithf "%d %s" 4 "dog" 5
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.FailwithfWithEscapedFormatAndWithExtraArgument() =
        this.Parse """
module Program

failwithf "%d %% %s" 4 "dog" 5
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

[<TestFixture>]
type TestConventionsNullArgWithSingleArg() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NullArgWithSingleArgument.rule)

    [<Test>]
    member this.NullArgWithCorrectNumberOfArguments() =
        this.Parse """
module Program

nullArg ""
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.NullArgWithExtraArgument() =
        this.Parse """
module Program

nullArg "" ""
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

[<TestFixture>]
type TestConventionsInvalidOpWithSingleArg() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InvalidOpWithSingleArgument.rule)

    [<Test>]
    member this.InvalidOpWithCorrectNumberOfArguments() =
        this.Parse """
module Program

invalidOp ""
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.InvalidOpWithExtraArgument() =
        this.Parse """
module Program

invalidOp "" ""
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

[<TestFixture>]
type TestConventionsInvalidArgWithTwoArguments() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InvalidArgWithTwoArguments.rule)

    [<Test>]
    member this.InvalidArgWithCorrectNumberOfArguments() =
        this.Parse """
module Program

invalidArg "month" "Expected value to be between 1 and 12"
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 0))

    [<Test>]
    member this.InvalidArgWithExtraArgument() =
        this.Parse """
module Program

invalidArg "month" "Expected value to be between 1 and 12" "some other arg"
"""

        Assert.IsTrue(this.ErrorExistsAt(4, 0))

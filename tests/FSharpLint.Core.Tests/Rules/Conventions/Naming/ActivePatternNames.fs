module FSharpLint.Core.Tests.Rules.Conventions.ActivePatternNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsActivePatternNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ActivePatternNames.rule config)

    [<Test>]
    member this.PatternFunctionValidActivePattern() =
        this.Parse """
module Program
let (|Even|Odd|) = function
| i when i % 2 = 0 -> Even
| _ -> Odd

match 4 with
| Even -> ()
| Odd -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PatternFunctionValidPartialActivePattern() =
        this.Parse """
module Program
let (|Even|_|) = function
| i when i % 2 = 0 -> Some(i)
| _ -> None"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ActivePatternContainsUnderscore() =
        this.Parse """
module program
let (|Ev_en|Odd|) input = if input % 2 = 0 then Ev_en else Odd

match 4 with
| Ev_en -> ()
| Odd -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.ActivePatternDoesNotContainUnderscore() =
        this.Parse """
module Program
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

match 4 with
| Even -> ()
| Odd -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PartialActivePatternContainsUnderscore() =
        this.Parse """
module program
let (|Ev_en|_|) input = if input % 2 = 0 then Some 4 else None

match 3 with
| Ev_en(x) -> ()
| dog -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 5))

    [<Test>]
    member this.PartialActivePatternDoesNotContainUnderscore() =
        this.Parse """
module Program
let (|Even|_|) input = if input % 2 = 0 then Some 5 else None

match 3 with
| Even(x) -> ()
| dog -> ()"""

        this.AssertNoWarnings()

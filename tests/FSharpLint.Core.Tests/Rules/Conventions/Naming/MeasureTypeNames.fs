module FSharpLint.Core.Tests.Rules.Conventions.MeasureTypeNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

let config =
    { NamingConfig.Naming = None
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsMeasureTypesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MeasureTypeNames.rule config)

    [<Test>]
    member this.``Unit of measure issues no casing naming warning.``() =
        this.Parse """
[<Measure>] type L

[<Measure>] type usGal
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``Unit of measure issues underscore naming warning.``() =
        this.Parse """
[<Measure>] type us_Gal
"""

        Assert.IsTrue(this.ErrorsExist)

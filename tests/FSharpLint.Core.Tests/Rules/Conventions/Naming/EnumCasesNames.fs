module FSharpLint.Core.Tests.Rules.Conventions.EnumCasesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsEnumCasesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(EnumCasesNames.rule config)

    [<Test>]
    member this.EnumCaseIsPascalCase() =
        this.Parse """
module Program

type MyEnum =
| EnumCase = 1
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.EnumCaseIsCamelCase() =
        this.Parse """
module Program

type MyEnum =
| enumCase = 1
"""

        Assert.IsTrue(this.ErrorExistsAt(5, 2))

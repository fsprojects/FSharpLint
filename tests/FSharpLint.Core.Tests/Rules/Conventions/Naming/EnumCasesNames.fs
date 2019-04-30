module FSharpLint.Core.Tests.Rules.Conventions.EnumCasesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = Some NamingCase.PascalCase
      underscores = Some NamingUnderscores.None
      prefix = None
      suffix = None }
[<TestFixture>]
type TestConventionsEnumCasesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(EnumCasesNames.rule config)

    [<Test>]
    member this.EnumCaseIsPascalCase() =
        this.Parse """
module Program
  type MyEnum =
    | EnumCase = 1"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.EnumCaseIsCamelCase() =
        this.Parse """
module Program
  type MyEnum =
    | enumCase = 1"""

        Assert.IsTrue(this.ErrorExistsAt(4, 6))

    [<Test>]
    member this.EnumCaseIsCamelCaseSuppressed() =
        this.Parse """
module Program
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "EnumCasesNames")>]
  type MyEnum =
    | enumCase = 1"""

        this.AssertNoWarnings()

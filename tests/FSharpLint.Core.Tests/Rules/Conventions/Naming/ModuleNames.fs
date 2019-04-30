module FSharpLint.Core.Tests.Rules.Conventions.ModuleNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = Some NamingCase.PascalCase
      underscores = Some NamingUnderscores.None
      prefix = None
      suffix = None }
[<TestFixture>]
type TestConventionsModuleNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ModuleNames.rule config)

    [<Test>]
    member this.ModuleNameIsPascalCase() =
        this.Parse """
module Program
  let main = ()"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.ModuleNameIsCamelCase() =
        this.Parse """
module program
  let main = ()"""

        Assert.IsTrue(this.ErrorExistsAt(2, 7))

    [<Test>]
    member this.ModuleNameIsCamelCaseSuppressed() =
        this.Parse """
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "ModuleNames")>]
module program
  let main = ()"""

        this.AssertNoWarnings()

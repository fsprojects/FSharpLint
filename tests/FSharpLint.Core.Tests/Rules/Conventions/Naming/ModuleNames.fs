module FSharpLint.Core.Tests.Rules.Conventions.ModuleNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsModuleNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ModuleNames.rule config)

    [<Test>]
    member this.ModuleNameIsPascalCase() =
        this.Parse """
module Program

let main = ()
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.ModuleNameIsCamelCase() =
        this.Parse """
module program

let main = ()
"""

        Assert.IsTrue(this.ErrorExistsAt(2, 7))

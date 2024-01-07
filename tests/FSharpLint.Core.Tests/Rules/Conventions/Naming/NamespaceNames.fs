module FSharpLint.Core.Tests.Rules.Conventions.NamespaceNames

// fsharplint:disable TupleIndentation

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsNamespaceNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NamespaceNames.rule config)

    [<Test>]
    member this.NamespaceIsPascalCase() =
        this.Parse """
namespace Matt.Dog.Cat
"""

        this.AssertNoWarnings()

    [<Test>]
    member this.NamespaceIsCamelCase() =
        this.Parse """
namespace matt.dog.cat
"""

        Assert.IsTrue(this.ErrorExistsAt(2, 10))

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/240
    [<Test>]
    member this.``Linter must not complain about naming of fsx file``() =
        this.Parse("""
type MyClass2() as this =
  member this.PrintMessage() = ()""", fileName = "3i-3.fsx")

        this.AssertNoWarnings()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/240
    [<Test>]
    member this.``Linter must not complain about naming of fsx file with long name``() =
        this.Parse("""
type MyClass2() as this =
  member this.PrintMessage() = ()""", fileName = "foo.3i-3.fsx")

        this.AssertNoWarnings()


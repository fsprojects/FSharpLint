module FSharpLint.Core.Tests.Rules.Conventions.InterfaceNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Core.Tests

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = Some "I"
      Suffix = None }
[<TestFixture>]
type TestConventionsInterfaceNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InterfaceNames.rule config)

    [<Test>]
    member this.InterfaceNameBeginsWithI() =
        this.Parse """
module Program

type IPrintable =
    abstract member Print : unit -> unit
"""

        this.AssertNoViolations()

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/100
    /// (static classes were thought to be interfaces)
    [<Test>]
    member this.StaticClassIsNotTreatedAsInterface() =
        this.Parse """
module Program

type Printable =
    static member Print() = ()
"""

        this.AssertNoViolations()

    /// Regression test for https://github.com/ionide/ionide-vscode-fsharp/issues/153
    /// (type aliases were thought to be interfaces)
    [<Test>]
    member this.TypeAliasIsNotTreatedAsInterface() =
        this.Parse """
module Program

type Matrix = int[,]
"""

        this.AssertNoViolations()

    [<Test>]
    member this.InterfaceNameDoesNotBeginWithI() =
        this.Parse """
module Program

type Printable =
    abstract member Print : unit -> unit
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 5))

    [<Test>]
    member this.AbstractClassNameDoesNotBeginWithI() =
        this.Parse """
module Program

[<AbstractClass>]
type Printable() =
    abstract member Print : unit -> unit
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Quick fix for prefixes adds missing prefix to identifier.``() =
        let source = """
module Program

type Printable =
    abstract member Print : unit -> unit
"""

        let expected = """
module Program

type IPrintable =
    abstract member Print : unit -> unit
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

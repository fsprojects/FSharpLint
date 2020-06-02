module FSharpLint.Core.Tests.Rules.Conventions.ExceptionNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = Some "Exception" }
[<TestFixture>]
type internal TestConventionsExceptionNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ExceptionNames.rule config)

    [<Test>]
    member this.ExceptionIsPascalCase() =
        this.Parse """
module program
exception MyError of string
"""

        let error = "Consider changing `MyError` to PascalCase."

        Assert.IsFalse(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionIsCamelCase() =
        this.Parse """
module program
exception myError of string
"""

        let error = "Consider changing `myError` to PascalCase."

        Assert.IsTrue(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionEndsWithException() =
        this.Parse """
module program
exception MyErrorException of string
"""

        let error = "Consider changing `MyErrorException` to be suffixed with 'Exception'."

        Assert.IsFalse(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.ExceptionDoesNotEndWithException() =
        this.Parse """
module Program
exception MyError of string
"""

        let error = "Consider changing `MyError` to be suffixed with `Exception`."

        Assert.IsTrue(this.ErrorWithMessageExistsAt(error, 3, 10))

    [<Test>]
    member this.``Quick fix for suffixes adds missing suffix to identifier.``() =
        let source = """
module Program
exception Foo of string
"""

        let expected = """
module Program
exception FooException of string
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

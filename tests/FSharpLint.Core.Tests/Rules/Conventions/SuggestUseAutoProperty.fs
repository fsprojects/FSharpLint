module FSharpLint.Core.Tests.Rules.Conventions.SuggestUseAutoProperty

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestSuggestUseAutoProperty() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(SuggestUseAutoProperty.rule)

    [<Test>]
    member this.``Should suggest usage of auto-property for property that only returns immutable value`` () =
        this.Parse """
type Foo(content: int) =
    member self.Content = content
"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.``Should suggest usage of auto-property for property that only returns immutable value (this self-identifier)`` () =
        this.Parse """
type Foo(content: int) =
    member this.Content = content
"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.``Should suggest usage of auto-property for property that only returns immutable value (__ self-identifier)`` () =
        this.Parse """
type Foo(content: int) =
    member _.Content = content
"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.``Should suggest usage of auto-property for property that only returns literal`` () =
        this.Parse """
type Foo() =
    member self.Content = 42
"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.``Shouldn't suggest usage of auto-property for property that returns mutable value``() =
        this.Parse """
type Foo(content: int) =
    let mutable mutableContent = content
    member self.Content = mutableContent
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Shouldn't suggest usage of auto-property for non-property member``() =
        this.Parse """
type Foo(content: int) =
    member self.Content() = content
"""

        Assert.IsTrue(this.NoErrorsExist)


    [<Test>]
    member this.``Should suggest usage of auto-property for for property that only returns list of immutable values``() =
        this.Parse """
type Foo(content: int) =
    member self.Content = [ 42 ]
"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.``Should suggest usage of auto-property for for property that only returns array of immutable values``() =
        this.Parse """
type Foo(content: int) =
    member self.Content = [| content; 42 |]
"""

        Assert.IsTrue(this.ErrorsExist)

    [<Test>]
    member this.``Should not suggest usage of auto-property for static property`` () =
        this.Parse """
type Foo() =
    static member Content = 42
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Quick fix for property that only returns immutable value`` () =
        let source = """
type Foo(content: int) =
    member self.Content = content
"""
    
        let expected = """
type Foo(content: int) =
    member val Content = content
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

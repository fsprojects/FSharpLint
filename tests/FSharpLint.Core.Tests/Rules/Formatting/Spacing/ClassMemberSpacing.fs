module FSharpLint.Core.Tests.Rules.Formatting.ClassMemberSpacing

open NUnit.Framework
open FSharpLint.Rules

[<TestFixture>]
type TestFormattingClassMemberSpacing() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ClassMemberSpacing.rule)

    [<Test>]
    member this.``Error for no space between class members``() =
        this.Parse """
module Program

type T = T of int with
    static member x = 1
    static member x = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))

    [<Test>]
    member this.``No error for correct spacing between class members``() =
        this.Parse """
module Program

type T = T of int with
    static member x = 1

    static member x = 2
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``No error for correct spacing between class members with comment``() =
        this.Parse """
module Program

type GenericOptions =
    { Props : IHTMLProp list
      Classes : string list }

    member this.AddModifiers(modifiers) =
        ()

    /// Conver to standard element
    member this.ToReactElement(el:IHTMLProp list -> ReactElement list -> ReactElement, ?children): ReactElement =
        ()

    /// Convert to self closing element
    member this.ToReactElement(el:IHTMLProp list -> ReactElement): ReactElement =
        ()
"""

        Assert.IsTrue(this.NoErrorsExist)

    [<Test>]
    member this.``Error for too much spacing between class members``() =
        this.Parse """
module Program

type T = T of int with
    static member x = 1



    static member x = 2
"""

        Assert.IsTrue(this.ErrorExistsAt(6, 0))


module FSharpLint.Core.Tests.Rules.Conventions.MemberNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = Some NamingCase.PascalCase
      underscores = Some NamingUnderscores.AllowPrefix
      prefix = None
      suffix = None }

[<TestFixture>]
type TestConventionsMemberNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MemberNames.rule config)

    [<Test>]
    member this.ClassMemberIsPascalCase() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.PrintMessage() = ()"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.ClassMemberIsCamelCase() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.printMessage() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 16))

    /// The new member (constructor) is not pascal case so check it does not post an error.
    [<Test>]
    member this.ConstructorDoesNotPostError() =
        this.Parse """
module Program
type MyClass(x) =
    new() = MyClass(0)"""
        
        this.AssertNoWarnings()



    [<Test>]
    member this.PropertyIsPascalCase() =
        this.Parse """
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX with get() = x and set xval = x <- xval"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.PropertyIsCamelCase() =
        this.Parse """
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.centerX with get() = x and set xval = x <- xval"""

        Assert.IsTrue(this.ErrorExistsAt(6, 16))

    [<Test>]
    member this.PropertyIsCamelCaseSuppressed() =
        this.Parse """
  [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "MemberNames")>]
  type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.centerX with get() = x and set xval = x <- xval"""
    
        this.AssertNoWarnings()

    [<Test>]
    member this.AbstractMemberNameIsPascalCase() =
        this.Parse """
module Program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member Rotate: float -> unit"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.AbstractMemberNameIsCamelCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member rotate: float -> unit"""

        Assert.IsTrue(this.ErrorExistsAt(5, 20))

    [<Test>]
    member this.AbstractPropertyNameIsPascalCase() =
        this.Parse """
module Program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract Area : float with get"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.AbstractPropertyNameIsCamelCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract area : float with get"""

        Assert.IsTrue(this.ErrorExistsAt(5, 13))

    [<Test>]
    member this.DefaultMemberIsPascalCase() =
        this.Parse """
module Program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member Rotate: float -> unit
    default this.Rotate(angle) = ()"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.DefaultMemberIsCamelCase() =
        this.Parse """
module program
  [<AbstractClass>]
  type Shape2D(x0 : float, y0 : float) =
    abstract member rotate: float -> unit
    default this.rotate(angle) = ()"""

        Assert.IsTrue(this.ErrorExistsAt(6, 17))
        
    [<Test>]
    member this.TypeExtensionMethodIsPascalCase() =
        this.Parse """
module Program

type MyClass() =
    member this.F() = 100

type MyClass with
    member this.Goat() = 200"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.TypeExtensionMethodIsCamelCase() =
        this.Parse """
module program

type MyClass() =
    member this.F() = 100

type MyClass with
    member this.goat() = 200"""

        Assert.IsTrue(this.ErrorExistsAt(8, 16))
       
    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/99
    /// (duplicated warning for underscore in identifier).
    [<Test>]
    member this.MemberWithUnderscoreDoesNotHaveDuplicateWarnings() =
        this.Parse """
module Program

type Cat() =
    member x.Pri_nt() = ()"""

        let numberOfErrors = this.ErrorsAt(5, 13) |> Seq.length

        Assert.AreEqual(1, numberOfErrors)
      
    [<Test>]
    member this.``When prefix of underscores is allowed expect no suggestions when the remaining member ident is PascalCase``() =
        this.Parse """
module Program

type Cat() =
    member x.__Print() = ()"""

        this.AssertNoWarnings()
        
    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/323
    [<Test>]
    member this.``Special op_ prefixed members do not cause errors`` () =
        let source = """
type X = X of int
with
    static member op_Explicit(X x) = x
    static member op_Implicit(X x) = x
"""

        this.Parse source
        this.AssertNoWarnings()
        
    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/327
    [<Test>]
    member this.``Memebers implementing interface should be ignored`` () =
        let source = """
type Foo() =
    interface IDisposable with 
        member x.dispose() = ()
    member x.Bar() = ()
"""

        this.Parse source
        this.AssertNoWarnings()

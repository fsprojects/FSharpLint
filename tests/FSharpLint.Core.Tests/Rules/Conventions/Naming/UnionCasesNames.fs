module FSharpLint.Core.Tests.Rules.Conventions.UnionCasesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = Some NamingCase.PascalCase
      underscores = Some NamingUnderscores.None
      prefix = None
      suffix = None }
[<TestFixture>]
type TestConventionsUnionCasesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnionCasesNames.rule config)

    [<Test>]
    member this.UnionCaseIsPascalCase() =
        this.Parse """
module Program
  type Union =
    | UnionCase"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.UnionCaseIsCamelCase() =
        this.Parse """
module Program
  type Union =
    | unionCase"""

        Assert.IsTrue(this.ErrorExistsAt(4, 6))
       
    [<Test>]
    member this.``Let DU deconstruction must not warn about DU name``() =
        this.Parse """
module Program

type Foo = Foo of bool

let Foo(foo) = Foo(true)"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.``Let parameter DU deconstruction must not warn about DU name``() =
        this.Parse """
module Program

type Foo = Foo of bool

let foo (Foo(v)) = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``For pattern DU deconstruction must not warn about DU name``() =
        this.Parse """
module Program

type Foo = Foo of bool

for Foo(foo) in [Foo(true)] do ()"""
        
        this.AssertNoWarnings()
        
     /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/97
    [<Test>]
    member this.``Deconstructing union case in func arg does not suggest to rename union case``() = 
        let source = """
module String10 =
    type T = private | String10 of string

    let create str = String10 str

    let value (String10 str) = str"""
 
        this.Parse source
        
        this.AssertNoWarnings()
              
    [<Test>]
    member this.UnionCaseWithoutValueDoesNotGenerateWarningWhenTypeCheckingInput() =
        this.Parse("""
module Program

type SingleCaseDUNoValues = | SingleCaseDUNoValues

let foo SingleCaseDUNoValues = ()""")

        this.AssertNoWarnings()
        
    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/188
    [<Test>]
    member this.``Single long ident union case in func arg does must not suggest rename``() = 
        let source = """
module Program
type WithCamel = YesCamel

let SomeCamel WithCamel.YesCamel = 12  """
 
        this.Parse source
        
        this.AssertNoWarnings()       

module FSharpLint.Core.Tests.Rules.Conventions.PublicValuesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.naming = None
      underscores = Some NamingUnderscores.AllowPrefix
      prefix = None
      suffix = None }
[<TestFixture>]
type TestConventionsPublicValuesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(PublicValuesNames.rule config)

    /// A public binding let binding identifier may be pascal case or upper case.
    [<Test>]
    member this.PublicTupleIsPascalCase() =
        this.Parse """
module Program

    let (Cat, _) = 1, 0"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.PublicTupleIsCamelCase() =
        this.Parse """
module Program
  let main =
    let (cat, _) = 1, 0"""
        
        this.AssertNoWarnings()

    /// A public binding let binding identifier may be pascal case or upper case.
    [<Test>]
    member this.PublicFunctionNameIsPascalCase() =
        this.Parse """
module Program
  let Main () = ()"""
        
        this.AssertNoWarnings()

    [<Test>]
    member this.PublicFunctionNameIsCamelCase() =
        this.Parse """
module Program
  let main () = ()"""
  
        this.AssertNoWarnings()

    [<Test>]
    member this.UnionCaseInBindingContainingValueDoesNotGenerateWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU myInt) = (SingleCaseDU 5)""")
        
        this.AssertNoWarnings() 

module FSharpLint.Core.Tests.Rules.Conventions.GenericTypesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.PascalCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsGenericTypesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(GenericTypesNames.rule config)

    [<Test>]
    member this.GenericTypeNameIsPascalCase() =
        this.Parse """
type Foo<'T> = Option<'T>
"""
        this.AssertNoWarnings()
        
    [<Test>]
    member this.``generic type name shouldn't be camelCase``() =
        this.Parse """
type Foo<'a> = Option<'a>
    """
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 9))

    [<Test>]
    member this.``generic type name shouldn't be camelCase (2 generic types)``() =
        this.Parse """
type Foo<'a, 'T> = Option<'a * 'T>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsAt(2, 9))
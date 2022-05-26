module FSharpLint.Core.Tests.Rules.Conventions.GenericTypesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.None
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsGenericTypesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(GenericTypesNames.rule config)

    [<Test>]
    member this.GenericTypeNameIsCamelCase() =
        this.Parse """
type Foo<'a> = Option<'a>
"""
        this.AssertNoWarnings()
        
    [<Test>]
    member this.``generic type name shouldn't be PascalCase``() =
        this.Parse """
type Foo<'T> = Option<'T>
    """
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names shouldn't be PascalCase (2 generic types)``() =
        this.Parse """
type Foo<'a, 'T> = Option<'a * 'T>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names shouldn't be PascalCase (2 generic types with different order)``() =
        this.Parse """
type Foo<'T, 'a> = Option<'T * 'a>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names are camelCase``() =
        this.Parse """
type Foo<'k, 'v> = Option<'k * 'v>
"""
        this.AssertNoWarnings()

    [<Test>]
    member this.``generic type names shouldn't be PascalCase (multiple generic types)``() =
        this.Parse """
type Foo<'a1, 'a2, 'a3, 'a4, 'a5, 'T, 'a6> = Option<'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'T * 'a6>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names shouldn't be PascalCase even for types in methods``() =
        this.Parse """
module PeerChannelEncryptorMonad =
    type PeerChannelEncryptorComputation<'a> =
        | PeerChannelEncryptorComputation of
            (PeerChannelEncryptor -> Result<'a * PeerChannelEncryptor, PeerError>)

    let runP pcec initialState =
        let (PeerChannelEncryptorComputation innerFn) = pcec
        innerFn initialState

    let returnP x =
        let innerFn state =
            Ok(x, state)

        PeerChannelEncryptorComputation innerFn

    let bindP
        (f: 'T1 -> PeerChannelEncryptorComputation<'T2>)
        (xT: PeerChannelEncryptorComputation<'T1>)
        : PeerChannelEncryptorComputation<'T2> =
        let innerFn state =
            runP xT state
            >>= fun (res, state2) ->
                    let h = runP (f res) state2
                    h

        PeerChannelEncryptorComputation innerFn
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 18)

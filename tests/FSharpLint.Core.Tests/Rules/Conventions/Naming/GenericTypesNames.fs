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
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names shouldn't be camelCase (2 generic types)``() =
        this.Parse """
type Foo<'a, 'T> = Option<'a * 'T>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names shouldn't be camelCase (2 generic types with different order)``() =
        this.Parse """
type Foo<'T, 'a> = Option<'T * 'a>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names are PascalCase``() =
        this.Parse """
type Foo<'K, 'V> = Option<'K * 'V>
"""
        this.AssertNoWarnings()

    [<Test>]
    member this.``generic type names shouldn't be camelCase (multiple generic types)``() =
        this.Parse """
type Foo<'T1, 'T2, 'T3, 'T4, 'T5, 'a, 'T6> = Option<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'a * 'T6>
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 2)

    [<Test>]
    member this.``generic type names shouldn't be camelCase even for types in methods``() =
        this.Parse """
module PeerChannelEncryptorMonad =
    type PeerChannelEncryptorComputation<'T> =
        | PeerChannelEncryptorComputation of
            (PeerChannelEncryptor -> Result<'T * PeerChannelEncryptor, PeerError>)

    let runP pcec initialState =
        let (PeerChannelEncryptorComputation innerFn) = pcec
        innerFn initialState

    let returnP x =
        let innerFn state =
            Ok(x, state)

        PeerChannelEncryptorComputation innerFn

    let bindP
        (f: 'a -> PeerChannelEncryptorComputation<'b>)
        (xT: PeerChannelEncryptorComputation<'a>)
        : PeerChannelEncryptorComputation<'b> =
        let innerFn state =
            runP xT state
            >>= fun (res, state2) ->
                    let h = runP (f res) state2
                    h

        PeerChannelEncryptorComputation innerFn
"""
        Assert.IsTrue(this.ErrorsExist)
        Assert.IsTrue(this.ErrorExistsOnLine 18)

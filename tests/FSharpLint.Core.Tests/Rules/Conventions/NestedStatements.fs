module FSharpLint.Core.Tests.Rules.Conventions.NestedStatements

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Rules.NestedStatements

[<TestFixture>]
type TestConventionsNestedStatements() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(NestedStatements.rule { Config.depth = 5 })
 
    [<Test>]
    member this.NestedTooDeep() = 
        this.Parse """
module Program

let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(9, 20)) 

    [<Test>]
    member this.``Repeated nested too deep complains for correct ranges``() = 
        this.Parse """
module Program

let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()"""
    
        Assert.IsTrue(this.ErrorExistsAt(9, 20)) 
        Assert.IsTrue(this.ErrorExistsAt(16, 20)) 

    [<Test>]
    member this.NestedTooDeepSuppressed() = 
        this.Parse """
module Program

[<System.Diagnostics.CodeAnalysis.SuppressMessage("NestedStatements", "*")>]
let dog =
    if true then
        if true then
            if true then
                if true then
                    if true then
                        if true then
                            ()
    ()"""

        Assert.IsFalse(this.ErrorExistsOnLine(10)) 

    [<Test>]
    member this.ElseIfsShouldNotCountAsNested() = 
        this.Parse """
module Program

let dog =
    if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else if true then
        ()
    else 
        ()"""

        Assert.IsFalse(this.ErrorExistsAt(13, 4)) 
        
    [<Test>]
    member this.LambdaWildcardArgumentsMustNotCountAsANestedStatement() = 
        this.Parse """
module Program

let dog = (fun _ _ _ _ _ _ _ _ -> ())"""

        Assert.IsFalse(this.ErrorExistsOnLine(4))

    [<Test>]
    member this.LambdaArgumentsMustNotCountAsANestedStatement() = 
        this.Parse """
module Program

let dog = (fun a b c d e f g h i j -> ())"""

        Assert.IsFalse(this.ErrorExistsOnLine(4))

    [<Test>]
    member this.NestedLambdasCountedCorrectly() = 
        this.Parse """
module Program

let dog = (fun x -> fun x -> fun x -> fun x -> fun x -> ())"""

        Assert.IsTrue(this.ErrorExistsAt(4, 47))

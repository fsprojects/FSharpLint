module FSharpLint.Core.Tests.Rules.Conventions.SourceLength

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Rules.Helper.SourceLength

let generateNewLines numNewLines numIndents =
    Array.mapi
        (fun index _ ->
            let indentationChars =
                if index = 0 then
                    String.Empty
                else
                    String.replicate numIndents " "
            sprintf "%sprintf System.String.Empty\n" indentationChars)
            (Array.create numNewLines String.Empty)
    |> String.concat String.Empty

let generateAbstractMembers numMembers numIndents =
    Array.init numMembers (fun index -> sprintf "abstract member Foo%i : unit -> unit\n" index)
    |> String.concat (String.replicate numIndents " ")

let FunctionLength = 70
[<TestFixture>]
type TestMaxLinesInFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInFunction.rule { Config.MaxLines = FunctionLength })

    [<Test>]
    member this.FunctionTooManyLines() =
        this.Parse(sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines FunctionLength 4))
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionNotTooManyLines() =
        this.Parse(sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines (FunctionLength - 4) 4))
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionTooManyLinesWithComment() =
        this.Parse(sprintf """
module Program

let dog x =
    // Foo
    // Bar
    // Buzz
    %s
    ()""" (generateNewLines (FunctionLength - 3) 4))
        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.FunctionTooManyLinesWithMultiLineComment() =
        this.Parse(sprintf """
module Program

let dog x =
    (*
    Foo
    Bar
    *)
    %s
    ()""" (generateNewLines (FunctionLength - 4) 4))
        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.FunctionTooManyLinesWithNestsedMultiLineComment() =
        this.Parse(sprintf """
module Program

let dog x =
    (*
    Foo (* baz *)
    let (*) = id
    Bar
    *)
    let (*) a b = a + b
    %s
    ()""" (generateNewLines (FunctionLength - 5) 4))
        Assert.IsFalse this.ErrorsExist

let LambdaFunctionLength = 5
[<TestFixture>]
type TestMaxLinesInLambdaFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInLambdaFunction.rule { Config.MaxLines = LambdaFunctionLength })

    [<Test>]
    member this.LambdaFunctionTooManyLines() =
        this.Parse(sprintf """
module Program

let dog = fun x ->
    match x with
    | Some(x) ->
        %s
        ()
    | None -> ()
        """ (generateNewLines LambdaFunctionLength 8))
        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.``Multiple arguments in a lamba should not be treated as separate lambdas.``() =
        this.Parse(sprintf """
module Program

let dog = fun x y ->
    match x with
    | Some(x) ->
        %s
        ()
    | None -> ()
        """ (generateNewLines LambdaFunctionLength 8))

        Assert.AreEqual(1, Seq.length <| this.ErrorsAt(4, 10))

    [<Test>]
    member this.LambdaFunctionNotTooManyLines() =
        this.Parse """
module Program

let dog = fun x ->
    match x with
    | Some(x) ->
        ()
    | None -> ()
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 10))

let MatchLambdaFunctionLength = 70
[<TestFixture>]
type TestMaxLinesInMatchLambdaFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInMatchLambdaFunction.rule { Config.MaxLines = MatchLambdaFunctionLength })

    [<Test>]
    member this.MatchFunctionTooManyLines() =
        this.Parse(sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines MatchLambdaFunctionLength 4))
        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.MatchFunctionNotTooManyLines() =
        this.Parse(sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines (MatchLambdaFunctionLength - 5) 4))
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

let ValueLength = 70
[<TestFixture>]
type TestMaxLinesInValue() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInValue.rule { Config.MaxLines = ValueLength })

    [<Test>]
    member this.ValueTooManyLines() =
        this.Parse(sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines ValueLength 4))
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueNotTooManyLines() =
        this.Parse(sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines (ValueLength - 4) 4))
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

let ConstructorLength = 70
[<TestFixture>]
type TestMaxLinesInConstructor() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInConstructor.rule { Config.MaxLines = ConstructorLength })

    [<Test>]
    member this.ConstructorTooManyLines() =
        this.Parse(sprintf """
module Program

type MyClass(x) =
    new() =
        %s
        MyClass(0)
      """ (generateNewLines ConstructorLength 8))
        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.ConstructorNotTooManyLines() =
        this.Parse """
module Program

type MyClass(x) =
    new() = MyClass(0)
"""

        Assert.IsFalse(this.ErrorExistsAt(5, 4))

let MemberLength = 70
[<TestFixture>]
type TestMaxLinesInMember() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInMember.rule { Config.MaxLines = MemberLength })
    // TODO: Add tests.

let PropertyLength = 70
[<TestFixture>]
type TestMaxLinesInProperty() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInProperty.rule { Config.MaxLines = PropertyLength })

    [<Test>]
    member this.PropertyNotTooManyLines() =
        this.Parse """
module Program

type Class() =
    let mutable value = 10
    member this.Property1 with get() =
        value
"""
        Assert.IsFalse(this.ErrorExistsAt(6, 31))

let ClassLength = 500
[<TestFixture>]
type TestMaxLinesInClass() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInClass.rule { Config.MaxLines = ClassLength })

    [<Test>]
    member this.ClassTooManyLines() =
        this.Parse(sprintf """
module Program

type MyClass2() as this =
    do
        %s
    member this.PrintMessage() = ()""" (generateNewLines ClassLength 8))
        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.ClassNotTooManyLines() =
        this.Parse """
module Program

type MyClass2() as this =
    member this.PrintMessage() = ()
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.InterfaceTooManyLines() =
        this.Parse(sprintf """
module Program

type IPrintable =
    %s
    abstract member Print : unit -> unit""" (generateAbstractMembers ClassLength 4))

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.InterfaceNotTooManyLines() =
        this.Parse """
module Program

type IPrintable =
    abstract member Print : unit -> unit
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 5))

let UnionLength = 500
[<TestFixture>]
type TestMaxLinesInUnion() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInUnion.rule { Config.MaxLines = UnionLength })
    // TODO: Add tests.

let RecordLength = 500
[<TestFixture>]
type TestMaxLinesInRecord() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInRecord.rule { Config.MaxLines = RecordLength })

    [<Test>]
    member this.RecordTooManyLines() =
        this.Parse(sprintf """
module Program

type Record =
    {
        %s
        dog: int
    }""" (generateNewLines RecordLength 8))
        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.RecordNotTooManyLines() =
        this.Parse """
module Program

type Record = { dog: int }
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 5))

let EnumLength = 1000
[<TestFixture>]
type TestMaxLinesInEnum() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInEnum.rule { Config.MaxLines = EnumLength })
    // TODO: Add tests.

let ModuleLength = 1000
[<TestFixture>]
type TestMaxLinesInModule() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInModule.rule { Config.MaxLines = ModuleLength })

    [<Test>]
    member this.ModuleTooManyLines() =
        this.Parse(sprintf """
module Program
%s
let foo = ""
exception SomeException of string""" (generateNewLines ModuleLength 0))
        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.ModuleNotTooManyLines() =
        this.Parse(sprintf """
module Program
%s
let foo = ""
exception SomeException of string""" (generateNewLines (ModuleLength - 4) 0))
        Assert.IsFalse(this.ErrorExistsAt(2, 0))

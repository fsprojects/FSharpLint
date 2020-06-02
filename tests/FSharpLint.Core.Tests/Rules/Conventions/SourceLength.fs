module FSharpLint.Core.Tests.Rules.Conventions.SourceLength

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Rules.Helper.SourceLength

let generateNewLines numNewLines = Array.create numNewLines "\n" |> String.concat ""

let FunctionLength = 70
[<TestFixture>]
type internal TestMaxLinesInFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInFunction.rule { Config.MaxLines = FunctionLength })

    [<Test>]
    member this.FunctionTooManyLines() =
        this.Parse(sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines FunctionLength))
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionNotTooManyLines() =
        this.Parse(sprintf """
module Program

let dog x =
    %s
    ()""" (generateNewLines (FunctionLength - 4)))
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

let LambdaFunctionLength = 5
[<TestFixture>]
type internal TestMaxLinesInLambdaFunction() =
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
        """ (generateNewLines LambdaFunctionLength))
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
        """ (generateNewLines LambdaFunctionLength))

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
type internal TestMaxLinesInMatchLambdaFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInMatchLambdaFunction.rule { Config.MaxLines = MatchLambdaFunctionLength })

    [<Test>]
    member this.MatchFunctionTooManyLines() =
        this.Parse(sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines MatchLambdaFunctionLength))
        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.MatchFunctionNotTooManyLines() =
        this.Parse(sprintf """
module Program

let dog = function
| Some(x) ->
    %s
    ()
| None -> ()""" (generateNewLines (MatchLambdaFunctionLength - 5)))
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

let ValueLength = 70
[<TestFixture>]
type internal TestMaxLinesInValue() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInValue.rule { Config.MaxLines = ValueLength })

    [<Test>]
    member this.ValueTooManyLines() =
        this.Parse(sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines ValueLength))
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueNotTooManyLines() =
        this.Parse(sprintf """
module Program

let dog =
    %s
    ()""" (generateNewLines (ValueLength - 4)))
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

let ConstructorLength = 70
[<TestFixture>]
type internal TestMaxLinesInConstructor() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInConstructor.rule { Config.MaxLines = ConstructorLength })

    [<Test>]
    member this.ConstructorTooManyLines() =
        this.Parse(sprintf """
module Program
type MyClass(x) =
    new() =
      %s
      MyClass(0)
      """ (generateNewLines ConstructorLength))
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ConstructorNotTooManyLines() =
        this.Parse """
module Program
type MyClass(x) =
    new() = MyClass(0)"""
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

let MemberLength = 70
[<TestFixture>]
type internal TestMaxLinesInMember() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInMember.rule { Config.MaxLines = MemberLength })
    // TODO: Add tests.

let PropertyLength = 70
[<TestFixture>]
type internal TestMaxLinesInProperty() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInProperty.rule { Config.MaxLines = PropertyLength })

    [<Test>]
    member this.PropertyNotTooManyLines() =
        this.Parse """
module Program
  type Class() =
    let mutable value = 10
    member this.Property1 with get() =
        value"""
        Assert.IsFalse(this.ErrorExistsAt(5, 31))

let ClassLength = 500
[<TestFixture>]
type internal TestMaxLinesInClass() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInClass.rule { Config.MaxLines = ClassLength })

    [<Test>]
    member this.ClassTooManyLines() =
        this.Parse(sprintf """
module Program
  type MyClass2() as this =
    %s
    member this.PrintMessage() = ()""" (generateNewLines ClassLength))
        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.ClassNotTooManyLines() =
        this.Parse """
module Program
  type MyClass2() as this =
    member this.PrintMessage() = ()"""
        Assert.IsFalse(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.InterfaceTooManyLines() =
        this.Parse(sprintf """
module Program
  type IPrintable =
    %s
    abstract member Print : unit -> unit""" (generateNewLines ClassLength))
        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.InterfaceNotTooManyLines() =
        this.Parse """
module Program
  type IPrintable =
    abstract member Print : unit -> unit"""
        Assert.IsFalse(this.ErrorExistsAt(3, 7))

let UnionLength = 500
[<TestFixture>]
type internal TestMaxLinesInUnion() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInUnion.rule { Config.MaxLines = UnionLength })
    // TODO: Add tests.

let RecordLength = 500
[<TestFixture>]
type internal TestMaxLinesInRecord() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInRecord.rule { Config.MaxLines = RecordLength })

    [<Test>]
    member this.RecordTooManyLines() =
        this.Parse(sprintf """
module Program
  type Record =
    {
      %s
      dog: int
    }""" (generateNewLines RecordLength))
        Assert.IsTrue(this.ErrorExistsAt(3, 7))

    [<Test>]
    member this.RecordNotTooManyLines() =
        this.Parse """
module Program
  type Record = { dog: int }"""
        Assert.IsFalse(this.ErrorExistsAt(3, 7))

let EnumLength = 1000
[<TestFixture>]
type internal TestMaxLinesInEnum() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInEnum.rule { Config.MaxLines = EnumLength })
    // TODO: Add tests.

let ModuleLength = 1000
[<TestFixture>]
type internal TestMaxLinesInModule() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInModule.rule { Config.MaxLines = ModuleLength })

    [<Test>]
    member this.ModuleTooManyLines() =
        this.Parse(sprintf """
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines ModuleLength))
        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.ModuleNotTooManyLines() =
        this.Parse(sprintf """
module Program
%s
// Some exception.
exception SomeException of string""" (generateNewLines (ModuleLength - 4)))
        Assert.IsFalse(this.ErrorExistsAt(2, 0))

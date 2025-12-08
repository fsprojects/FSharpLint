module FSharpLint.Core.Tests.Rules.Conventions.FunctionReimplementation

open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Core.Tests

[<TestFixture>]
type TestConventionsFunctionReimplementation() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(ReimplementsFunction.rule)

    [<Test>]
    member this.LambdaReimplementingMultiplicationIssuesViolation() =
        this.Parse """
module Program

let f = fun a b -> a * b
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.``Operator ident is displayed in violation as an operator symbol.``() =
        this.Parse """
module Program

let f = fun a b -> a * b
"""

        this.ViolationMsg.Contains "`( * )`" |> Assert.IsTrue

    /// Regression test for https://github.com/fsprojects/FSharpLint/issues/237
    [<Test>]
    member this.``When there's a tuple pattern in the lambda eta contraction should not be recommended.``() =
        this.Parse """
module Program

let f = List.map (fun (x,_) -> id x) []
"""

        this.AssertNoViolations()

    [<Test>]
    member this.``Quickfix for lambda reimplementing operator is to replace the lambda with the operator.``() =
        let source = """
module Program

let f = fun a b -> a * b
"""

        let expected = """
module Program

let f = ( * )
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.``Lambda reimplementing long identifier function issues violation``() =
        this.Parse """
module Program

let f = fun a b -> List.map a b
"""

        Assert.IsTrue(this.ViolationsExist)

    [<Test>]
    member this.``Quickfix for lambda reimplementing function is to replace the lambda with the func ident.``() =
        let source = """
module Program

let f = fun a b -> List.map a b
"""

        let expected = """
module Program

let f = List.map
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to DU constructor application should be suggested``() =
        this.Parse("""
module Program

type Cat = | Meower of string

let f = List.map (fun x -> Meower x) ["1";"2"]""")

        Assert.IsTrue(this.ViolationsExist)

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/113
    [<Test>]
    member this.``Lambda to pointfree constructor application should be suggested if using F# 4 or above``() =
        this.Parse("""
module Program

type Duck(info:string) =
    do ()

let f = List.map (fun x -> Duck x) ["1";"2"]

open System
let f = List.map (fun x -> String x) ["1";"2"]""")

        Assert.IsTrue(this.ViolationsExist)

    [<Test>]
    member this.LambdaNotReimplmentingMultiplicationAsUsingConstantDoesNotIssueViolation() =
        this.Parse """
module Program

let f = fun a b -> a * 1
"""

        this.AssertNoViolations()

    [<Test>]
    member this.LambdaReimplementingCeilingFunctionIssuesViolation() =
        this.Parse """
module Program

let f = fun x -> ceil x
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.MultiplicationLambdaWithWildcardParameterDoesNotIssueViolation() =
        this.Parse """
module Program

let x = 6

let f = fun a b _ -> a * b
"""

        this.AssertNoViolations()

    [<Test>]
    member this.LambdaWithUnitParameterDoesNotIssueViolation() =
        this.Parse """
module Program

let x = 6

let f = fun () -> ceil x
"""

        this.AssertNoViolations()

    [<Test>]
    member this.LambdaWithWildcardParameterDoesNotIssueViolation() =
        this.Parse """
module Program

let x = 6

let f = fun _ -> ceil x
"""

        this.AssertNoViolations()

[<TestFixture>]
type TestConventionsCanBeReplacedWithComposition() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(CanBeReplacedWithComposition.rule)

    [<Test>]
    member this.LambdaNestedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesViolation() =
        this.Parse """
module Program

let f = fun x -> tan(cos(tan x))
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.LambdaPipedFunctionCallsThatCouldBeReplacedWithFunctionCompositionIssuesViolation() =
        this.Parse """
module Program

let f = fun x -> x |> tan |> cos |> tan
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.``Applying constants in chain issues composition suggestion``() =
        this.Parse """
module Program

let f = fun x -> x |> tan 0 |> cos |> tan
"""

        Assert.IsTrue(this.ViolationExistsAt(4, 8))

    [<Test>]
    member this.``Test quick fix for nested function calls that can be replaced with composition``() =
        let source = """
module Program

let f = fun x -> sin(cos(tan x))
"""
        
        let expected = """
module Program

let f = tan >> cos >> sin
"""
        
        this.Parse source

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.``Test quick fix for piped function calls that can be replaced with composition``() =
        let source = """
module Program

let f = fun x -> x |> tan |> cos |> sin
"""
        
        let expected = """
module Program

let f = tan >> cos >> sin
"""
        
        this.Parse source

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.``Test quick fix for piped function calls with partially applied functions that can be replaced with composition``() =
        let source = """
module Program

let f = fun x -> x |> min 0.0 |> cos |> tan
"""
        
        let expected = """
module Program

let f = min 0.0 >> cos >> tan
"""
        
        this.Parse source

        let result = this.ApplyQuickFix source

        Assert.AreEqual(expected, result)

    [<Test>]
    member this.``Applying closed over identifier in chain will not issue composition suggestion``() =
        this.Parse """
module Program

let f = fun x -> x |> tan y |> cos |> tan
"""

        this.AssertNoViolations()

    [<Test>]
    member this.LambdaWithUnitParameterDoesNotIssueViolation() =
        this.Parse """
module Program

let x = 6

let f = fun () -> ceil x
"""

        this.AssertNoViolations()

    [<Test>]
    member this.LambdaWithWildcardParameterDoesNotIssueViolation() =
        this.Parse """
module Program

let x = 6

let f = fun _ -> ceil x
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/130
    [<Test>]
    member this.``No suggestion should be given for function composition when the lambda's parameter's property/field is accessed``() =
        this.Parse """
module Program

let x = 6

let f = fun p -> p.Name <= packageName || not (isPackageLastInSource p)
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/140
    [<Test>]
    member this.``No suggestion should be given for function composition when an infix operator is in the expression``() =
        this.Parse """
module Program

let x = 6

let f = (fun value -> state + (findCoefficient conversion.Coefficients key) * value)
"""

        this.AssertNoViolations()

    /// Regression test for: https://github.com/fsprojects/FSharpLint/issues/140
    [<Test>]
    member this.``No suggestion should be given for function composition when lambda has multiple arguments``() =
        this.Parse """
module Program

let x = 6

let f = fun s1 s2 -> concat s1 s2 |> parse
"""

        this.AssertNoViolations()